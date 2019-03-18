
outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("name") #, "used_shiny", "r_num_years")

saveData <- function(data) {
  # transpose data to wide format
  data <- t(data)
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.csv", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    data <- data.frame(
      name = character(),
      #used_shiny = logical(),
      #r_num_years = integer(),
      submit_time = as.Date(character())
    )
  } else {
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

deleteData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  lapply(files, file.remove)
}

#take in a list of artists?
library(shiny)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(fmsb)
# The function to acquire data from Spotify API
Artist_Server = function(names){
  artists <- get_artists(names)#5
  if(nrow(artists)>0){
    albums <- get_albums(artists$artist_uri[1])#3
    if(nrow(albums)>0){
      tracks <- get_album_tracks(albums)#2
      if(nrow(tracks)>0){
        radiohead_audio_features <- get_track_audio_features(tracks)#4
        if(nrow(radiohead_audio_features)>0){
          img_link = head(albums$album_img,3)
          N_Song = nrow(radiohead_audio_features)
          Song_Duration = paste0(format(min(albums$album_release_year),"%Y/%m"),"-",format(max(albums$album_release_year),"%Y/%m"))
          Tempo = paste(round(mean(radiohead_audio_features$tempo)),"BPM")
          
          p1 = radiohead_audio_features %>% 
            group_by(key_mode) %>% 
            summarise(N=n()) %>% 
            mutate(Pect = N/sum(N)) %>% 
            arrange(-N) %>% 
            head(5) %>%
            as.data.frame() %>%
            
            ggplot(aes(x=reorder(key_mode,Pect),y=Pect))+theme_light()+geom_bar(stat='identity',fill="#1db954",alpha=0.8)+coord_flip()+
            scale_y_continuous(labels=scales::percent)+xlab("Key Mode")+ylab("Percentage")+
            theme(axis.text = element_text(size=12),axis.title = element_text(size=10,face='italic'))
          radar = data.frame("acousticness"=c(0,1,mean(radiohead_audio_features$acousticness)),
                             "danceability"=c(0,1,mean(radiohead_audio_features$danceability)),
                             "energy"=c(0,1,mean(radiohead_audio_features$energy)),
                             "speechness"=c(0,1,mean(radiohead_audio_features$speechiness)),
                             "valence"=c(0,1,mean(radiohead_audio_features$valence)))
          return(list("Go",img_link,N_Song,Song_Duration,Tempo,p1,radar)) 
        } else {return(list("Nothing"))}
      } else {return(list("Nothing"))}
    } else {return(list("Nothing"))}
  } else {return(list("Nothing"))}
  
}

# Server
server <- function(input, output, session) {
  # Look up
  observeEvent(input$Action, {
    Element = Artist_Server(input$Artist)
    # Information
    output$Information <- renderUI({
      if(Element[[1]] == "Nothing"){
        h2("!!Cannot find the artist on Spotify!!",style="color:red")
      } else if(length(Element[[2]])>=3){
        div(style = "width=100%",
            img(src=Element[[2]][1],style='width:30%'),
            img(src=Element[[2]][2],style='width:30%'),
            img(src=Element[[2]][3],style='width:30%'),
            h3(paste("There are",Element[[3]],"songs in Spotify"),style="font-family:courier"),
            h3(paste0("Songs from ",Element[[4]]),style="font-family:courier"),
            h3(paste("Average Tempo",Element[[5]],"BPM"),style="font-family:courier"))
      } else {
        div(style = "width=100%",
            img(src=Element[[2]][1],style='width:45%'),
            h3(paste("There are",Element[[3]],"songs in Spotify"),style="font-family:courier"),
            h3(paste0("Songs from ",Element[[4]]),style="font-family:courier"),
            h3(paste("Average Tempo",Element[[5]],"BPM"),style="font-family:courier"))
      }
    })
    # Major distribution
    output$Major <- renderPlot({
      if(Element[[1]]=="Nothing"){
        NULL
      } else{
        Element[[6]]
      }
    })
    
    # Radar plot
    output$Radar <- renderPlot({
      if(Element[[1]]=="Nothing"){
        NULL
      } else{
        radarchart(Element[[7]],axistype=0,
                   pcol="#1db954",pfcol=rgb(0.2,0.7,0.5,0.8), plwd=2 ,
                   cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=2,
                   vlcex=1.5
        )
      }
    })
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    data <- sapply(fields, function(x) input[[x]])
    data$submit_time <- date()
    saveData(data)
  })
  
  # When the Delete button is clicked, delete all of the saved data files
  observeEvent(input$delete, {
    deleteData()
  })
  
  # Show the previous responses in a reactive table ----
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    input$delete
    
    # reset values
    updateTextInput(session, "name", value = "")
    #updateCheckboxInput(session, "used_shiny", value = FALSE)
    #updateSliderInput(session, "r_num_years", value = 0)
    
    loadData()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  responses_data <- reactive({
    input$Artist
    load_data(input$storage)
  })
  
  # Show the responses in a table
  output$responsesTable <- DT::renderDataTable(
    DT::datatable(
      responses_data(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
  
  
  
  
  
  
}
