ui <- fluidPage(
  #App layout has theme selector to avoid stagnant themes
  themeSelector(),
  h1("Genrify",style="font-family:Arial Black"),
  #br(),
    fluidRow(
      img(src="spotify.png",style="height:50px;margin:15px 150px"),
      textInput("Artist", h4("Search for a particular Artist",style="font-family:Comic Sans MS"), 
                value = "The Beatles",width="40%"),
      div(style="float:center",
        actionButton("Action","Search")),
      uiOutput("Information"),
      plotOutput("Major",height = "250px")
    ),
    fluidRow(
      div(style="border:1px solid grey",plotOutput("Radar",height = "500px",width = "500px"))#,
      #img(src="meaning.png",style="width:100%")
  )
)
