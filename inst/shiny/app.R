library(shiny)

ui <- fluidPage(
  tags$style("#IsPkgInstalled {font-size:10em}"),
  titlePanel("Is pfsrsdk installed?"),
  mainPanel(
    textOutput(
      outputId = 'IsPkgInstalled'
    )
  )
)

server <- function(input, output){
  packageName <- "pfsrsdk"
  pkgs <- as.data.frame(installed.packages()[,c(1,3)])
  
  if(packageName %in% pkgs[["Package"]]) {
    output$IsPkgInstalled <- renderText({"YES!"})
  } else {
    output$IsPkgInstalled <- renderText({"No."})
  }
}

shinyApp(ui = ui, server = server)
