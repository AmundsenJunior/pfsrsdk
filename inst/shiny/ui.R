library(shiny)

shinyUI(
  fluidPage(
    tags$style("#IsPkgInstalled {font-size:10em}"),
    titlePanel("Is pfsrsdk installed?"),
    mainPanel(
      textOutput(
        outputId = 'IsPkgInstalled'
      )
    )
  )
)
