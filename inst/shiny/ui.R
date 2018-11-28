library(shiny)

shinyUI(
  fluidPage(
    tags$style("#IsPkgInstalled {font-size:10em}"),
    titlePanel("Is CoreAPIV2 installed?"),
    mainPanel(
      textOutput(
        outputId = 'IsPkgInstalled'
      )
    )
  )
)
