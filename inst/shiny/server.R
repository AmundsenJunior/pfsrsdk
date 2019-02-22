library(shiny)

shinyServer(function(input, output){
  packageName <- "pfsrsdk"
  pkgs <- as.data.frame(installed.packages()[,c(1,3)])
  
  if(packageName %in% pkgs[["Package"]]) {
    output$IsPkgInstalled <- renderText({"YES!"})
  } else {
    output$IsPkgInstalled <- renderText({"No."})
  }
})
