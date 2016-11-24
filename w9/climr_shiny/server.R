#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(climr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    data_climr = load_clim(input$data)
    fit_climr = fit(data_climr, data_type = input$scale, fit_type = input$smoother)
    
    plot(fit_climr, time_grid = pretty(fit_climr$data$x, n= inpur$grid))
    
  })
  
})
