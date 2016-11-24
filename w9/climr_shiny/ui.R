
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climate change analysis in R "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("data",
                  "Data Type",
                  choices = list("Global" = "GLB", "Northern Hemisphere" = "NH", "Southern Hemisphere" = "SH"),
                  selected ="GLB"),
      selectInput("smoother",
                  "Smoother Type",
                  choices = list("Linear Regression" = "lm", "Loess smooth" = "loess", "Spline smooth" = "smooth.spline"),
                  selected ="loess"),
      selectInput("scale",
                  "Scale Type",
                  choices = list("Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly"),
                  selected ="monthly"),
      sliderInput("grid",
                  "NUmber of time grid points", 
                  min = 10, 
                  max=200, 
                  value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
