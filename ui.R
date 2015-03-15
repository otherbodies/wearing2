
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
options(rgl.useNULL=TRUE)

library(shiny)
library(shinyRGL)
library(rgl)

shinyUI(fluidPage(

  # Application title
  titlePanel("Regression Lines"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "participant",
                  label = "Participant:",
                  choices = c("Co1","Co2","Co3","Co4","Co5","Co6","Co7","Co8","Co9","Co10",
"Sa1","Sa4","Sa5","Sa6","Sa7","Sa8","Sa9","Sa11","Sa13","Sa14","Sa20","Sa21","Sa22","Sa23","Sa24","Sa25","Sa26"),
                  selected = "Co1")
      ,
      selectInput(inputId = "condition",
                  label = "Condition:",
                  choices = c("str","hd","tr","eyes"),
                  selected = "str")
      ,
      selectInput(inputId = "task",
                  label = "Task:",
                  choices = c("month","crazy","mini"),
                  selected = "month")
      ,
      selectInput(inputId = "model",
                  label = "Model Fit:",
                  choices = c("null","plot 45deg shift","furthest points","cluster plot"),
                  selected = "null")
      ,
      selectInput(inputId = "plane",
                  label = "Plane:",
                  choices = c("zx plane","zy plane","2d data"),
                  selected = "zx plane")

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("textOut"),
      textOutput("textOut2"),
      textOutput("textOut3"),
      webGLOutput("myWebGL",width="800px",height="800px")
    )
  )
))
