library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram

shinyUI(fluidPage(        
        tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
        ),
        
  
  # Application title
  titlePanel("Coursera Data Science Specialization"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      
      h5("Capstone Project Data Product"),
      h5("Submitted by SergiosK"),
      br(),
      h3("Please enter an incomplete sentence and check the prediction for the next word"),
      textInput(inputId = "userSentence", 
                label = "", 
                value = "" # 
      ),
      
     
      
      sliderInput("numPredicted", "Number of words to predict (max. 10):", 
                  min=1, max=10, value=4)
      
      
    ),  
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Application",
          h4("The predicted word (with highest probability) is:"),
          HTML("<span style='color:blue'>"),
          h3(textOutput("predictedWordMain"), align="center"),
          HTML("</span>"),
          br(),
          h4(textOutput("kText")),
          hr(),
          plotOutput("predictionTable")
     
         
          ),
        tabPanel("App documentation",
         includeMarkdown("app_documentation.Rmd")
          )
    )
  ) #mainPanel ends
)
))