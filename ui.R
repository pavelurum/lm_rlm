library(shiny)
stock <- Cl(read.zoo("CVX.csv", header = TRUE, sep = ",", tz = ""))

shinyUI(fluidPage(  
  titlePanel("lm vs rlm: XOM~CVX"), 
  
  sidebarLayout(
  sidebarPanel(
    selectInput("model", "Model:",
                list("lm" = "lm",
                     "rlm" = "rlm",
                     "lm vs rlm" = "lm vs rlm")),    
    checkboxInput("constant", "Constant", FALSE),
    numericInput("trainInput", "Number of train observations:", min = 200, value = 600, step = 100)
  ),
  
  mainPanel(
    h2(textOutput("caption")), 
    plotOutput("lmvsrlm", height="500px",width="900px"),
    plotOutput("symbol", height="300px",width="900px"),
    plotOutput("regressor", height="300px",width="900px")
    #uiOutput("ggvis_ui"),
    #ggvisOutput("ggvis")
    )
  )
))
