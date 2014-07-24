library(shiny)
library(tseries, zoo)
library(ggplot2)
library(caret)
symbol <- Cl(read.zoo("BAC.csv", header = TRUE, sep = ",", tz = ""))
regressor <- Cl(read.zoo("C.csv", header = TRUE, sep = ",", tz = ""))
stocks.xts <- merge(symbol, regressor, all=FALSE)
colnames(stocks.xts) <- c("symbol","regressor")
stocks <- as.data.frame(stocks.xts)
stocks$index <- 1:length(stocks$symbol)

#stocks <- melt(stocks, id = "index")

shinyServer(function(input, output) {
  
  formulaText <- reactive({
    paste("Model:", input$model, "Observations:", length(stocks$symbol), "Train:", input$trainInput, "Test:",  length(stocks$symbol) - input$trainInput)
  })
  
  output$caption <- renderText({
    formulaText()
  })


  mod <- reactive({   
    trainSet <- 1:input$trainInput
    testSet <- (input$trainInput + 1):length(stocks$symbol)
    x <- stocks$regressor[trainSet]
    y <- stocks$symbol[trainSet]
    if(input$constant == FALSE){
      formula <- as.formula(y ~ 0 + x)
    }else if(input$constant == TRUE){
      formula <- (y ~ x)
    }
    if(input$model != "lm vs rlm"){
      FUN <- match.fun(input$model)   
      lm_rlm <- FUN(formula)
      if(input$constant == TRUE){ 
        modTest <- stocks$symbol[testSet] - coef(lm_rlm)[[1]] - coef(lm_rlm)[[2]]* stocks$regressor[testSet] 
        } else {
          modTest <- stocks$symbol[testSet] - coef(lm_rlm)[[1]] * stocks$regressor[testSet]
        }
      df <- data.frame(index = 1:length(stocks$symbol), lm = rep(NA,length(stocks$symbol)), modTest = rep(NA,length(stocks$symbol)))
      df$lm[trainSet] <- resid(lm_rlm)
      df$modTest[testSet] <- modTest
      melt(df, id = "index", na.rm = TRUE)
      } else {
        if(input$constant == TRUE){ 
          lmTrain <- lm(formula)
          rlmTrain <- rlm(formula)
          lmTest <- stocks$symbol[testSet] - coef(lmTrain)[[1]] - coef(lmTrain)[[2]]* stocks$regressor[testSet] 
          rlmTest <- stocks$symbol[testSet] - coef(rlmTrain)[[1]] - coef(rlmTrain)[[2]]* stocks$regressor[testSet] 
          } else {
            lmTrain <- lm(formula)
            rlmTrain <- rlm(formula)
            lmTest <- stocks$symbol[testSet] - coef(lmTrain)[[1]] * stocks$regressor[testSet]
            rlmTest <- stocks$symbol[testSet] - coef(rlmTrain)[[1]] * stocks$regressor[testSet]      
          }
        df <- data.frame(index = 1:length(stocks$symbol), 
                         lmTrain = rep(NA,length(stocks$symbol)), 
                         rlmTrain = rep(NA,length(stocks$symbol)), 
                         lmTest = rep(NA,length(stocks$symbol)),
                         rlmTest = rep(NA,length(stocks$symbol)))
        df$lmTrain[trainSet] <- resid(lmTrain)
        df$rlmTrain[trainSet] <- resid(rlmTrain)
        df$lmTest[testSet] <- lmTest
        df$rlmTest[testSet] <- rlmTest
        melt(df, id = "index", na.rm = TRUE)
      }
  })
  
  output$symbol <- renderPlot({
    print(ggplot(stocks , aes(x = index, y = symbol))+
            labs(title = "Symol", y = "Price")+
            geom_line(size = 1.3, col = "darkgreen"))
  })
  
  output$regressor <- renderPlot({
    print(ggplot(stocks , aes(x = index, y = regressor))+
            labs(title = "Regressor", y = "Price")+
            geom_line(size = 1.3, col = "darkgreen"))
  })
  
    
  output$lmvsrlm <- renderPlot({
    print(ggplot(mod() , aes(x = index, y = value, colour = variable))+
      geom_line(size = 1.3))
  })
  
#   vis <- reactive({
#     stocks %>%
#       ggvis(x = ~index, y = ~value, stroke = ~variable) %>% 
#       layer_lines(stroke := "blue")
#   })
#   vis %>% bind_shiny("ggvis", "ggvis_ui")
})
