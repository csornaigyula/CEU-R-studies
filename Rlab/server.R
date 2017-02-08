#this is not an arbitrary name, used by shiny, need to be in same folder
library(shiny)
library(ggplot2)

shinyServer(
  function(input, output){
    #because we have an output with ggplot ID on UI
    output$ggplot <- renderPlot({
      ggplot(mtcars,aes_string(x=input$var1, y=input$var2, col=input$cby))+
        geom_point(size=3)+
        geom_smooth(method='lm', 
                    formula = y~poly(x,input$poly), 
                    se=input$se, 
                    col='dodgerblue3' )
    })
    output$model <- renderPrint({
      fit <- lm(mtcars[,input$var1] ~ poly(mtcars[,input$var2],input$poly))
      summary(fit)
    })
  }
  
)