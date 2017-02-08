#this is not an arbitrary name, used by shiny
library(shiny)

shinyUI(
  fluidPage(
    titlePanel('The great mtcars analysis engine'),
    sidebarLayout(
      sidebarPanel(
        selectInput('var1', 'X variable',
                    names(mtcars), selected='wt'),
        selectInput('var2', 'Y variable',
                    names(mtcars), selected='hp'),
        sliderInput('poly',"Polynomial",min=1, max=6, value=1),
        checkboxInput('se',"Confidence interval to plot"),
        radioButtons('cby',"Color by: ",
                     c("Number of gears" = 'gear',
                       "Transmission" = 'am',
                       "Number of cylinders" = 'cyl'))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel('Plot',plotOutput('ggplot')),
          tabPanel('Model',verbatimTextOutput('model'))
        )
      )
    )
  )
)

?mtcars
