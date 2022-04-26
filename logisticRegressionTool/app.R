#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
getDat <- function(x1t,x2t){
  x1tr<-rep(1,x1t)
  x1fa<-rep(0,100-x1t)
  x2tr<-rep(1,x2t)
  x2fa<-rep(0,100-x2t)
  x1<-data.frame(y=c(x1tr,x1fa),type="a")
  x2<-data.frame(y=c(x2tr,x2fa),type="b")
  x<-rbind(x1,x2)
  return(x)
}
ui <- fluidPage(
  titlePanel("Logistic Regression"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x1T",
                  "#X1 True:",
                  min = 0,
                  max = 100,
                  value = 50),
      sliderInput("x2T",
                  "#X2 True:",
                  min = 0,
                  max = 100,
                  value = 50)
    ),
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("glmOut")
    )
  )
)
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    d<-getDat(input$x1T,input$x2T)
    ggplot(data=d,aes(y=y,x=type,color=as.factor(y)))+
      geom_point()
  })
  output$glmOut <- renderPrint({
    d<-getDat(input$x1T,input$x2T)
    summary(glm(y~type,data=d,family="binomial"))
  })
}
shinyApp(ui = ui, server = server)
