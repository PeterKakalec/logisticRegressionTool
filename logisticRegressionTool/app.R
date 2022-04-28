library(shiny)
library(tidyverse)
library(car)
library(extraDistr)
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
      verbatimTextOutput("glmOut"),
      verbatimTextOutput("logita"),
      verbatimTextOutput("logitb")
    )
  )
)
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    d<-getDat(input$x1T,input$x2T)
    ggplot(data=d,aes(y=y,x=type,color=as.factor(y)))+
      geom_point()+
      theme_classic()
  })
  output$glmOut <- renderPrint({
    d<-getDat(input$x1T,input$x2T)
    summary(glm(y~type,data=d,family="binomial"))
  })    
  output$logita <- renderPrint({
    d<-getDat(input$x1T,input$x2T)
    dfa<-d %>% filter(type=="a")
    dfb<-d %>% filter(type=="b")
    ahit<-length(which(dfa$y==1))
    amiss<-length(which(dfa$y==0))
    print(paste("Intercept:",log(ahit/amiss)))
    #exp(log(ahit/amiss)) gives actual fraction of ahit/amiss
  })
  output$logitb <- renderPrint({
    d<-getDat(input$x1T,input$x2T)
    dfa<-d %>% filter(type=="a")
    dfb<-d %>% filter(type=="b")
    ahit<-length(which(dfa$y==1))
    amiss<-length(which(dfa$y==0))
    bhit<-length(which(dfb$y==1))
    bmiss<-length(which(dfb$y==0))
    diffhit<-ahit-bhit
    diffmiss<-amiss-bmiss
    print(paste("Slope:",log(bhit/bmiss)-log(ahit/amiss)))
  })
}
shinyApp(ui = ui, server = server)