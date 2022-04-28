library(shiny)
library(tidyverse)
library(car)
library(extraDistr)
library(nnet)
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
# ui <- navbarPage("My Application",
#                  tabPanel("Component 1"),
#                  tabPanel("Component 2"),
#                  tabPanel("Component 3")
#)
ui <- fluidPage("Logistic Regression Tool",
  tabsetPanel(
             tabPanel("Binomial - Categorical Predictor",fluid=TRUE,
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
                           value = 50)),
             mainPanel(
               plotOutput("distPlot"),
               verbatimTextOutput("glmOut"),
               verbatimTextOutput("logita"),
               verbatimTextOutput("logitb")
    ))),
    tabPanel("Binomial - Continuous Predictor",fluid=TRUE,
             sidebarLayout(
             sidebarPanel(
               sliderInput("intercept",
                           "Intercept:",
                           min = 0,
                           max = 1,
                           value = 0,
                           step=0.01),
               sliderInput("slope",
                           "Slope",
                           min = 0,
                           max = 5,
                           value = 1,
                           step=.1)
             ),
             mainPanel(
                plotOutput("contPlot"),
                verbatimTextOutput("contOut"),
              )
  )),
  tabPanel("Multinomial - Categorical Predictor",fluid=TRUE,
           sidebarLayout(
             sidebarPanel(
               sliderInput("x1a",
                           "x1 A Counts:",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1),
               sliderInput("x1b",
                           "x1 B Counts:",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1),
               sliderInput("x1c",
                           "x1 C Counts:",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1),
               sliderInput("x2a",
                           "x2 A Counts",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1),
               sliderInput("x2b",
                           "x2 B Counts",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1),
               sliderInput("x2c",
                           "x2 C Counts",
                           min = 1,
                           max = 20,
                           value = 1,
                           step=1)
             ),
             mainPanel(
               plotOutput("multiCatPlot"),
               verbatimTextOutput("multiCatOut"),
               verbatimTextOutput("mcib"),
               verbatimTextOutput("mcic"),
               verbatimTextOutput("mcsb"),
               verbatimTextOutput("mcsc"),
             )
           ))
))
server <- function(input, output) {
  output$distPlot <- renderPlot({
    d<-getDat(input$x1T,input$x2T)
    ggplot(data=d,aes(y=y,x=type,color=as.factor(y)))+
      geom_point()+
      stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
                  method.args = list(family=binomial)) + theme_classic()
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
    print(paste("Slope:",log(bhit/bmiss)-log(ahit/amiss)))
  })
  output$contPlot <- renderPlot({
    slope<-input$slope
    intercept<-input$intercept
    #sval<-(1/slope)
    xSeq<-seq(from=0,to=1/slope-intercept,length.out=100)
    hits<-rbinom(xSeq,100,(xSeq*slope))+(intercept*100)
    hits[which(hits>100)]<-100
    tempDat<-NULL
    finDat<-NULL
    for(i in 1:length(xSeq)){
      tempDat<-data.frame(y=c(rep(1,hits[i]),rep(0,100-hits[i])),x=xSeq[i])
      finDat<-rbind(finDat,tempDat)
    }
    ggplot(data=finDat,aes(y=y,x=x,color=as.factor(y)))+
      geom_point()+
      stat_smooth(method="glm", se=FALSE, color="black", 
                  method.args = list(family=binomial)) + theme_classic()
    })  
  output$contOut <- renderPrint({
    slope<-input$slope
    intercept<-input$intercept
    #sval<-(1/slope)
    xSeq<-seq(from=0,to=1/slope-intercept,length.out=100)
    hits<-rbinom(xSeq,100,(xSeq*slope))+(intercept*100)
    hits[which(hits>100)]<-100
    tempDat<-NULL
    finDat<-NULL
    for(i in 1:length(xSeq)){
      tempDat<-data.frame(y=c(rep(1,hits[i]),rep(0,100-hits[i])),x=xSeq[i])
      finDat<-rbind(finDat,tempDat)
    }
    summary(glm(y~x,data=finDat))
  }) 
  output$multiCatOut <- renderPrint({

    at<-rep(1,input$x1a)
    bt<-rep(1,input$x1b)
    ct<-rep(1,input$x1c)
    a<-data.frame(type="a",val=at,v1=factor("a"))
    b<-data.frame(type="b",val=bt,v1=factor("a"))
    c<-data.frame(type="c",val=ct,v1=factor("a"))
    dat1<-rbind(a,b,c)
    
    at2<-rep(1,input$x2a)
    bt2<-rep(1,input$x2b)
    ct2<-rep(1,input$x2c)
    a2<-data.frame(type="a",val=at2,v1=factor("b"))
    b2<-data.frame(type="b",val=bt2,v1=factor("b"))
    c2<-data.frame(type="c",val=ct2,v1=factor("b"))
    dat2<-rbind(a2,b2,c2)
    
    dat<-rbind(dat1,dat2)
    summary(multinom(type~v1,data=dat))
  })
  output$mcib <- renderPrint({
    at<-rep(1,input$x1a)
    bt<-rep(1,input$x1b)
    ct<-rep(1,input$x1c)
    a<-data.frame(type="a",val=at,v1=factor("a"))
    b<-data.frame(type="b",val=bt,v1=factor("a"))
    c<-data.frame(type="c",val=ct,v1=factor("a"))
    dat1<-rbind(a,b,c)
    
    at2<-rep(1,input$x2a)
    bt2<-rep(1,input$x2b)
    ct2<-rep(1,input$x2c)
    a2<-data.frame(type="a",val=at2,v1=factor("b"))
    b2<-data.frame(type="b",val=bt2,v1=factor("b"))
    c2<-data.frame(type="c",val=ct2,v1=factor("b"))
    dat2<-rbind(a2,b2,c2)
    
    dat<-rbind(dat1,dat2)
    total<-sum(c(at,bt,ct,at2,bt2,bt2))
    print(paste("B Intercept:",
                log(input$x1b/total)-log(input$x1a/total)
                ))
  })
  output$mcic <- renderPrint({
    at<-rep(1,input$x1a)
    bt<-rep(1,input$x1b)
    ct<-rep(1,input$x1c)
    a<-data.frame(type="a",val=at,v1=factor("a"))
    b<-data.frame(type="b",val=bt,v1=factor("a"))
    c<-data.frame(type="c",val=ct,v1=factor("a"))
    dat1<-rbind(a,b,c)
    
    at2<-rep(1,input$x2a)
    bt2<-rep(1,input$x2b)
    ct2<-rep(1,input$x2c)
    a2<-data.frame(type="a",val=at2,v1=factor("b"))
    b2<-data.frame(type="b",val=bt2,v1=factor("b"))
    c2<-data.frame(type="c",val=ct2,v1=factor("b"))
    dat2<-rbind(a2,b2,c2)
    
    dat<-rbind(dat1,dat2)
    total<-sum(c(at,bt,ct,at2,bt2,bt2))
    print(paste("C Intercept:",
                log(input$x1c/total)-log(input$x1a/total)
                ))
  })
  output$mcsb <- renderPrint({
    at<-rep(1,input$x1a)
    bt<-rep(1,input$x1b)
    ct<-rep(1,input$x1c)
    a<-data.frame(type="a",val=at,v1=factor("a"))
    b<-data.frame(type="b",val=bt,v1=factor("a"))
    c<-data.frame(type="c",val=ct,v1=factor("a"))
    dat1<-rbind(a,b,c)
    
    at2<-rep(1,input$x2a)
    bt2<-rep(1,input$x2b)
    ct2<-rep(1,input$x2c)
    a2<-data.frame(type="a",val=at2,v1=factor("b"))
    b2<-data.frame(type="b",val=bt2,v1=factor("b"))
    c2<-data.frame(type="c",val=ct2,v1=factor("b"))
    dat2<-rbind(a2,b2,c2)
    
    dat<-rbind(dat1,dat2)
    total<-sum(c(at,bt,ct,at2,bt2,bt2))
    print(paste("B Slope:",
                (log(input$x2b/total)-log(input$x2a/total)) - (log(input$x1b/total)-log(input$x1a/total))
                ))
  })
  output$mcsc <- renderPrint({
    at<-rep(1,input$x1a)
    bt<-rep(1,input$x1b)
    ct<-rep(1,input$x1c)
    a<-data.frame(type="a",val=at,v1=factor("a"))
    b<-data.frame(type="b",val=bt,v1=factor("a"))
    c<-data.frame(type="c",val=ct,v1=factor("a"))
    dat1<-rbind(a,b,c)
    
    at2<-rep(1,input$x2a)
    bt2<-rep(1,input$x2b)
    ct2<-rep(1,input$x2c)
    a2<-data.frame(type="a",val=at2,v1=factor("b"))
    b2<-data.frame(type="b",val=bt2,v1=factor("b"))
    c2<-data.frame(type="c",val=ct2,v1=factor("b"))
    dat2<-rbind(a2,b2,c2)
    
    dat<-rbind(dat1,dat2)
    total<-sum(c(at,bt,ct,at2,bt2,bt2))
    print(paste("C Slope:",
                (log(input$x2c/total)-log(input$x2a/total)) - (log(input$x1c/total)-log(input$x1a/total))
    ))
  })
  output$multiCatPlot <- renderPlot({
    slope<-input$slope
    intercept<-input$intercept
    #sval<-(1/slope)
    xSeq<-seq(from=0,to=1/slope-intercept,length.out=100)
    hits<-rbinom(xSeq,100,(xSeq*slope))+(intercept*100)
    hits[which(hits>100)]<-100
    tempDat<-NULL
    finDat<-NULL
    for(i in 1:length(xSeq)){
      tempDat<-data.frame(y=c(rep(1,hits[i]),rep(0,100-hits[i])),x=xSeq[i])
      finDat<-rbind(finDat,tempDat)
    }
    ggplot(data=finDat,aes(y=y,x=x,color=as.factor(y)))+
      geom_point()+
      stat_smooth(method="glm", se=FALSE, color="black", 
                  method.args = list(family=binomial)) + theme_classic()
  })  

}
shinyApp(ui = ui, server = server)