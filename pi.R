#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(ggplot2)
require(ggplot2)
require(ggforce)
require(devtools)
require(shinythemes)
norms_plot <- function(k,n){
    E<- c()
    for (i in 1:k) {
        x1<-rnorm(n,0,(pi^0.5))
        ests<-(sd(x1))^2
        E <-append(E,ests)
    }
    h<-density(E) 
    p<-plot(h)
    abline(v = mean(E),col="red")
    text(4.5, 0.7, "Estimate", col = "red")
    text(4.5, 0.75, "Pi", col = "green")
    abline(v = pi,col="green")
    rect(4,0.65, 5, 0.8)
    return (p)}

chudnovsky <- function(n){
    Csky <- numeric(n)
    options(digits=15)
    for (i in 0:n){
        numer<-((-1)^i)*factorial(6*i)*((54514013*i)+13591409)
        denom<-factorial(3*i)*(factorial(i)^3)*(640320)^((3*i)+(3/2))
        k <- numer/denom
        Csky<-append(Csky,k)
        
    }
    x<-12*sum(Csky)
    est<-1/x
    return(est)
}

co_prime<-function(n,upper,lower){
    co<-0
    for (i in 1:n){
        x1<-runif(1,lower,upper)
        x2<-runif(1,lower,upper)
        n1<-abs(round(x1))
        n2<-abs(round(x2))
        if (numbers::GCD(n1,n2) ==1){
            co <- co + 1
        }
        else{
            co <- co
        }
        x<-co/i
        e_pi <- (6/x)^0.5
    }
    return(e_pi)
}

newt_pi<-function(a,n){
    x0 <- a 
    for (i in 1:n) {
        xn <- x0
        xn1 <- xn - tan(xn)
        x0 <- xn1
    }
    return(xn1)
}

graph_unif<-function(n,upper,lower){
    graph_data <- c()
    co <- 0
    for (i in 1:n){
        x1<-runif(1,lower,upper)
        x2<-runif(1,lower,upper)
        n1<-round(x1)
        n2<-round(x2)
        if (numbers::GCD(n1,n2) ==1){
            co <- co + 1
        }
        else{
            co <- co
        }
        x<-co/i
        e_pi <- (6/x)^0.5
        graph_data<-append(graph_data,e_pi)}
    graph_data<-as.data.frame(graph_data)
    return(graph_data)
}


est<-function(N){
    lower<- 0 
    upper<- 1 
    
    x<-runif(N, lower, upper) 
    
    h.x<-function(x){   
        return((-6*pi/7)*(x^2 - 3*x))
    }
    
    
    plot(seq(lower,upper, by=.01), h.x(seq(lower,upper, by=.01)), type="l") 
    
    abline(h=mean(h.x(x)), col="blue") 
    
    mc.int<-(mean(h.x(x)) *(upper-lower))
    return(mc.int)
}
pl<-function(N){
    lower<- 0 
    upper<- 1 
    
    x<-runif(N, lower, upper) 
    
    h.x<-function(x){   
        return((-6*pi/7)*(x^2 - 3*x))
    }
    
    
    p<-plot(seq(lower,upper, by=.01), h.x(seq(lower,upper, by=.01)), type="l") 
    return(p)
}
s<-round(runif(1,0,1000))
monte<-function(n){
    c1<-0
    set.seed(s)
    for (i in 1:n) {
        c <- c1 
        x1<-runif(1,-1,1)
        y1<-runif(1,-1,1)
        d <- x1^2 + y1^2
        if(d <=1){
            c1 <- c +1
        }
        else{
            c1 <- c
        }
    }
    return(4*(c1/n))}

plots <- function(n){
    set.seed(s)
    x1<-runif(n,-1,1)
    y1<-runif(n,-1,1)
    dat<-data.frame(x = 0, y = 0)
    in.circle <- x1^2 + y1^2 <= 1^2
    points<-data.frame(cbind(x1,y1))
    g<-ggplot(data = points, aes(x=x1,y=y1))+
        geom_point(color=ifelse(in.circle,"orange", "black"))+
        ylab("y")+
        xlab("x")+
        stat_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE,color="blue")+
        coord_fixed()
    return(g)}
norms <- function(k,n){
    E<- c()
    for (i in 1:k) {
        x1<-rnorm(n,0,(pi^0.5))
        ests<-(sd(x1))^2
        E <-append(E,ests)
    }
    return (mean(E))}




invs<-function(n){
    square_inv <- numeric(n)
    for (i in 1:n){
        l <- 1/(i^2)
        square_inv<-append(square_inv,l)
    }
    x<-sum(square_inv)
    return((x*6)^0.5)
}


ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Estimating Pi"),
    tabsetPanel(
        # making a sidebar menu
        tabPanel("Newton's Method", 
                     tabName = "nm",  h2("Newton's Method", align = "center"),
                  numericInput("obs1", " Number of Observations:", 1000, min = 1, max = 10000000),
                            numericInput("st", " starting value :", 3, min = 1, max = 10000),
                            plotOutput("Plot1"),
                            verbatimTextOutput("val1"),
                     icon = icon("seedling")),
        tabPanel("Uniform Distribution", tabName = "unif",h2("Sampling from Uniform", align = "center"),
                 numericInput("obs2", " Number of Observations:", 1000, min = 1),
                 numericInput("lb", "Lower bound:",0,min=0),
                 numericInput("ub", "Upper bound:", 1000, min=0),
                 plotOutput("Plot2"),
                 verbatimTextOutput("val2"),
                     icon = icon("list-alt")),
        tabPanel("Normal Distribution ", tabName = "norm",
                 h2("Simulating from a Normal Distribution", align = "center"),
                 numericInput("n", "Number of observations:",100, min=1),
                 numericInput("k", "Number of simulations:", 1000, min=0),
                 plotOutput("Plot3"),
                 verbatimTextOutput("val3"), 
                     icon = icon("bar-chart-o")),
        tabPanel("Infinite series", 
                     tabName = "ca",
                 h2("Chudnovsky Algorithm", align = "center"),
                 numericInput("n2", "Number where we truncate:", 10, min=0, max = 25),
                 verbatimTextOutput("val4"),
                 h2("Sum of Square Inverse", align = "center"),
                 numericInput("n5", "Number where we truncate:", 1000, min=0),
                 verbatimTextOutput("val5"),
                     icon = icon("table")),
        tabPanel("Monte Carlo integration", 
                     tabName = "mcint",
                 h2("Monte Carlo integration", align = "center"),
                 numericInput("n4", "n:", 1000, min=0),
                 verbatimTextOutput("val6"),
                 plotOutput("Plot6"),
                     icon = icon("list-alt")),
        tabPanel("Circle", 
                     tabName = "mcint", 
                 sliderInput("n3", "Number of observations:", 3500, min=0, max = 15000),
                 plotOutput("Plot7"),
                 verbatimTextOutput("val7"),
                 plotOutput("Plot8"),
                     icon = icon("bar-chart-o"))
        )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    samp_plot<-function(N){
        plots <- c()
        for (i in 1:N){
            m<-monte(i)
            plots <- append(plots,m)
        }
        plots<-as.data.frame(plots)
        g<-ggplot(data=plots,aes(y=plots,x = 1:N))+
            geom_line(aes(y=plots,x = 1:N)) +
            geom_line(y=pi,aes(x = 1:N),color="red",inherit.aes = FALSE) +
            xlab("number of observations") +
            ylab("pi estimate")
        return(g)
    }
    samp_plot1<-function(N){
        plots <- c()
        for (i in 1:N){
            m<-est(i)
            plots <- append(plots,m)
        }
        plots<-as.data.frame(plots)
        g<-ggplot(data=plots,aes(y=plots,x = 1:input$n4))+
        geom_line(aes(y=plots,x = 1:N)) + 
            geom_line(y=pi, color="green")+
            xlab("n")+
            ylab("pi estimate")
        return(g)
    }
    options(digits=10)
    output$val1 <- renderText({ newt_pi(input$st,input$obs1)})
    output$val2 <- renderText({ co_prime(input$obs2,input$ub,input$lb)})
    # ggplot2
    output$Plot1 <- renderPlot({
        s<- 0
        e<- 100
        graph_data <- c()
        for (i in s:e) {
            val<-newt_pi(i,input$obs1) 
            graph_data<-append(graph_data,val)
        }
        graph_data<-as.data.frame(graph_data)
        ggplot(data=graph_data,aes(y=graph_data,x = s:e))+
            geom_point() +
            geom_line(y=pi, color="blue") +
            ylim(0,max(graph_data))+
            xlab("starting values") +
            ylab("pi estimate")
    })
    
    output$Plot2 <- renderPlot({
        dat<-graph_unif(input$obs2,input$ub,input$lb)
        ggplot(data=dat,aes(y=graph_data,x = 1:input$obs2))+
            geom_point() +
            geom_line(y=pi, color="green")+
            xlab("number of observations") +
            ylab("pi estimate")
            
    })
    output$val3 <- renderText({norms(input$k,input$n)})
  
    output$Plot3<-renderPlot({
    norms_plot(input$k,input$n)})
    

    output$val4<-renderText({chudnovsky(input$n2)})

    output$Plot7<-renderPlot({
        plots(input$n3)})
    output$val7<-renderText({
        monte(input$n3)})
    output$Plot8<-renderPlot({
        samp_plot(input$n3)})
    

    output$Plot6<-renderPlot({
       samp_plot1(input$n4)
        })
    output$val6<-renderText({
        est(input$n4)})
    
  
    output$val5<-renderText({
        invs(input$n5)})
    
    observeEvent(input$switchtab, {
        newtab <- switch(input$tabs, 
                         "Newton's Method" = "Uniform distribution", 
                         "Normal Distribution"="Newton's Method",
                         "Newton's Method" = "Chudnovsky Algorithm", 
                         "Sum of Inverse squares"="Newton's Method",
                         "Monte Carlo integration" = "Newton's Method",
                         "Monte Carlo method" = "Newton's Method",
                         "Monte Carlo method" = "Uniform Distribution", 
                         "Normal Distribution"="Monte Carlo method",
                         "Monte Carlo method" = "Chudnovsky Algorithm", 
                         "Sum of Inverse squares"="Monte Carlo method",
                         "Monte Carlo integration" = "Monte Carlo method",
                         "Normal Distribution"="Uniform Distribution",
                         "Uniform Distribution" = "Chudnovsky Algorithm", 
                         "Sum of Inverse squares"="Uniform Distribution",
                         "Monte Carlo integration" = "Uniform Distribution",
                         "Normal Distribution" = "Chudnovsky Algorithm", 
                         "Sum of Inverse squares"="Normal Distribution",
                         "Monte Carlo integration" = "Normal Distribution",
                         "Sum of Inverse squares"="Chudnovsky Algorithm",
                         "Monte Carlo integration" = "Chudnovsky Algorithm",
                         "Monte Carlo integration" = "Sum of Inverse squares")
        #update tab items 
        updateTabItems(session, "tabs", newtab)
    })   
}

# Run the application 
shinyApp(ui = ui, server = server)


