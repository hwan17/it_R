cars


carr <- c(1,3,6,4,9)
carr
plot(carr)

plot(carr,type="o")


carr <- c(1,3,6,4,9)
truckk <- c(2,5,4,5,12)
plot(carr,type="o",col="blue", ylim = c(0,12),axes=False,ann=False)
lines(truckk,type="o",pch=22,lty=2,col="red")
axis(1, at= 1:5, lab=c("mon","tue","wed","thur","fri"))
axis(2)
box()
g_range <- range(0, create_cnt$치킨집,drop_cnt$치킨집)
g_range
legend(8, g_range[2], c("창업","폐업"),cex=0.8,col=c("blue","red"),pch=21:22, lty=1:2)

range(0,6,4)
---------------------------
  
  
  
  create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 
g <- create_cnt$치킨집
m <- drop_cnt$치킨집
g_range <- range(0, g, m)
plot(g, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)
axis(1, at=1:10, lab=create_cnt$년도)
axis(2)
box()
lines(m, type="o", pch=22, lty=2, col="red")
title(main="치킨집 창업/폐업현황", col.main="red")
title(xlab="year" )
title(ylab="Total" )
legend( 8, g_range[2], c("창업","폐업"), cex=0.8, 
        col=c("blue","red"), pch=21:22, lty=1:2);

-------------------------------------
  
  
  
library(shiny)
library(datasets)

setwd("c:\\data") 
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("년도별 업종별 창업, 폐업 현황"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("typePlot")  
    )
    
  )
)

# Define server logic ----
server <-function(input, output) {
  
  # Fill in the spot we created for a plot
  output$typePlot <- renderPlot({
    
    
    # Render a barplot
    g <- create_cnt[,input$region]
    m <- drop_cnt[,input$region]
    
    g_range <- range(0, g, m)
    plot(g, type="o", col="blue", ylim=g_range, 
         axes=FALSE, ann=FALSE)
    axis(1, at=1:10, lab=create_cnt$년도)
    axis(2)
    box()
    lines(m, type="o", pch=22, lty=2, col="red")
    title(main=input$region, col.main="red")
    title(xlab="year" )
    title(ylab="Total" )
    legend( 8, g_range[2], c("창업","폐업"), cex=0.8, 
            col=c("blue","red"), pch=21:22, lty=1:2);
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
