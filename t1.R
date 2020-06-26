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
g_range <- range(0, create_cnt$ġŲ��,drop_cnt$ġŲ��)
g_range
legend(8, g_range[2], c("â��","���"),cex=0.8,col=c("blue","red"),pch=21:22, lty=1:2)

range(0,6,4)
---------------------------
  
  
  
  create_cnt <- read.csv("â���Ǽ�.csv", header=T)
drop_cnt <- read.csv("����Ǽ�.csv", header=T) 
g <- create_cnt$ġŲ��
m <- drop_cnt$ġŲ��
g_range <- range(0, g, m)
plot(g, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)
axis(1, at=1:10, lab=create_cnt$�⵵)
axis(2)
box()
lines(m, type="o", pch=22, lty=2, col="red")
title(main="ġŲ�� â��/�����Ȳ", col.main="red")
title(xlab="year" )
title(ylab="Total" )
legend( 8, g_range[2], c("â��","���"), cex=0.8, 
        col=c("blue","red"), pch=21:22, lty=1:2);

-------------------------------------
  
  
  
library(shiny)
library(datasets)

setwd("c:\\data") 
create_cnt <- read.csv("â���Ǽ�.csv", header=T)
drop_cnt <- read.csv("����Ǽ�.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("�⵵�� ������ â��, ��� ��Ȳ"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "����:", 
                  choices=colnames(create_cnt)[-1]),
      hr(),
      helpText("������ â���� ��� ��Ȳ ������")
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
    axis(1, at=1:10, lab=create_cnt$�⵵)
    axis(2)
    box()
    lines(m, type="o", pch=22, lty=2, col="red")
    title(main=input$region, col.main="red")
    title(xlab="year" )
    title(ylab="Total" )
    legend( 8, g_range[2], c("â��","���"), cex=0.8, 
            col=c("blue","red"), pch=21:22, lty=1:2);
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)