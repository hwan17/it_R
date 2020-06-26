#install.packages("DT")
library(DT)
library(shiny)
library(ggplot2)
emp <- read.csv("c:\\data\\emp3.csv",header=T)
# Define UI ----
ui <- fluidPage(
  titlePanel("EMP DataTable"),
  # Create a new Row in the UI for selectInputs
  # Create a new row for the table.
  DT::dataTableOutput("table")
)
# Define server logic ----
server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- emp
  }))
}
# Run the app ----
shinyApp(ui = ui, server = server)

barplot(emp$sal)

barplot(emp$sal, main="Salary Bar Chart")

barplot(emp$sal, main="Salary Bar Chart", names.arg=emp$ename)

barplot(emp$sal, main="Salary Bar Chart", names.arg=emp$ename, ylab = "Salary", xlab = "name")

cc <- read.csv("창업건수.csv", header = T)
cc

barplot(cc$치킨집, main="년도별 치킨집 창업건수", names.arg = cc$년도, ylim = c(0,1600))
cc$치킨집
barplot(cc[,"치킨집"])

dc <- read.csv("폐업건수.csv", header = T)
dc
barplot(dc$치킨집, main="년도별 치킨집 창업건수", names.arg = dc$년도, ylim = c(0,4000))


x <- rbind(cc$커피음료, dc$커피음료)

x

barplot(x, main = "년도별 커피음료 창업,폐업", names.arg= cc$년도, col=c("yellow", "orchid"), ylim = c(0,4000), beside =T, legend=c("창업","폐업"))



library(shiny)
setwd("c:\\data") 

create_cnt <- read.csv("창업건수.csv", header=T)

# Define UI ----

ui <- fluidPage(    
  # Give the page a title
  
  titlePanel("년도별 업종별 창업현황"),
  # Generate a row with a sidebar
  
  sidebarLayout(      
    
    # Define the sidebar with one input
    
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
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
    
    barplot(create_cnt[,input$region], 
            main=input$region,
            col = rainbow(10),
            density=50,
            names.arg= create_cnt$년도,    
            ylab="건수",
            xlab="년도")
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)
--------------------------------
  
  
  library(shiny)
setwd("c:\\data") 

create_cnt <- read.csv("폐업건수.csv", header=T)

# Define UI ----

ui <- fluidPage(    
  # Give the page a title
  
  titlePanel("년도별 업종별 창업현황"),
  # Generate a row with a sidebar
  
  sidebarLayout(      
    
    # Define the sidebar with one input
    
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
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
    
    barplot(create_cnt[,input$region], 
            main=input$region,
            col = rainbow(10),
            density=50,
            names.arg= create_cnt$년도,    
            ylab="건수",
            xlab="년도")
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)

---------------------------------
  
  
  
  
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
    barplot( rbind( create_cnt[,input$region], 
                    drop_cnt[,input$region] ),
             main=input$region,
             col = c("dark orchid","peru"),
             beside=T, 
             legend=c("창업","폐업"),
             names.arg= create_cnt$년도,    
             ylab="건수",
             xlab="년도",
             args=list(x='topright', bty='n', inset=c(0,-0.2)))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)



------------
  
  
  pie(emp$sal, main= "Salary Pie Chart", labels = emp$ename, col=rainbow(15))
sal_labels <- round(emp$sal/sum(emp$sal)*100,1)
sal_labels
sal_labels2 <- paste(emp$ename, sal_labels, '%')
sal_labels2
pie(emp$sal, main= "Salary Pie Chart", labels = sal_labels2, col=rainbow(15))
--------------
  
cc
x2<-cc[cc$년도=='2014',][-1]
pie(t(x2))
x3<-round(cc[cc$년도=='2014',][-1]/sum(cc[cc$년도=='2014',][-1])*100,1)
sum(x3)
x3
pie(t(x3),labels = cc_label,main="14년도 업종별 창업비율")
colnames(cc)
colnames(cc)[-1]
cc_label <- paste(colnames(cc)[-1],x3,'%')
cc_label
t(x2)
x2
str(x2)
str(t(x2))

--------------
  
  
cc
x2 <- cc[cc$년도== '2013' , ][-1]
x3<-round(x2/sum(x2)*100,1)
x3
cc_label <- paste(colnames(cc)[-1],x3,'%')
pie(t(x3),labels = cc_label,main="13년도 업종별 창업비율")

-----------------------------------
  
library(shiny)
library(datasets)

setwd("d:\\data")
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T)

# Define UI ----
ui <- fluidPage(
  
  # Give the page a title
  titlePanel("년도별 업종별 창업현황"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:",
                  choices=create_cnt$년도 ) ,
      hr(),
      helpText("년도별 창업 현황 보고서")
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
    
    x2 <-?? create_cnt[ create_cnt$년도==input$region, -1?? ]
    cnt_labels <- round( x2/sum(x2) * 100, 1)??
    cnt_labels2 <-?? paste( colnames(cnt_labels) ,cnt_labels ,'%')??
    pie(?? t(x2)?? , col=rainbow(7), labels=cnt_labels2)??
    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
----------------------------------------------
  
  library(shiny)
library(datasets)

setwd("d:\\data") 
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("년도별 업종별 창업현황"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:", 
                  choices=create_cnt$년도 ) ,
      helpText("년도별 창업 현황 보고서")
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
    
    x2 <-  create_cnt[ create_cnt$년도==input$region, -1  ]
    cnt_labels <- round( x2/sum(x2) * 100, 1) 
    cnt_labels2 <-  paste( colnames(cnt_labels) ,cnt_labels ,'%') 
    pie(  t(x2)  , col=rainbow(7), labels=cnt_labels2) 
    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
  

---------------
  
  
  

---------------------------
  library(shiny)
library(datasets)
library(data.table)
setwd("d:\\data") 
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("년도별 업종별 폐업현황"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:", 
                  choices=drop_cnt$년도 ) ,
      helpText("년도별 폐업 현황 보고서")
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
    
    a=drop_cnt[drop_cnt$년도==input$region,-1]
    a=data.table(colnames(a),t(a))
    colnames(a)=c('가게','건수')
    per=round(a$건수/sum(a$건수)*100,1)
    ggplot(a, aes(x = "", y = 건수, fill=가게)) +
      geom_bar(width = 1, stat = "identity",color='white') +
      coord_polar("y")  + 
      geom_text(aes(label = paste0(per,"%")),
                position = position_stack(vjust = 0.5))   
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)



---------
  
carr <- c(1,3,6,4,9)
carr
plot(carr)
