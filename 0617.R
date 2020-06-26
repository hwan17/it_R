#132

x1 <- c(7,8,9,9,10,10,11,11,12,13)
x2 <- c (7,9,9,10,10,10,10,11,11,13 )
x3 <- c(1,1,7,7,10,10,10,11,13,30 )

mean(x1) #10
mean(x2) #10
mean(x3) #10

median(x1) #10
median(x2) #10
median(x3) #10

table(x1)
names(table(x1))
names(table(x1))[table(x1) == max(table(x1))] # "9", "10", "11"

table(x2)
names(table(x2))
names(table(x2))[table(x2) == max(table(x2))] # "10"

table(x3)
names(table(x3))
names(table(x3))[table(x3) == max(table(x3))] # "10"

range(x1)
range(x2)
range(x3)

#133

x3 <- c(1,1,7,7,10,10,10,11,13,30 )
a <- boxplot(x3)
a$stats


#134

x2 <- c (7,9,9,10,10,10,10,11,11,13 )
x3 <- c(1,1,7,7,10,10,10,11,13,30 )
b <- boxplot(x2,x3)
b$stats

#135

x1 <- c(7,8,9,9,10,10,11,11,12,13)
x2 <- c (7,9,9,10,10,10,10,11,11,13 )
b <- boxplot(x1,x2)
b$stats
var(x1)
var(x2)
sd(x1)
sd(x2)


#136

setwd("c:\\data")
car <- read.csv("usedcars.csv", header=T,stringsAsFactors=F)
str(car)
a <- boxplot(car$price, horizontal = T)
quantile(car$price)

IQR(car$price)
quantile(car$price)[4]-quantile(car$price)[2]


#137
car$mileage
nrow(car)
sort(car$mileage)
hist(sort(car$mileage), col = "orchid", main = "주행거리 히스토그램 그래프")
a <- hist(sort(car$mileage), col = "orchid", main = "주행거리 히스토그램 그래프")
a
hist(sort(car$mileage), col = "orchid", main = "주행거리 히스토그램 그래프")
par(new=T)
class1 <- sort(car$mileage)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)), type='l', axes = F, ann = F, col = "orchid")
box()


#138
hist(sort(car$price), col = "orchid", main = "가격 히스토그램 그래프")
par(new=T)
class1 <- sort(car$price)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)), type='l', axes = F, ann = F, col = "orchid")
box()



---
# 139
iris
nrow(iris) # 150
#140
ncol(iris) # 5
str(iris)
#141
unique(iris$Species)



#142

summary(iris)


#143
hist(sort(iris$Sepal.Width), col = "orchid")
par(new=T)
class1 <- sort(iris$Sepal.Width)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)), type='l', axes = F, ann = F, col = "orchid")
box()




#145
b <- hist(sort(iris$Sepal.Width), col = "orchid", breaks =  seq(2.0,5.0, by = 1), prob = T, ylim = c(0,1))


#146
colnames(iris)


iris$Sepal.Width


#147


library(shiny)
library(datasets)

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("iris 데이터의 히스토그램 그래프"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "컬럼:", 
                  choices= colnames(iris)),
      hr(),
      helpText("iris 컬럼의 히스토그램 그래프 ")
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
    
    hist( iris[ ,input$region ] , col="green", density=80 ) 
    par(new=T)
    class1 <- sort(iris[ ,input$region ])
    plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)), type='l', axes = F, ann = F, col = "orchid")
    box()
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


-------
  
  
#149
  
  
  library(shiny)
library(datasets)

# Define UI ----
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("iris 데이터의 히스토그램 그래프"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "컬럼:", 
                  choices= colnames(iris)),
      hr(),
      helpText("iris 컬럼의 히스토그램 그래프 ")
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
    
    par(mfrow=c(2,2))  # 1행 2열이라는 뜻
    x <- sort(iris[ ,input$region ])
    hist( iris[ ,input$region ] , col="orchid", density=80 , main=input$region)
    par(new=T)
    plot(x, dnorm(x, mean=mean(x), sd=sd(x)), col='red', axes=F, ann=F, type='l')
    
    boxplot(iris[ ,input$region ])
    
    pie(table(iris$Species))
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


----------------------
  

pie(iris$Species)
pie(table(iris$Species))



iris


------------------------
  
  library(shiny)

library(datasets)
library(data.table)


# Define UI ----

ui <- fluidPage(    
  
  
  
  # Give the page a title
  
  titlePanel("iris 데이터의 히스토그램, 사분위수, 원형 그래프"),
  
  
  
  # Generate a row with a sidebar
  
  sidebarLayout(      
    
    
    
    # Define the sidebar with one input
    
    sidebarPanel(
      
      selectInput("region", "컬럼:", 
                  
                  choices= colnames(iris[1:4])),
      
      hr(),
      
      helpText("iris 컬럼의 히스토그램 그래프와 사분위수, 원형 그래프 ")
      
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
    
    
    
    par(mar=c(0.1,0.1,1,0.1))
    
    par(mfrow=c(2,2))
    
    hist( iris[ ,input$region ] , col="green",main='아이리스 Histogram',xlab=input$region)
    
    par(new=T)
    
    a = sort(iris[,input$region])
    
    plot(a,dnorm(a,mean=mean(a),sd=sd(a)),type='l',axes=FALSE,ann=FALSE,col='maroon')
    
    
    
    boxplot( iris[ ,input$region ] , col="medium purple",main='아이리스',xlab=input$region)
    
    
    
    x2 = iris[iris$Species=="setosa",]
    
    x3 = iris[iris$Species=="versicolor",]
    
    x4 = iris[iris$Species=="virginica",]
    
    
    
    par(mar=c(0.1,0.1,1,0.1))
    
    cnt = data.table(setosa=sum(x2[ ,input$region ]),versicolor=sum(x3[ ,input$region ]),virginical=sum(x4[ ,input$region ]))
    
    cnt_labels <- round( cnt/sum(cnt) * 100, 1) 
    
    cnt_labels2 <-  paste( colnames(cnt_labels) ,cnt_labels ,'%') 
    
    pie(  t(cnt)  , col=rainbow(7), labels=cnt_labels2,main=c('꽃 종류별',input$region))
    
    
    
  })
  
}



# Run the app ----

shinyApp(ui = ui, server = server)





iris.Species

x3 = iris[iris$Species=="setosa",]

x3

x4 = iris[iris$Species=="versicolor",]

x5 = iris[iris$Species=="virginica",]

unique(iris$Species)

x3

sum(iris[ ,"Sepal.Length" ])

cnt = data.table(setosa=sum(x3$Sepal.Length),versicolor=sum(x4$Sepal.Length),virginical=sum(x5$Sepal.Length))

cnt

cnt_labels <- round( cnt/sum(cnt) * 100, 1) 

cnt_labels

b = round(x3/sum(x3)*100,1)

paste(colnames(b),b,'%')

pie(t(x3))



iris

cnt_labels2 <-  paste( colnames(cnt_labels) ,cnt_labels ,'%') 

pie(  t(cnt)  , col=rainbow(7), labels=cnt_labels2)


  
