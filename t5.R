
############## set this file location to working directory ##########################
packages <- 'rstudioapi'
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library('rstudioapi')
current_dir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

package_in<-function(p_name,option=1){
  packages <- p_name
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  if (option==1){
    library(p_name,character.only = TRUE)
  }
}

###########################1. 패키지 설치##########################################

package_in('shinydashboard')
package_in('shiny')
package_in('ggplot2')
package_in('plotly')
package_in('lattice')

######################### 2. 화면 개발 ###########################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",".xlsx",".txt",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    menuItem("Plot",
             menuSubItem('Barplot',tabName='barplot'),
             menuSubItem('Piechart',tabName='piechart'),
             menuSubItem('Lineplot',tabName='lineplot'),
             menuSubItem('Scatterplot',tabName='scatterplot')
    )
    
    
  )
)


body <- dashboardBody(
  
  tabItems(
    
    ##### bar plot
    tabItem(tabName = "barplot",
            sidebarPanel(
              selectInput("in_sel_bar_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_bar_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotOutput('plot_bar')
            )
    ),
    ##### piechart
    tabItem(tabName = "piechart",
            sidebarPanel(
              selectInput("in_sel_pie_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotlyOutput('plot_pie')
            )
    ),
    ##### line plot
    tabItem(tabName = "lineplot",
            sidebarPanel(
              selectInput("in_sel_line_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_line_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotlyOutput('plot_line')
            )
    ),
    ##### scatter plot
    tabItem(tabName = "scatterplot",
            sidebarPanel(
              selectInput("in_sel_scatter_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_scatter_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotOutput('plot_scatter'),
              textOutput('text_scatter')
            )
    )
  )
)



ui<-dashboardPage(
  dashboardHeader(title='my graph'),
  sidebar,
  body
  
)




######################3. 서버단 개발 ########################################


server <- function(input, output,session) {
  options(warn = -1)
  options(shiny.maxRequestSize = 30*1024^2)
  
  
  
  
  dataload<-reactive({
    req(input$file1)
    
    file1 = input$file1
    data1 = read.csv(file1$datapath)
    
    
    updateSelectInput(session, "in_sel_bar_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_bar_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_pie_xVar", choices = data1[,1])
    
    updateSelectInput(session, "in_sel_line_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_line_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_scatter_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_scatter_yVar", choices = colnames(data1))
    
    return(data1)
    
  })
  
  ####nomal_bar
  output$plot_bar <- renderPlot({
    table_in<-dataload()
    
    xdata<-as.factor(table_in[,input$in_sel_bar_xVar])
    ydata<-as.factor(table_in[,input$in_sel_bar_yVar])
    fdata=data.frame(x=xdata,y=ydata)
    
    
    ggplot(fdata) + 
      geom_bar(aes_string(x='x',y='y',fill='x'),stat = "identity",show.legend=F)
    
    
  })
  
  output$plot_pie <- renderPlotly({
    table_in<-dataload()
    
    plot_ly(table_in, labels = ~colnames(table_in)[-1], values=~as.factor( table_in[table_in[,1] == input$in_sel_pie_xVar,-1] ),type='pie')
    
    
  })
  
  output$plot_line <- renderPlotly({
    table_in<-dataload()
    
    x <- list(title = input$in_sel_line_xVar)
    y <- list(title = input$in_sel_line_yVar)
    
    plot_ly(data = table_in,x=~table_in[,input$in_sel_line_xVar],y=~table_in[,input$in_sel_line_yVar],type='scatter',mode='dot')%>%
      layout(xaxis = x, yaxis = y)
    
    
  })
  
  output$plot_scatter <- renderPlot({
    table_in<-dataload()
    
    xyplot(table_in[,input$in_sel_scatter_yVar]~table_in[,input$in_sel_scatter_xVar], grid=T,type=c('p','smooth'),col.line='darkorange',lwd=2, xlab=input$in_sel_scatter_xVar,ylab=input$in_sel_scatter_yVar)
    
  })
  
  output$text_scatter <- renderText({
    table_in<-dataload()
    paste("The correlation between the two is: ", cor(table_in[,input$in_sel_scatter_yVar],table_in[,input$in_sel_scatter_xVar]))
  })
  
}

######################### 4. 샤이니 실행 ###############################

shinyApp(ui = ui, server = server)