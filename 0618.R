
############## set this file location to working directory ##########################
packages <- 'rstudioapi'
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library('rstudioapi')
current_dir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

# package_in : ������ ��ġ, ������ libraryȭ �϶�� �Լ�
package_in<-function(p_name,option=1){
  packages <- p_name
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  if (option==1){
    library(p_name,character.only = TRUE)
  }
}

###########################1. ��Ű�� ��ġ##########################################

package_in('shinydashboard') # shinydashboard ��Ű���� ������ ��ġ�ϰ� ������ libraryȭ �϶�� ��
package_in('shiny')
package_in('ggplot2')
package_in('plotly')
package_in('lattice')

######################### 2. ȭ�� ���� ###########################################

sidebar <- dashboardSidebar(
  
  
  
)


body <- dashboardBody(
  
  
  
  
)



ui<-dashboardPage(
  dashboardHeader(title='my graph'),
  sidebar,
  body
  
)




######################3. ������ ���� ########################################


server <- function(input, output,session) {
  options(warn = -1)
  options(shiny.maxRequestSize = 30*1024^2)
  
  
  
  
  dataload<-reactive({
    
    
    
    
    
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
  
  
}



######################### 4. ���̴� ���� ###############################

shinyApp(ui = ui, server = server)
