#install.packages("rsconnect")
library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='hwan17', token='60D9DB08B314DAAD667DD89403E8CE36', secret='fbgOojzvtthNlJNBTavFUY81qO2tyFAjEhi81y/V')

rsconnect::deployApp("d:\\hw177",appName = 'myapp177' )

