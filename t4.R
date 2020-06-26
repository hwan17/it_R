x1 = c(7,8,9,9,10,10,11,11,12,13)
x2 = c(7,9,9,10,10,10,10,11,11,13)
x3 = c(1,1,7,7,10,10,10,11,13,30)

library(plotly)
subplot(
  add_markers = plot_ly(y=~x1,type='box',name='hi'),
  add_markers = plot_ly(y=~x3,type='box',name='no')
)

