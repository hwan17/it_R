stats <- function() {
  fname <- file.choose()
  fname
  
  table <- read.csv(fname, header =T)
  
  #t_name <- readline('���̺��� �Է�')
  #table_name = get(t_name)
  xcol_name <- readline('�÷��� �Է�')
  xdata <- table[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('�ִ밪', max(x), '\n',
      '�ּҰ�', min(x), '\n',
      '��հ�', mean(x),'\n',
      '�߾Ӱ�', median(x),'\n',
      '�л갪', var(x) , '\n',
      'ǥ������',sd(x) )
}

stats()