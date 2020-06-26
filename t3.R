stats <- function() {
  fname <- file.choose()
  fname
  
  table <- read.csv(fname, header =T)
  
  #t_name <- readline('테이블명 입력')
  #table_name = get(t_name)
  xcol_name <- readline('컬럼명 입력')
  xdata <- table[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) )
}

stats()
