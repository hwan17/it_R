finsu <- function(){
  #setwd("d:\\data")
  #insu <- read.csv("csv\\insurance.csv")
  #head(insu)
  #attach(insu)
  #lm(expenses~age+children+smoker+bmi+region,insu)
  fage = readline('나이')
  fsex = readline('성별(male/female)')
  fbmi = readline('bmi (16~53)')
  fchildren = readline('부양가족수')
  fsmocker = readline('흡연여부 (yes/no)')
  fregion = readline('거주지역 (southwest/southeast/northwest/northeast)')
#  if (fregion == 'northeast'){
#    fregion2 = 0
#  } else if (fregion == 'northwest'){
#    fregion2 = -352
#  } else if (fregion == 'southeast'){
#    fregion2 = -1034.9
#  } else if (fregion == 'southwest'){
#    fregion2 = -958.6
#  }
  if (fregion == 'northeast'){
    fregion2 = 0
  } else if (fregion == 'northwest'){
    fregion2 = -352.8
  } else if (fregion == 'southeast'){
    fregion2 = -1035.6
  } else if (fregion == 'southwest'){
    fregion2 = -959.3
  }
  #re = -19993.3 + 257*as.integer(fage) + 474.8*as.integer(fchildren) + 23835.2*ifelse(fsmocker=='yes',1,0) + 338.8*as.double(fbmi) + fregion2
  re = -19941.6 + 256.8*as.integer(fage) -131.4*ifelse(fsex=='male',1,0)+ 475.7*as.integer(fchildren) + 23847.5*ifelse(fsmocker=='yes',1,0) + 339.3*as.double(fbmi) + fregion2
  return (re)
  
}

finsu()


#------------------------------------------
  
  

