
my_reg <- function(){
  fname <- file.choose() # 윈도우 탐색기 열리게 하는 코드
  table <- read.csv(fname, header=T, stringsAsFactor=F )
  table2 <- table
  age <- as.integer(readline('나이가 어떻게 되십니까? '))
  sex <- as.character(readline('성별이 어떻게 되십니까? (male/female) '))
  bmi <- as.numeric(readline('비만지수가 어떻게 되십니까? (16 ~ 53) '))
  children <- as.integer(readline('부양가족수가 몇명입니까? '))
  smoker <- as.character(readline('흡연을 하십니까? (yes, no) '))
  region <- as.character(readline('사는 지역이 어디십니까? (southwest/southeast/northwest/northeast) '))
  bmi30 <- as.integer(readline('bmi30 은?'))
  test <- data.frame(age = age, sex = sex, bmi = bmi, children = children, smoker = smoker, bmi30=bmi30)
  table2$bmi30 <- ifelse(table$bmi >= 30, 1, 0)
  model <- lm(expenses ~age + age^2 + children + bmi + sex + bmi30 * smoker + region + bmi * smoker +
                bmi * region + region * bmi30 + smoker * region, data = table2)
  a <- predict(model, test)
  print(paste('AI가 예측한 의료비는', round(a), '달러 입니다. '))
}

my_reg()
