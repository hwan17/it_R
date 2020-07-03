
setwd("d:\\data\\csv")


##apriori 알고리즘 예제 3 ( 영화 라라랜드 )
#1. 라라랜드 데이터를 로드한다.
library(KoNLP)
library(wordcloud)

lala <- read.csv('라라랜드.csv', header=T, stringsAsFactors = F)

#2. 영화평점이 9점이상은 긍정변수넣고 2점 이하는 부정변수에 넣는다
lala_positive <- lala[lala$score>=9,c('content')]
lala_negative <- lala[lala$score<=2,c('content')]

head(lala_positive)
head(lala_negative)

#3. 긍정게시판 변수에서 명사만 추출하고 데이터 정제 작업을 한다.
po <- sapply(lala_positive, extractNoun, USE.NAMES=F)

po2 <- unlist(po)
po2 <- Filter(function(x){nchar(x)>=2},po2)
po3 <- gsub('\\d+','',po2)
po3 <- gsub('관람객','',po3)
po3 <- gsub('평점', '', po3)
po3 <- gsub('영화', '', po3)
po3 <- gsub('진짜', '', po3)
po3 <- gsub('완전', '', po3)
po3 <- gsub('시간', '', po3)
po3 <- gsub('올해', '', po3)
po3 <- gsub('장면', '', po3)
po3 <- gsub('남자', '', po3)
po3 <- gsub('여자', '', po3)
po3 <- gsub('만큼', '', po3)
po3 <- gsub('니가', '', po3)
po3 <- gsub('년대', '', po3)
po3 <- gsub('옆사람', '', po3)
po3 <- gsub('들이', '', po3)
po3 <- gsub('저녁', '', po3)

write(unlist(po3), 'lala_positive.txt')

po4 <- read.table('lala_positive.txt')

po_wordcount <- table(po4)
po_wordcount
#4. 라라랜드 영화에 부정적인 평가 게시글들 명사로 변경하고 정제 작업을 수행한다.
ne <- sapply(lala_negative, extractNoun, USE.NAMES=F)
ne2 <- unlist(ne)
ne2 <- Filter(function(x){nchar(x)>=2},ne2)
ne3 <- gsub('\\d+','',ne2)
ne3 <- gsub('관람객','',ne3)
ne3 <- gsub('평점', '', ne3)
ne3 <- gsub('영화', '', ne3)
ne3 <- gsub('진짜', '', ne3)
ne3 <- gsub('완전', '', ne3)
ne3 <- gsub('시간', '', ne3)
ne3 <- gsub('올해', '', ne3)
ne3 <- gsub('장면', '', ne3)
ne3 <- gsub('남자', '', ne3)
ne3 <- gsub('여자', '', ne3)
ne3 <- gsub('만큼', '', ne3)
ne3 <- gsub('니가', '', ne3)
ne3 <- gsub('년대', '', ne3)
ne3 <- gsub('옆사람', '', ne3)
ne3 <- gsub('들이', '', ne3)
ne3 <- gsub('저녁', '', ne3)

write(unlist(ne3), 'lala_negative.txt')

ne4 <- read.table('lala_negative.txt')

ne_wordcount <- table(ne4)
ne_wordcount
#5. 긍정 단어와 부정단어를 각각 워드 클라우드로 그려서 한 화면에 출력한다.
graphics.off()
palete <- brewer.pal(9,'Set1')
par(new=T, mfrow=c(1,2))
wordcloud(names(po_wordcount), freq=po_wordcount, scale=c(3,1), rot.per=0.1, random.order = F, random.color = T, col=rainbow(15))
title(main='라라랜드의 긍정적인 평가', col.main='blue')

wordcloud(names(ne_wordcount), freq=ne_wordcount, scale=c(3,1), rot.per=0.1, random.order = F,random.color = T, col=rainbow(15))
title(main='라라랜드의 부정적인 평가', col.main='red')

#문제. 라라랜드의 긍정적 평가 게시판의 글들을 명사만추출한 다음 단어들간의 연관관계를 출력하시오
#답 :
#1. 관련된 패키지 설치
library(KoNLP)
library(wordcloud)
library(tm)
library(stringr)
library(arules)

#2. 명사 추출하는 코드
lala_positive <- sapply(lala_positive, extractNoun, USE.NAMES=F)
head(lala_positive)

#3. unlist 로 변환한후에 철자가 2개이상이고 5개 이하인것만 추출
c <- unlist(lala_positive)
lala_positive2 <- Filter(function(x) { nchar(x) >= 2 & nchar(x) <= 5 } , c)

#4. 데이터 정제작업( 분석하기에 너무 많이 나오는 단어를 삭제하는 작업 )
# 숫자제거
lala_positive2 <- gsub('\\d+','',lala_positive2)
lala_positive2 <- gsub('관람객','',lala_positive2)
lala_positive2 <- gsub('평점', '', lala_positive2)
lala_positive2 <- gsub('영화', '', lala_positive2)
lala_positive2 <- gsub('진짜', '', lala_positive2)
lala_positive2 <- gsub('완전', '', lala_positive2)
lala_positive2 <- gsub('시간', '', lala_positive2)
lala_positive2 <- gsub('올해', '', lala_positive2)
lala_positive2 <- gsub('장면', '', lala_positive2)
lala_positive2 <- gsub('남자', '', lala_positive2)
lala_positive2 <- gsub('여자', '', lala_positive2)
lala_positive2 <- gsub('만큼', '', lala_positive2)
lala_positive2 <- gsub('니가', '', lala_positive2)
lala_positive2 <- gsub('년대', '', lala_positive2)
lala_positive2 <- gsub('옆사람', '', lala_positive2)
lala_positive2 <- gsub('들이', '', lala_positive2)
lala_positive2 <- gsub('저녁', '', lala_positive2)
lala_positive2 <- gsub('영화', '', lala_positive2)
lala_positive2


#5. 한글이 아닌 데이터를 제거하는 작업
res <- str_replace_all(lala_positive2, "[^[:alpha:]]","")

#6. "" 데이터 제거하는 작업
res <- res[res != ""]

#7. 단어와 그 건수를 출력하는 작업
wordcount <- table(res)
wordcount2 <- sort( table(res), decreasing=T)

#8. 단어의 건수가 100 보다 큰것만 필터링
keyword <- names( wordcount2[wordcount2>100] )
length(lala_positive)
lala_positive[[1]]

#9. 아프리오리 분석을 위해서 표형태로 만드는 작업
contents <- c()
for(i in 1:length(lala_positive)) {
  inter <- intersect(lala_positive[[i]] , keyword)
  contents <- rbind(contents ,table(inter)[keyword])
}
contents
#10. 표의 컬럼명에 단어가 들어가게한다.
colnames(contents) <- keyword

#11. na 를 숫자 0 으로 변경한다.
contents[which(is.na(contents))] <- 0
str(lala_positive)

#12. 아프리오리 데이터 분석
detach(package:tm, unload=T)
library(arules)

rules_lala <- apriori(contents , parameter = list(supp = 0.007 , conf = 0.3 , target = "rules"))
rules_lala

inspect(sort(rules_lala))

b2 <- t(as.matrix(contents)) %*% as.matrix(contents)
b2

b2.w <- b2 - diag(diag(b2))
b2.w

library(sna)
library(rgl)

#rownames(b2.w)
#colnames(b2.w)
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "green" ,
      edge.col="blue" , boxed.labels=F , arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2)

#visualization

gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "pink" ,
      edge.col="light green" , boxed.labels=F ,
      arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2)
