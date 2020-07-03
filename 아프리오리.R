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
wordcount2
#8. 단어의 건수가 100 보다 큰것만 필터링
keyword <- names( wordcount2[wordcount2>100] )
keyword
length(lala_positive)


#9. 아프리오리 분석을 위해서 표형태로 만드는 작업
contents <- c()
for(i in 1:length(lala_positive)) {
  inter <- intersect(lala_positive[[i]] , keyword)
  contents <- rbind(contents ,table(inter)[keyword])
}

#10. 표의 컬럼명에 단어가 들어가게한다.
colnames(contents) <- keyword

#11. na 를 숫자 0 으로 변경한다.
contents[which(is.na(contents))] <- 0
dim(lala_positive)

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
