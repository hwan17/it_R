
library(KoNLP)
library(wordcloud)
library(plyr)

useSejongDic() # 세종사전에 있는 한글을 R로 로드하는 명령어

setwd("d:\\data")  # 워킹 디렉토리 소환
NIV <- readLines('NIV.txt') 

nouns <- extractNoun(NIV)# 명사 단어만 추출
nouns <- unlist(nouns)  
nouns
nouns <- nouns[nchar(nouns)>=2] # 단어의 길이가 2글자 이상인 것만 뽑겠다.
cnouns <- count(nouns)  # 단어별 건수 출력 ( 건수가 많은게 크게 그려지도록 )

# 색깔 추가
pal <- brewer.pal(6,"Dark2") 
pal <- pal[-(1)]

# 폰트 설정
windowsFonts(malgun=windowsFont("맑은 고딕"))

wordcloud( words=cnouns$x,# 단어
           freq=cnouns$freq,         # 빈도수
           colors=pal,             # 색
           min.freq=3,            # 빈도수가 3 이상인 것 기준 시각화
           random.order=F,         # F 로 하게되면 큰 글씨부터 출력 중앙에서부터 퍼지게
           family="malgun")        #  맑음 글씨체??

