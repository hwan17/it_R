Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_251')
install.packages("rJava")
library(rJava)
install.packages("KoNLP")  

install.packages("wordcloud")

install.packages("plyr") 

install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")



install.packages("remotes")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force=TRUE)



library(KoNLP)

useSejongDic() # 3:none

useSejongDic() 

setwd("d:\\data")  # ��ŷ ���丮 ��ȯ
winter <- readLines('winter.txt') 

nouns <- extractNoun(winter)# ���� �ܾ ����
nouns <- unlist(nouns)  
nouns
nouns <- nouns[nchar(nouns)>=2] # �ܾ��� ���̰� 2���� �̻��� �͸� �̰ڴ�.
cnouns <- count(nouns)  # �ܾ �Ǽ� ��� ( �Ǽ��� ������ ũ�� �׷������� )

# ���� �߰�
pal <- brewer.pal(6,"Dark2") 
pal <- pal[-(1)]

# ��Ʈ ����
windowsFonts(malgun=windowsFont("���� ����"))

wordcloud( words=cnouns$x,# �ܾ�
           freq=cnouns$freq,         # �󵵼�
           colors=pal,             # ��
           min.freq=3,            # �󵵼��� 3 �̻��� �� ���� �ð�ȭ
           random.order=F,         # F �� �ϰԵǸ� ū �۾����� ��� �߾ӿ������� ������
           family="malgun")    