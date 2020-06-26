
library(KoNLP)
library(wordcloud)
library(plyr)

useSejongDic() # ���������� �ִ� �ѱ��� R�� �ε��ϴ� ���ɾ�

setwd("d:\\data")  # ��ŷ ���丮 ��ȯ
NIV <- readLines('NIV.txt') 

nouns <- extractNoun(NIV)# ���� �ܾ ����
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
           family="malgun")        #  ���� �۾�ü??
