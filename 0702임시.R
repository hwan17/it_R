install.packages("networkD3")
install.packages("dplyr")
library(networkD3)
library(dplyr)

data(MisLinks, MisNodes)

head(MisNodes) # 책 읽는 시간같은 데이터
head(MisLinks) # 인물끼리 몇번 만났는지 데이터

# plot

D3_network_LM<-forceNetwork(Links = MisLinks, Nodes = MisNodes,
                            Source = 'source', Target = 'target',
                            NodeID = 'name', Group = 'group',opacityNoHover = TRUE,
                            zoom = TRUE, bounded = TRUE,
                            fontSize = 15,
                            linkDistance = 75,
                            opacity = 0.9)

D3_network_LM

# html 발사

networkD3::saveNetwork(D3_network_LM, "D3_LM.html", selfcontained = TRUE)
