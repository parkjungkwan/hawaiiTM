install.packages("tm")
library(tm)
library(wordcloud)
tgt <- readLines("https://www.dropbox.com/s/28n2omtacbbp2aw/Shilla__Hotel__Review.txt?dl=1")
head(tgt)
tgt1 <- VCorpus(VectorSource(tgt)) 
tgt1 <- tm_map(tgt1, tm::stripWhitespace) # 공백처리
tgt1 <- tm_map(tgt1, tolower) # 알파벳이면 소문자
tgt1 <- tm_map(tgt1, removePunctuation) # 마침표, 공백, 세미콜론, 콜론제거
gsb <- c(stopwords(
  ('english')
))
tgt1 <- tm_map(tgt1,removeWords,gsb)
tgt1 <- tm_map(tgt1, PlainTextDocument)
tgt2 <- TermDocumentMatrix(tgt1)
findFreqTerms(tgt2, 5)
findAssocs(tgt2, "신라호텔",0.5)
tgt3 <- as.matrix(tgt2)
head(tgt3)
tgt4 <- sort(rowSums(tgt3),decreasing = T)
tgt4
pal <- brewer.pal(8,"Dark2")
set.seed(1234)
wordcloud(
  names(tgt4),
  freq = tgt4,
  scale = c(2.5,0.1), # 단어크기 0.1 ~ 2.5
  rot.per = 0.25, # 회전비율
  min.freq = 2, # 최저 빈도수 2회이상
  random.order = F, # 고빈도 단어 중앙배치
  random.color = T,
  colors = pal
)
legend(
  0.3,0.8,
  "서울신라호텔 이용후기",
  cex = 0.8,
  fill= NA,
  border = NA,
  bg = 'white',
  text.col = 'red',
  text.font = 2,
  box.col = 'red'
  
)






