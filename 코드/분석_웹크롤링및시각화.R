# 네이버 뉴스기사 웹 크롤링 및 시각화 코드

## 웹 크롤링
# 사용 패키지
install.packages('rvest') # 웹문서 수집에 필요한 함수 제공 
library(rvest)  

# 노인복지 관련 뉴스기사(기간 : 2022.01.01 ~ 2022.08.29)
base_url <- 'https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%85%B8%EC%9D%B8%20%EB%B3%B5%EC%A7%80&sort=0&photo=0&field=0&pd=3&ds=2022.01.01&de=2022.08.29&cluster_rank=24&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20220101to20220829,a:all&start='

urls <- c()

# 6000페이지까지 분석
for (x in 0:6000) {
  urls <- c(urls, paste(base_url,x*10+1,sep='')) 
} 

length(urls) # 6001


# html 문서 읽기 & url 수집
old_urls <- c()

for (url in urls){
  html <- read_html(url)
  html_a <- html_nodes(html,'ul.list_news > li > div > div > div > div.info_group > a.info')
  old_urls <- c(old_urls, html_attr(html_a,'href'))
}

length(old_urls) # 66222

# 네이버 뉴스 url만 수집
news_urls <- old_urls[which(grepl("n.news.naver.", old_urls))]
news_urls <- unique(news_urls)

length(news_urls) # 496

length(which(grepl("n.news.naver.", old_urls)))

# 뉴스 기사 제목 & 내용 수집
title <- c()
body <- c()

for (i in 1:length(news_urls)){
  html <- read_html(news_urls[i])
  html_t <- html_nodes(html, 'body > div > div > div> div > div > div > div.media_end_head_title')
  title <- c(title, html_t %>% html_text()) # 제목 수집
  
  html_b <- html_nodes(html, 'body > div > div > div> div > div > div > div.newsct_article')
  body <- c(body, html_b %>% html_text()) # 내용 수집
}

# 전처리
title_1 <- gsub("[\r\n\t]", ' ', title) # 이스케이프 제거
title_1 <- gsub('[[:punct:]]',' ',title_1) # 문장부호 제거
title_1 <- gsub('[[:cntrl:]]',' ',title_1) # 특수문자 제거
title_1 <- gsub('\\s+',' ',title_1)
title_1

body_1 <- gsub("[\r\n\t]", ' ', body) # 이스케이프 제거
body_1 <- gsub('[[:punct:]]',' ',body_1) # 문장부호 제거
body_1 <- gsub('[[:cntrl:]]',' ',body_1) # 특수문자 제거
body_1 <- gsub('\\s+',' ',body_1)
body_1


## 시각화 과정
# 사용 패키지
library(KoNLP)
library(tm)
library(wordcloud)

exNouns <- function(x) { 
  paste(extractNoun(as.character(x)), collapse=" ")
}

# exNouns 함수 이용 단어 추출 
news_nouns <- sapply(body_1, exNouns) 

# 자료 전처리 : tm 패키지 
myCorpus <- Corpus(VectorSource(news_nouns)) 

myCorpusPrepro <- tm_map(myCorpus, removePunctuation) # 문장부호 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers) # 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower) # 소문자 변경

# 불용어 추가 후 제거
myStopwords = c(stopwords('english'), "노인", "어르신", "지역", "사업",
                "운영", "제공", "들이", "뉴스","포토","지원","노인복지관",
                "활동", "서비스","사회","진행","코로나","이번","센터",
                "대상", "이용", "전달","참여", "필요", "이상",
                "사진", "하기","다양","시설","프로그램"); # 제거할 문자 추가
myCorpusPrepro <-tm_map(myCorpusPrepro, removeWords, myStopwords) # 불용어제거

# 단어 선별(단어 길이 2개 이상)
# (1) 한글 단어길이 2음절 ~ 8음절(한글 1개 2byte) 
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, 
                                          control=list(wordLengths=c(4,16))) 

myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term)) 

# 단어 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing=TRUE) 
wordResult[1:10] # top10 단어  

# 단어 구름 만들기
myName <- names(wordResult)  
word.df <- data.frame(word=myName, freq=wordResult) 
pal <- brewer.pal(8,"Set2") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows

wordcloud(word.df$word, word.df$freq, 
          scale=c(3,1), min.freq=2, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")

# 원그래프
topWord <- head(sort(wordResult, decreasing=T), 10) # 상위 10개 토픽추출 
# (2) 파일 차트 생성 
pie(topWord, radius=0.6,cex=0.8, labels=label) 
# radius=1 : 반지름 지정 - 확대 기능  

# (3) 빈도수 백분율 적용 
pct <- round(topWord/sum(topWord)*100, 1) # 백분율

# (4) 단어와 백분율 하나로 합친다.
label <- paste(names(topWord), "\n", pct, "%")

# (5) 파이차트에 단어와 백분율을 레이블로 적용 
pie(topWord, main="노인 복지 네이버 뉴스 기사 분석", 
    cex=0.8, labels=label)
