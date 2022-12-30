# 서울시 노인의료복지시설 데이터 전처리

# 데이터 불러오기
home <- read.csv("home.csv")

# 변수명 변경
names(home) <- c("시설명", "a", "b",    
                 "c", "d", "e", "f", "지역명", "g")

# 불필요한 변수 제거
home1 <- subset(home, select=-c(a, b, c, d, e, f, g))

# 결과 확인
home1