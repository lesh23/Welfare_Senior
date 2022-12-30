# 노인의료복지시설의 필요성을 확인하기 위한 순위 확인 코드

# 데이터 불러오기
result <- read.table(file = "result.txt", header = T)

# 칼럼명 설정
names(result) <- c("자치구","2021","2022","2023","복지관수")
result

# 각 구별 노인 인구 증가율
result$rate <- (result$'2023' - result$'2021')/result$`2021` *100

# 각 구별 노인 인구당 복지관의 비율
result$wrate <- result$'복지관수'/result$`2021` * 100

# 노인 인구 증가율 내림차순 점수 부여
result_pop <- result[order(result$rate, decreasing = T),]
result_pop$ratescore <- c(25:1)

# 노인 인구당 복지관 비율 오름차순 점수 부여
result_wel <- result_pop[order(result_pop$wrate),]
result_wel$welscore <- c(25:1)

# 점수 합산 및 내림차순 정렬
result_wel$sum <- result_wel$ratescore + result_wel$welscore
result <- result_wel[order(result_wel$sum, decreasing = T),]

result