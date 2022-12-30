# 노인 인구수 예측을 위한 시계열 분석 코드

# 사용 패키지
install.packages("TTR")
install.packages("forecast")
library(TTR)
library(forecast)
library(help='forecast')
library(help='TTR')

# 데이터 불러오기
total <- c(1018534,1025615,1038150,1044750,1065727,1078098,1092672,1105583,1124282,1135929,1149455,1161708,1178004,1188361,1204968,1233528,1233528,1243829,1253588,1262436,1274296,1280784,1288162,1295899,1315717,1333704,1348123,1359901,1376261,1387631,1399494,1410297,1429163,1444763,1461460,1478664,1510460,1527341,1544944,1561139,1559400,1568769,1584249,1597447)

# 시계열 데이터로 전환
old_total <- ts(total, frequency = 4, start=c(2011,1))
old_total

# old_total : 시계열 데이터
#       Qtr1    Qtr2    Qtr3    Qtr4
# 2011 1018534 1025615 1038150 1044750
# 2012 1065727 1078098 1092672 1105583
# 2013 1124282 1135929 1149455 1161708
# 2014 1178004 1188361 1204968 1233528
# 2015 1233528 1243829 1253588 1262436
# 2016 1274296 1280784 1288162 1295899
# 2017 1315717 1333704 1348123 1359901
# 2018 1376261 1387631 1399494 1410297
# 2019 1429163 1444763 1461460 1478664
# 2020 1510460 1527341 1544944 1561139
# 2021 1559400 1568769 1584249 1597447

# 시계열 시각화
plot.ts(old_total, main = "시계열 데이터 시각화")

# 데이터 추세 확인
old_dec <- decompose(old_total)
plot(old_dec)
plot(old_total - old_dec$trend)

# 시계열 요소 분해 시각화
plot(stl(old_total, s.window = "periodic"))

# 차분
old_diff1 <- diff(old_total, differences = 1)
ts.plot(old_diff1, main="1차 차분")

old_diff2 <- diff(old_total, differences = 2)
ts.plot(old_diff2, main="1차 차분")

# 자기상관함수 - 자기상관성 없음
acf(old_diff1) # MA(q)

# 부분 자기 상관 함수
pacf(old_diff1)  # AR(p)

# 모형 식별과 추정
auto.arima(old_total)
# Series: old_total 
# ARIMA(0,1,0) with drift 
# 
# Coefficients:
#           drift
#       13463.0930
# s.e.    916.3091
# 
# sigma^2 = 36987344:  log likelihood = -435.16
# AIC=874.31   AICc=874.61   BIC=877.83

old_arima <- arima(old_total, order = c(0,1,0), seasonal=list(order=c(1,1,0)))
old_arima
# arima(x = old_total, order = c(0, 1, 0), seasonal = list(order = c(1, 1, 0)))
# 
# Coefficients:
#         sar1
#       -0.5017
# s.e.   0.1652
# 
# sigma^2 estimated as 61595162:  log likelihood = -405.67,  aic = 815.34

# 모형 타당성 검정
tsdiag(old_arima)

Box.test(old_arima$residuals, lag=1, type = "Ljung")
# Box-Ljung test
# 
# data:  old_arima$residuals
# X-squared = 0.44071, df = 1, p-value = 0.5068 > 0.05 : 통게적으로 적절

# 예측
old_fcast <- forecast(old_arima) # 기본값 : 24개월 후
old_fcast
#         Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
# 2022 Q1        1612533 1602475 1622591 1597150 1627915
# 2022 Q2        1625671 1611446 1639895 1603917 1647424
# 2022 Q3        1642216 1624795 1659637 1615573 1668859
# 2022 Q4        1656917 1636801 1677033 1626153 1687682
# 2023 Q1        1663562 1638427 1688696 1625122 1702002
# 2023 Q2        1674809 1645503 1704115 1629989 1719629
# 2023 Q3        1690820 1657866 1723773 1640421 1741218
# 2023 Q4        1704767 1668531 1741003 1649349 1760185

# 예측 그래프
plot(old_fcast, main = "향후 2년간 노인 인구 예측")