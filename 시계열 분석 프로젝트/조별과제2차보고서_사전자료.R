library(readxl)
library(forecast)
library(TTR)
library(tseries)
library(MASS)

wm = read_excel("C:/박규영/4학년_1학기/시계열/조별과제/과제데이터.xlsx")
wm <- head(wm, n = nrow(wm) - 3) # 2005년 1월 ~ 2022년 12월까지 고려하기 위해
man = ts(data=wm[,2], frequency=12, start=c(2005,1))
woman = ts(data=wm[,3], frequency=12, start=c(2005,1))
max_w <- max(woman)

### 남자 

# 남자의 시계열 그래프를 보면 진폭이 일정하지 않고, 평균 역시 일정하지 않음 
# 분산안정화, 평균안정화 필요 
plot(man)
plot(decompose(man)) 

# 분산안정화
boxcox(man~time(man))
bc = boxcox(man~time(man), plotit=F)
lam = bc$x[which.max(bc$y)]
man_v = (man^lam-1)/lam

par(mfrow=c(2,1))
plot(man_v)
plot(log(man)) #로그 변환이 더 나아보임..?

# 추세제거
# 확률적 추세 존재 -> 차분을 통한 정상화 필요
# 계절차분 이전에 계절성이 있는지 판단 필요 

# (1) 계절성이 존재하지 않는다는 가정 하 
plot(diff(log(man), lag = 1, differences = 1))

x = diff(log(man), lag = 1, differences = 1)
kpss.test(x) # 유의
adf.test(x) # 유의

# 1차 차분된 시계열 sacf, spacf 계산 및 arma모형 차수 결정 
acf(x);pacf(x)

# acf는 0.5, 1.5 구간에 0이 아님. pacf도 0.3, 0.6 구간에 0이 아님 -> 그렇다면 계절성이 존재한다는 것인데...?

# (2) 계절성 존재한다는 가정 하 
plot(diff(log(man), lag = 1, differences = 1)) # 1차 차분
plot(diff(log(man), lag = 12, differences = 1)) # 계절차분

# 계절차분 시에도 확률적 추세 존재하는듯함
plot(diff(diff(man, lag = 12, differences = 1),lag = 1, differences = 1)) # 계절차분->1차차분 

# 차분 합리성 부여: kpss.test 확인
kpss.test(diff(man, lag = 12, differences = 1))
# p-value: 0.01 => ho reject (alpha=0.05 하)

# adf.test 확인
adf.test(diff(man, lag = 12, differences = 1))
# p-value: 0.0251 => h0 reject (alpha=0.05 하)

# 2차 차분된 모형의 kpss, adf.test 실시 
x = diff(diff(man, lag = 12, differences = 1),lag = 1, differences = 1) 
kpss.test(x)
adf.test(x)

# 2차 차분(계절차분->1차 차분)된 시계열 sacf, spacf 계산 및 arma모형 차수 결정 
acf(x);pacf(x)

# sacf는 lag=1, spacf도 lag=1이고, 계절성이 존재할 때의 acf, pacf 그림과 달리 lag 배수 사이값들에 0이 아닌 값들이 없음
# 그렇다면 게절차분하지 않은 데이터를 사용? 아니면 계절차분한 데이터를 그냥 ar(1), ma(1) 이런 주기가 없는 모형으로 적합? 
# 이 x데이터를 사용할 때, acf, pacf 그림을 보면 눈에 띄게 0을 넘는 구간이 없으므로 계절 모형 고려 x



### 여자 

plot(woman)
plot(log(woman))

plot(diff(woman, lag = 12, differences = 1))
plot(diff(diff(woman, lag = 12, differences = 1),lag = 1, differences = 1))

# kpss.test 확인
kpss.test(diff(woman, lag = 12, differences = 1))
# p-value: 0.01 => ho reject (alpha=0.05 하)

# adf.test 확인
adf.test(diff(woman, lag = 12, differences = 1))
# p-value: 0.022 => h0 reject (alpha=0.05 하)

x = diff(diff(woman, lag = 12, differences = 1),lag = 1, differences = 1) # 2차 차분된 모형 
acf(x);pacf(x)

arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12))
arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1),period=12))
arima(x,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12))

tsdiag(arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12)))
tsdiag(arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1),period=12)))
tsdiag(arima(x,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12)))

arima(woman, order=c(1,1,1), seasonal=list(order=c(1,1,1)))
      