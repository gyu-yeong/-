library(readxl)
library(forecast)
library(TTR)
library(tseries)
library(MASS)

wm = read_excel("C:/박규영/4학년_1학기/시계열/조별과제/과제데이터.xlsx")
wm <- head(wm, n = nrow(wm) - 3) # 2005년 1월 ~ 2022년 12월까지 고려하기 위해
man = ts(data=wm[,2], frequency=12, start=c(2005,1))
woman = ts(data=wm[,3], frequency=12, start=c(2005,1))

man = ts(data=wm[,2], frequency = 12, start=c(2005,1))
woman = ts(data=wm[,3], frequency = 12, start=c(2005,1))

# 추세분석을 사용하여 전체적인 시계열의 특징들을 파악할 수 있다. 
# 특히 그중에서도 추세분석에서 가장 많이 사용하는 회귀분석 기법을 이용하여 데이터의 추세 및 가장 적합한 회귀 모형이 무엇인지에 대하여 분석해보고자 한다.

# 2차, 3차, 4차 다항회귀분석에 대하여 추세분석을 진행한 후 각각 독립변수들이 유의한 결과가 나오는지를 확인하기 위해 유의성 검정을 실시하였다.
# 유의수준 0.05하에서 2차, 3차 회귀분석은 모든 변수들이 유의한 결과가 나오는 것을 확인할 수 있지만
# 4차 회귀분석에서는 이차항 독립변수가 유의하지 않은 결과가 나오는 것을 확인할 수 있다.
# 따라서 해당 데이터에 대해서는 3차 모형까지가 적합한 모형으로 판단한 후 3차모형에 로그변환을 한 것까지 같이 비교해 보았다.

# ---------------------------------- 남성
# 2차 회귀분석
tt = 1:length(man) # 설명변수
reg1 = lm(man~tt+I(tt^2))
summary(reg1) # 독립변수 모두 유의
plot(tt, man, type="l") # 그냥 plot으로 그리면 reg1 직선을 못그림
lines(reg1$fitted.values, col="blue") 


# 3차 회귀분석
reg2 = lm(man~tt+I(tt^2)+I(tt^3))
summary(reg2) # 독립변수 모두 유의
lines(reg2$fitted.values, col="red") 


# 4차 회귀분석
reg3 = lm(man~tt+I(tt^2)+I(tt^3)+I(tt^4))
summary(reg3) # 이차항 독립변수가 유의수준 0.05하에서 귀무가설 기각하지 못함 -> 4차 모형은 부적합
lines(reg3$fitted.values, col="green") 


# 로그변환
plot(log(man))
reg4 = lm(log(man)~tt+I(tt^2)+I(tt^3))
plot(tt, man, type="l")
lines(exp(reg4$fitted.values), col="purple") # 로그변환된 모형 또한 추세를 잘 나타내는 것으로 확인 가능


# 잔차제곱합으로 모형 비교
sse1 = sum(reg1$residuals^2) # reg1의 잔차제곱합
sse2 = sum(reg2$residuals^2) # reg2의 잔차제곱합
sse3 = sum((man-exp(reg4$fitted.values))^2) # reg4의 잔차제곱합
c(sse1, sse2, sse3) # 3차 회귀분석이 가장 적절한 것으로 판단됨


# 예측오차로 모형비교(2005년 1월부터 2020년 12월을 train데이터, 2021년 1월부터 2022년 12월을 test데이터로 두어 test데이터에 대한 예측오차 비교)
train_data = window(man, start=c(2005,1), end=c(2020,12))
test_data = window(man, start=c(2021,1), end=c(2022,12))

# 2차 회귀분석
regA = lm(man~tt+I(tt^2))
predA = as.data.frame(test_data) - predict(regA, newdata =test_data) # A모형을 사용했을 때 test 데이터에 대한 예측오차 계산

# 3차 회귀분석
regB = lm(man~tt+I(tt^2)+I(tt^3))
predB = as.data.frame(test_data) - predict(regB, newdata =test_data) # B모형을 사용했을 때 test 데이터에 대한 예측오차 계산

# 로그변환(3차 회귀분석)
regC = lm(log(man)~tt+I(tt^2)+I(tt^3))
predC = as.data.frame(test_data) - exp(predict(regC, newdata =test_data)) # C모형을 사용했을 때 test 데이터에 대한 예측오차 계산


# 모형 비교
pred.err = cbind(predA, predB, predC)
acc = colMeans(pred.err^2)
acc # 잔차제곱합으로 비교하면 3차 모형이 더욱 적합해 보였으나, 예측오차로 비교해보니 2차 모형만 사용하는 것이 더욱 효율적이라는 결과 도출


# 2차 모형과 로그변환한 2차모형에 대한 예측오차 비교
regD = lm(log(man)~tt+I(tt^2))
predD = as.data.frame(test_data) - exp(predict(regD, newdata =test_data)) # D모형을 사용했을 때 test 데이터에 대한 예측오차 계산

pred.err = cbind(predA, predD)
bdd = colMeans(pred.err^2)
bdd # 로그변환 한 것보다 그냥 2차모형만 사용한 것이 예측오차가 더 작은 것으로 확인가능

# 따라서 20대 남성에 대해서는 주어진 모형을 어느정도 잘 설명하고 있고, 예측 또한 매우 우수한
# 2차 모형을 사용하는 것이 가장 좋은 모형이라고 판단 할 수 있음




# --------------------- 여성
# 2차 회귀분석
tt = 1:length(woman) # 설명변수
reg1 = lm(woman~tt+I(tt^2))
summary(reg1) # 독립변수 모두 유의
plot(tt, woman, type="l") # 그냥 plot으로 그리면 reg1 직선을 못그림
lines(reg1$fitted.values, col="blue") 


# 3차 회귀분석
reg2 = lm(woman~tt+I(tt^2)+I(tt^3))
summary(reg2) # 독립변수 모두 유의
lines(reg2$fitted.values, col="red") 


# 4차 회귀분석
reg3 = lm(woman~tt+I(tt^2)+I(tt^3)+I(tt^4))
summary(reg3) # 이차항 독립변수가 유의수준 0.05하에서 귀무가설 기각하지 못함 -> 4차 모형은 부적합
lines(reg3$fitted.values, col="green") 


# 로그변환(3차 회귀분석)
plot(log(woman))
reg4 = lm(log(woman)~tt+I(tt^2)+I(tt^3))
plot(tt, woman, type="l")
lines(exp(reg4$fitted.values), col="red")


# 잔차제곱합 비교
sse1 = sum(reg1$residuals^2) # reg1의 잔차제곱합
sse2 = sum(reg2$residuals^2) # reg2의 잔차제곱합
sse3 = sum((woman-exp(reg4$fitted.values))^2) # reg4의 잔차제곱합
c(sse1, sse2, sse3) # 3차 회귀분석이 가장 적절한 것으로 판단됨


# 예측오차로 모형비교(2005년 1월부터 2020년 12월을 train데이터, 2021년 1월부터 2022년 12월을 test데이터로 두어 test데이터에 대한 예측오차 비교)
train_data = window(woman, start=c(2005,1), end=c(2020,12))
test_data = window(woman, start=c(2021,1), end=c(2022,12))

# 2차 회귀분석
regA = lm(woman~tt+I(tt^2))
predA = as.data.frame(test_data) - predict(regA, newdata =test_data) # A모형을 사용했을 때 test 데이터에 대한 예측오차 계산

# 3차 회귀분석
regB = lm(woman~tt+I(tt^2)+I(tt^3))
predB = as.data.frame(test_data) - predict(regB, newdata =test_data) # B모형을 사용했을 때 test 데이터에 대한 예측오차 계산

# 로그변환(3차 회귀분석)
regC = lm(log(woman)~tt+I(tt^2)+I(tt^3))
predC = as.data.frame(test_data) - exp(predict(regC, newdata =test_data)) # C모형을 사용했을 때 test 데이터에 대한 예측오차 계산


# 모형 비교
pred.err = cbind(predA, predB, predC)
acc = colMeans(pred.err^2)
acc # 남성과 동일하게 잔차제곱합으로 비교하면 3차 모형이 더욱 적합해 보였으나, 예측오차로 비교해보니 2차 모형만 사용하는 것이 더욱 효율적이라는 결과 도출


# 2차 모형과 로그변환한 2차모형에 대한 예측오차 비교
regD = lm(log(woman)~tt+I(tt^2))
predD = as.data.frame(test_data) - exp(predict(regD, newdata =test_data)) # D모형을 사용했을 때 test 데이터에 대한 예측오차 계산

pred.err = cbind(predA, predD)
bdd = colMeans(pred.err^2)
bdd # 로그변환 한 것보다 그냥 2차모형만 사용한 것이 예측오차가 더 작은 것으로 확인가능

# 따라서 20대 여성에 대해서도 마찬가지로 주어진 모형을 어느정도 잘 설명하고 있고, 예측 또한 매우 우수한 2차 모형을 사용하는 것이 가장 좋은 모형이라고 판단 할 수 있음








#### 정상성 확인(남성)
plot(man) # 그림을 보았을 때 비정상성으로 판단
plot(diff(man)) # 정상성이라고 볼수 있음


# 정상성을 판단하기 위해 시계열 그림을 보고 하는 것과, SACF를 계산해서 시각적으로 판단 -> 주관적인 요소가 들어가기 때문에 단위근 검정이라는 객관적인 평가 요소를 확인해봄 
adf.test(man) # 귀무가설을 기각할 수 없으므로 비정상시계열이라고 판단 가능
adf.test(diff(man)) # 1차 차분한 결과 귀무가설을 기각 -> 단위근이 존재하지 않으므로 정상시계열이라고 판단 가능

# kpss.test로 확인
kpss.test(man, null="Trend", lshort=T) # 비정상 시계열
kpss.test(man, null="Level", lshort=T) # 비정상 시계열

kpss.test(diff(man), null="Trend", lshort=T) # 정상 시계열
kpss.test(diff(man), null="Level", lshort=T) # 정상 시계열


# seasonal이 좋을지 non-seasonal이 좋을지 ACF와 PACF 그래프 그려서 확인
par(mfrow=c(2,1))
acf(diff(man), lag.max=50) 
pacf(diff(man), lag.max=50)
# ACF, PACF 모두 중간 값은 0으로 보이고 lag 1, 2, 3, 4인 부분이 튀어나와 있는 것으로 보아 seasonal 모형이라고 생각 가능 + ACF에서 lag 0.6, 1.6, 2.6 부분과 PACF 0.6 인 부분에도 튀어나와 있는 것으로 보아 ARIMA 모형과 seasonal ARIMA 모형을 곱하기로 연결한 승법모형을 사용하는 것이 좋을 것이라고 판단 


# 계절 차분된 것이 몇 번 차분이 이루어져야 하는지 단위근 검정을 통해 확인
dev.off()

plot(diff(man, lag=1, differences=1))
plot(diff(man, lag=12, differences=1))

plot(diff(diff(man, lag=12, differences=1), lag=1, differences=1))

kpss.test(diff(man, lag=12, differences=1))
kpss.test(diff(diff(man, lag=12, differences=1), lag=1, differences=1))


x = diff(diff(man, lag=12, differences=1), lag=1, differences=1)
auto.arima(x)
arima(x, order=c(1,1,0), seasonal=list(order=c(1,0,0)))

# 그냥 차분한 것 사용
x = diff(man, lag=1, differences=1)
plot(x)

par(mfrow=c(2,1))
acf(x, lag.max=50)
pacf(x, lag.max=50)


# arima 모형 적합
arima(man, order=c(1,1,0), seasonal=list(order=c(1,0,0))) # AR(1) 모형
arima(man, order=c(2,1,0), seasonal=list(order=c(2,0,0))) # AR(2) 모형
arima(man, order=c(12,1,0))

Arima(man,order=c(2,1,0),seasonal=c(2,0,0),include.constant = T)

tsdiag(Arima(man,order=c(1,1,0),seasonal=c(2,0,0),include.constant = T))
tsdiag(Arima(man,order=c(2,1,0),seasonal=c(2,0,0),include.constant = T))
tsdiag(Arima(man,order=c(12,1,0),include.constant = T))
# 정해야 할 것 -> non-seasonal part와 seasonal part 모형 어떻게 정하는지


# 어떤 모형이 적절한지 판단하기 위해 모형진단 실시
tsdiag(arima(man, order=c(1,1,0), seasonal=list(order=c(1,0,0)))) # 불가능
tsdiag(arima(man, order=c(2,1,0), seasonal=list(order=c(2,0,0)))) # 가능
tsdiag(arima(man, order=c(12,1,0)))
tsdiag(arima(man, order=c(24,1,0)))


auto.arima(man)

# 예측
result = arima(man, order=c(2,1,0), seasonal=list(order=c(2,0,0)))
forecast(result, h=10)
plot(forecast(result,h=36))

result2 = arima(man, order=c(12,1,0))
forecast(result2, h=10)
plot(forecast(result2,h=36))

216-36
# 두 모형 예측 오차 계산 
x = man[1:180] # 맨 끝의 36개 데이터 제외
x = ts(x,frequency = 12, start=2005) # 다시 time-series로 변환해줘야함 

y = man[-(1:180)]

result_mean = forecast(result, h=36)$mean
result_mean2 = forecast(result2, h=36)$mean

a = y-result_mean # 예측오차
b = y-result_mean2
y

cbind(a,b)
sum((a)^2) 
sum((b)^2)
cbind(result_mean,a,result_mean2,b,y)



#### 정상성 확인(여성)
plot(woman) # 그림을 보았을 때 비정상성으로 판단
plot(diff(woman)) # 정상성이라고 볼수 있음


# 정상성을 판단하기 위해 시계열 그림을 보고 하는 것과, SACF를 계산해서 시각적으로 판단 -> 
# 주관적인 요소가 들어가기 때문에 단위근 검정이라는 객관적인 평가 요소를 확인해봄 
adf.test(woman) # 귀무가설을 기각할 수 없으므로 비정상시계열이라고 판단 가능
adf.test(diff(woman)) # 1차 차분한 결과 귀무가설을 기각 -> 단위근이 존재하지 않으므로 정상시계열이라고 판단 가능

# kpss.test로 확인
kpss.test(woman, null="Trend", lshort=T) # 비정상 시계열
kpss.test(woman, null="Level", lshort=T) # 비정상 시계열

kpss.test(diff(woman), null="Trend", lshort=T) # 정상 시계열
kpss.test(diff(woman), null="Level", lshort=T) # 정상 시계열


# seasonal이 좋을지 non-seasonal이 좋을지 ACF와 PACF 그래프 그려서 확인
par(mfrow=c(2,1))
acf(diff(woman), lag.max=100) 
pacf(diff(woman), lag.max=100)
# 승법 모형 사용


# 계절 차분된 것이 몇 번 차분이 이루어져야 하는지 단위근 검정을 통해 확인

plot(diff(woman, lag=1, differences=1))
plot(diff(diff(woman, lag=12, differences=1), lag=1, differences=1))

kpss.test(diff(woman, lag=1, differences=1))
kpss.test(diff(diff(woman, lag=12, differences=1), lag=1, differences=1))


# 그냥 차분한 것 사용
dev.off()
x = diff(woman, lag=1, differences=1)
plot(x)

par(mfrow=c(2,1))
acf(x, lag.max=50)
pacf(x, lag.max=50)


# arima 모형 적합
arima(woman, order=c(1,1,0), seasonal=list(order=c(1,0,0))) # AR(1) 모형(seasonal + non seasonal)
arima(woman, order=c(12,1,0)) # AR(12) 모형

Arima(woman, order=c(1,1,0),seasonal=list(order=c(1,0,0)),method=c("ML"))
Arima(woman, order=c(12,1,0),method=c("ML"))

# 정해야 할 것 -> non-seasonal part와 seasonal part 모형 어떻게 정하는지


# 어떤 모형이 적절한지 판단하기 위해 모형진단 실시
tsdiag(arima(woman, order=c(1,1,0), seasonal=list(order=c(1,0,0)))) # 가능
tsdiag(arima(woman, order=c(12,1,0)))


auto.arima(woman)

# 예측
result_w = arima(woman, order=c(1,1,0), seasonal=list(order=c(1,0,0)))
forecast(result_w, h=10)
plot(forecast(result_w,h=36))

result_w1 = arima(woman, order=c(12,1,0))
plot(forecast(result_w1,h=36))

# 두 모형 예측 오차 계산 
length(woman)
x = woman[1:180] # 맨 끝의 36개 데이터 제외
x = ts(x,frequency = 12, start=2005) # 다시 time-series로 변환해줘야함 

y = woman[-(1:180)]

resultw_mean = forecast(result_w, h=36)$mean
resultw_mean2 = forecast(result_w1, h=36)$mean

a = y-result_mean # 예측오차
b = y-result_mean2
y

cbind(a,b)
sum((a)^2) 
sum((b)^2)
cbind(result_mean,a,result_mean2,b,y)

max(resultw_mean)
