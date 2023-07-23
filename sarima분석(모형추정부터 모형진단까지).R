AirPassengers # 주기가 월단위 (12)

# 1. 모형 추정 

plot(AirPassengers)
# 앞부분이 진폭이 작다가 뒤로갈수록 커짐 -> 분산안정화 필요
# 로그변환해줌
plot(log(AirPassengers))

# 차분을 이용해 평균 정상화
plot(diff(log(AirPassengers), lag = 1, differences = 1))
# 계절 성분이 여전히 남아있는듯함 

par(mfrow=c(2,1))
plot(diff(log(AirPassengers), lag = 12, differences = 1)) 
# 주기를 12로 하면 계절차분됨
# 여전히 확률적 추세가 존재하는 것처럼 보임 -> 차분 한번 더
plot(diff(diff(log(AirPassengers), lag = 12, differences = 1),lag = 1, differences = 1))
# 정말로 계절차분한 데이터가 한번 더 차분이 필요한가? 확인
# 검정으로 확인: 단위근 검정(kpss.test, adf.test)
library(tseries)

# kpss.test 확인
kpss.test(diff(log(AirPassengers), lag = 12, differences = 1)) 
# p-value: 0.09 => ho not reject (alpha=0.05 하)
          
# adf.test 확인
adf.test(diff(log(AirPassengers), lag = 12, differences = 1))
# p-value: 0.09265 => h0 not rehect (alpha=0.05 하)

# 최종적으로 우리가 분석할 데이터 모형 = 두번 차분한 모형 
# (교수님은 한번 더 차분 필요하다고 판단하에)

x = diff(diff(log(AirPassengers), lag = 12, differences = 1),lag = 1, differences = 1)
# x는 두번 차분한 모형 (계절차분+차분) => 정상시계열 만듦

# 2. 모형 적합 

# 그 다음 할 일 ar,ma,arma,sarma 모형중 선택 

# pacf, acf로 확인 
acf(x);pacf(x)
# acf, pacf 둘다 중간 파트는 거의 0에 근사하고, 0.1부분에 0이 아닌 부분 존재, 
# 1일 때, 0이 아님 => 승법적(계절*non계절) ar 모형 적합이 적절해보임 

# 승법적(계절*non계절) ar 모형 적합
arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12)) #period는 자동으로 주기 정해줌
# ar1(non-seasonal파트 계수) / sar1(seasonal파트 계수) / intercept(mu)
# (1+0.3745B)(1+0.4637B^12)(x_t-0.0001) = a_t 
# 0.0001 너무 작으므로 mu=0이라 해도 무방 
# s.e보다 계수의 절댓값이 더 큼 -> 유의한 값 
# 아래 모형도 이런식으로 판단해볼 것  

# 승법적(계절*non계절) ma 모형 적합
arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1),period=12))
# x_t+2*10^-4 = (1-0.4021B)(1-0.5657B^12)*a_t


# 승법적(계절*non계절) arma 모형 적합
arima(x,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12))
# ar1      ma1     sar1     sma1  intercept 각각 어떤 추정값인지 확인! 

# 3. 모형 진단 (잔차들이 wn만족)
tsdiag(arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12)))
# 잔차가 다 wn 만족함 

tsdiag(arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1),period=12)))

# 진단했을 때, 교수님은 ar모형이 낫다더라
# 해석 편리: 왜? 과거의 값으로 항상 미래를 설명 
# 혹은 aic 등 정보함수 크기 비교 

# 차분안한 데이터 활용 (한번만 차분한 데이터)
arima(log(AirPassengers), order=c(0,1,1), seasonal=list(order=c(0,1,1)))
# order = c(ar계수,차분,ma계수)

arima(x,order=c(0,1,1),seasonal=list(order=c(0,1,1)), include.mean=FALSE)
# include.mean=FALSE -> 아까 MU 작게 나왔으니까 제외 

# 예측을 한다라면 
# 변환된 x = diff(diff(log(AirPassengers), lag = 12, differences = 1),lag = 1, differences = 1)
# 보다 log(AirPassengers), order=c(0,1,1), seasonal=list(order=c(0,1,1) 형태가 편하다!
# 근데 둘다 같이 차분했다는 같은 의미가 아닌지? 