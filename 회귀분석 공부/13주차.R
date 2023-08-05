data=(, header=TRUE, sep='\t')
#가정을 하면 안된다고 하는 뚜렷한 증거가 존재하는지 
n=1000
e=rnorm(n, 0, 1)
e
plot(e) #
abline(h=0, lty=2)

#실습
cars
plot(cars)
out=lm(dist~speed, data=cars)
out
plot(out) #잔차 분석
#첫번째 그림: 적합값과 잔차의 산점도 
#빨간색 참조선은 국소회귀선(데이터를 윈도우 내의 작은집단에 대해 회귀분석을 돌려서 가운데 적합값 계산)
#b0+b1speed 에서 speed가 커질수록 잔차의 진폭이 커짐

plot(cars)
par(mfrow=c(2,2))
plot(out)
#Q-Q플롯은 분위수에 대한 것 가로축의 분위수를 구할 때 NOMAL(정규분포) 이용해서 구함 
#Q-Q플롯은 내가 가지고 있는 자료가 내가 가정한? 분포를 따르느냐 
#세번째 그림: 적합값과 표준화한 잔차의 절댓값에 루트: Y값이 커질수록 진폭이 커진다
#근데 그게 별로 크지 않으니 일반적인 OLS 기법으로 단순선형회귀해도 될듯이라느 추론 가능
#네 번째 그림: 영향점분석 (독립변수 입장에선 고지렛점, 종속변수 입장에선 동떨어진 값)
#잔차의 절댓값이 크면 Y입장에서 특잇값이 될 것이다. 
#영향값: 해당 값을 뺐을 때 심각하게 변화가 있으면 그 점은 영향점임 
#COOK의 거리: 잔차의 제곱과 레버리지 둘 다 가지고 있어서 그게 작으면 영향점이 작은거임

plot(cars$speed, out$resid)
plot(out$resid)

?avPlot
influence(out)
#hat(레버리지값)
#coef(n번째 데이터를 넣고 회귀분석을 돌릴 때 나오는 차잇값)
#즉, (전체 데이터를 넣고 돌린 회귀계수 - n번째 빼고 돌린 회귀계수값)
out$coefficient
out1=lm(dist~speed, data=cars[-1,]) #첫 번째 행을 빼고 돌린 회귀분석 
out1$coeff
#sigma는 (n번쨰 데이터를 넣고 돌릴 때 나온느 rse의 차잇값)
#즉, 전체 데이터를 넣고 돌린 rse - n번째 빼고 돌린 rse)
summary(out1)
summary(out)
#wt.res는 안함 

influence.measures(out) #표준화해서 출력 
#*표는 영향점 판별 '

supervisor
library(car)
car::avPlots(fit,ask=FALSE,id.method="identify")
car:: avPlots(reg)
e.Y=lm(Y~X2+X3+X4+X5+X6. data=supervisor)$resid #y를 x2~x6로 조정
e.1=lm(X1~X2+X3+X4+X5+X6. data=supervisor)$resid #x1을 x2~x6로 조정 
lm(e.Y~e.1)
#e.1의 회귀계수랑 reg$coeff의 x1과 동일
#인터셉트의 7.9e-16은 사실상 0
lm(e.Y~e.1-1) #-1은 인터셉트가 없다는 말 
library()
install.packages("car")
