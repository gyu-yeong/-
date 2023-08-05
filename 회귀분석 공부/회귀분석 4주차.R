#회귀분석 4주차
repair = read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P031.txt", head = TRUE, sep="\t") 
repair
x=
#lm함수는 lm(y~x) (종속변수~독립변수) 형태로 표현해야 함, 이는 yi=b0+b1xi+ei 임을 의미 
#독립변수가 두개라면 y~x1+x2꼴
# y~0+x 꼴이면 인터셉트가 없는 꼴. 원점을 지나는 형태 

lm(Minutes~Units, data=repair)
lm(Minutes~Units, data = repair)
#y_hat = 4.162+15.509x_hat 결과 산출 
out =lm(Minutes~Units, data = repair)
out$coefficients
#coefficients 데이터의 개수가 궁금하다면
length(out$coefficients)
#intercept만 궁금하다면
out$coefficients[1]

#각 잔차가 궁금하다면 
out$residuals
out$fitted.values

#각각의 적합값(fitted.values) y=b0+b1x꼴을 다르게 표현 
out$coefficients[1]+out$coefficients[2]*repair$Units

#각각의 잔차(residuals)를 다르게 표현
repair$Minutes - out$fitted.values
sum(out$residuals) #잔차의 합은 0이지만 r에서는 근사치로 계산나와서 0으로 출력안됨

#각각의 독립변수 x 잔차를 다르게 표현
repair$Units*out$residuals
sum(repair$Units*out$residuals) #독립변수x잔차의 합 역시 0 (잔차의 성질 sum(xi * ei)=0 확인) 

#자유도
out$df.residual

#잔차제곱합SSE, 총제곱합SST, 회귀(적합값)제곱합SSR
sse = sum(out$residuals^2) #잔차들이 퍼져있는 정도 
sst = sum((repair$Minutes-mean(repair$Minutes))^2)   
ssr = sum((out$fitted.values-mean(repair$Minutes))^2) #적합값의 퍼져있는 정도 
sst
sse+ssr
mean(repair$Minutes) #종속변수의 평균 = 적합값의 평균 
mean(out$fitted.values) #적합값의 평균 

#결정계수
r2 = ssr/sst
r2

cov(out$residuals, repair$Units) #잔차와 독립변수의 공분산은 0 = 상관관계X 
cor(out$residuals, repair$Units) #상관계수도 마찬가지
cor(out$residuals, out$fitted.values) #잔차와 적합값의 상관계수 = 0 
plot(out$residuals, repair$Units) #산점도 상에서 확인 가능 
