'''10주차 대면 (다중 회귀)'''

supervisor=read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P060.txt", header=TRUE, sep="\t")
supervisor

out=lm(Y~X1+X2+X3+X4+X5+X6, data=supervisor)
out #bhat에 대한 회귀식 작성 가능
#다른표현
lm(Y~., data=supervisor)


summary(out)
#coefficients 부분 estimate(추정값), std.error(표준오차, mse로 대체), tvalue, 유의확률
# tvalue는 추정값/표준오차 
#residual standard error(mse제곱안한꼴?), 자유도, 다중 결정계수, 수정 결정계수
#F-statistic: (msr/mse)=10.5 on 6(ssr 자유도) and 23(잔차자유도) DF -> (F(6,23))

#필요없는 변수 제거하고 출력해보기
out1=lm(Y~X1+X2+X3+X4+X6, data=supervisor)
out1
summary(out1)
#신뢰구간
confint(out1) #디폴트 유의수준 5
confint(out1, level=0.90) #레벨로 유의수준조절

#예측
predict(out, newdata=data.frame(X1=1, X2=1, X3=1, X4=1, X5=1, X6=1))
predict
#분산분석표, 이걸로 보면 어려우니까 수업때 한것처럼하는걸 다음주에 배움 
anova(out)

'''11주차 대면 (아노바, 예측)'''

#anova
out0=lm(Y~1, data=supervisor) #상수항만 들어있음
out
out0
anova(out0,out)
#res.DF(잔차들의 자유도)
#즉, OUT0의 자유도는 N-1, OUT의 자유도는 N-P-1, 
#RSS는 잔차제곱합(수업에서 배운 SSE랑 같음)
#DF는 29-23, SUM OF SQ는 RSS처음꺼에서 두번째꺼 뺀거 
#잔차제곱합의 차이는 추가제곱합이라는 용어, (늘어난 설명력, SSR로 설명할 수 있는 부분 증가)
# (추가제곱합/6)/(sse/23) = 추가제곱합평균 즉, 10.502가 됨(???)

anova(out)
#각 x1~x6까지 F는 어캐구했냐면 sum sq에 잔차의 mse(49.96)로 나눈것
#sse가 1149, mse가 49.96

#순차제곱합을 확인할 때
out1=lm(Y~X1, data=supervisor) #이렇게 쓸 필요없이 update 사용
out1=update(out0,.~.+X1) #변수 하나 정도만 바꾸려면 update 사용 가능 
out2=update(out1,.~.+X2)
out3=update(out2,.~.+X3)
out4=update(out3,.~.+X4)
out5=update(out4,.~.+X5)
out6=update(out5,.~.+X6)

anova(out0,out1) #여기에서의 F값, 유의확률은 풀모델에서 나온 잔차를 사용
anova(out0, out) #에서의 sum of sq가 순차제곱합에서의 늘어난 설명력의 총합임
anova(out) #즉, 여기에서의 sum sq의 총합임

out.R=lm(Y~X1+X3, data=supervisor)
out.R
out
summary(out)$r.squared
summary(out.R)$r.squared #결정계수는 변수가 많아지면(x1보다 x1+x2일때~) 무조건 늘어남 
#근데 2개 변수가지고 있을 때, 0.70, 풀모델일 때 0.73인데 과연 복잡한 모델 필요한가?->검정을 통해 유의성 확인
anova(out.R, out)
#유의확률이 존내 크기 때문에 H0 기각 

#예측
X1=1;X2=1;X3=1;X4=1;X5=1;X6=1 #여섯개의 독립변수가 모두 1인 예제 
new = data.frame(X1, X2, X3, X4, X5, X6)
predict(out, newdata=new) #모든 변수에 1을 대입했을 때, 회귀식 예측값
predict(out, newdata=new, interval="prediction") #모든 값이 1일 때, 95% 예측구간이 -16..~39..

X1=c(1,2);X2=c(1,2);X3=c(1,2);X4=c(1,2);X5=c(1,2);X6=c(1,2) #여러 개를 예측하고 싶을 때
new = data.frame(X1, X2, X3, X4, X5, X6)
new
predict(out, newdata=new, interval="prediction") 

#행렬 표현
matrix(1:12, nrow=2, ncol=6)
matrix(1:12, nrow=2, ncol=6, byrow=T) #행부터 입력(1행부터 123456하고 2행에 789~)
data.frame(matrix(1:12, nrow=2, ncol=6, byrow=T))
predict(out, newdata=new, interval="prediction") 

#컬럼명 바꾸기
colnames(supervisor) <-c("Y", "A", "B", "C", "D", "E", "F")
head(supervisor)
out = lm(Y~., data=supervisor)
out
predict(out, newdata=new, interval="prediction")
#에러 뜨는 이유는 변수 이름 바꿔서 new데이터에 바꾼 컬럼명을 지정안해줘서
colnames(new)<- colnames(supervisor)[-1]
colnames(new)<- c( "A", "B", "C", "D", "E", "F") #위 또는 이 방법으로 컬럼명 변경
head(new)
predict(out, newdata=new, interval="prediction") #newdata란 회귀식에서 x1,x2 ...에 해당하는 값들 
