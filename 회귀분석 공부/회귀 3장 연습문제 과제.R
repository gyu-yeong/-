#과제 3-3 
exam=read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P083.txt", header=TRUE, sep="\t")
exam
model1=lm(F~P1, data=exam)
model1
model2=lm(F~P2, data=exam)
model2
model3=lm(F~P1+P2, data=exam)
model3
summary(model1)
#model1에서의 결정계수는 0.8023, 회귀식은 F=-22.3424+1.2605P1
summary(model2)
#model2에서의 결정계수는 0.86, 회귀식은 F=-.1853+1P2
summary(model3)
#modle3에서의 결정계수는 0.8863, 회귀식은 F=-14.5+0.4883P1+0.672P2
P1=78;P2=85
new = data.frame(P1, P2)
predict1=predict(model1, newdata=new)
predict1
predict2=predict(model2, newdata=new)
predict2
predict3=predict(model3, newdata=new)
predict3

#과제 3-13
#유의확률 구하기
qf(0.95, 4, 88) #f값 임곗값
qf(0.05, 4, 88, lower.tail=FALSE) #위 수랑 똑같은 값 
pf(22.98, 4, 88, lower.tail=FALSE) #누적확률에서 왼쪽넓이 계산이 기본값, false로 두면 오른쪽 넓이
1-pf(22.98, 4, 88) #위 값이랑 같음 (유의확률)

#과제 3-15
siga=read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P088.txt", head=TRUE, sep='\t')
siga
siga=siga[,c(2:8)]
head(siga)
colnames(siga) <-c('X1','X2','X3','X4','X5','X6','Y')
head(siga)
model1=lm(Y~., data=siga) #풀모델
model1
summary(model1)
confint(model1)
model2=lm(Y~X1+X2+X3+X4+X6, data=siga) #Female를 제외한 모델
anova(model2, model1)
model3=lm(Y~X3, data=siga) #Income만 사용한 모델
summary(model3)
confint(model3, level=0.95) #Income만 사용한 모델의 95% 신뢰구간
model4=lm(Y~X1+X3+X6, data=siga) #Price, Age, Income를 적합한 모델
summary(model4)
anova(model1, model3)
model5=lm(Y~X1+X2+X4+X5+X6, data=siga) #Income을 제외한 모델델
summary(model5)
anova(model5)
model6=lm(Y~X1+X3+X4+X6, data=siga) #Female, HS를 제외한 모델
anova(model6, model1)
confint(model6)
confint(model2)

plot(siga)
#검정 주에서 x1, x3가 필요한지 아닌지 검정
#-> 이 때, summary를 사용할 때 t-test는 풀 모델에서 각각의 x1, x3가 유의한지 아닌지를 검정해줌