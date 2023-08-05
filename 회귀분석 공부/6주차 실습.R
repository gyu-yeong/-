repair = read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P031.txt", header=TRUE, sep='\t')
repair

x=repair$Units
y=repair$Minutes

?summary
reg=lm(y~x)
summary(reg)
#sigma는 mse임 (summary에서 사용도는)
#b0hat에 대한 표준오차(s.e(b0hat))는 3.355(error부분)
#스튜던트화: b0hat-0/s.e(b0hat) = t value
#Pr(유의확률): Ph0(|T| > 1.24)=0.239 유의확률이 크면 H0지지, 작으면 H1지지 
#residual standard error은 루트mse, degrees of freedom은 잔차 자유도


confint(reg)
?confint #95%신뢰구간 확인법 b0에 대한 신뢰구간은 인터셉트줄, b1은 x줄
#단측검정의 유의확률은 2로 나눠야 함. 
#만약 p0.05면 양측은 p0.025로 하고 단측이면 p0.05로 하면 
#해당 모형의 관계식을 통해 설명할 수 있는 정도가 결정계수
#수정된 결정계수는 나중에 다중회귀할 때,
#F통계량 (회귀모형의 유의성 검정), 자유도, 유의확률

anova(reg)
#F값이 크면클수록, P(V>F값)의 확률 Pr(>F)유의확률이 작으면작을수록 h1 지지

#상수모형, 기본모형 각각 분산분석에 적합시켜서 테이블 확인하기
reg1 = lm(y~1)
reg2 = lm(y~x)
anova(reg1, reg2) #두 개의 회귀분석 결과를 동시에 입력 가능
#RSS는 잔차제곱합(SSE), 모델1의 설명 못하는 부분은 그냥 REG분산분석 시 설명못하는 부분의 합
#F값은 늘어난 설명력이 충분히 의미가 없는지 판단하게 하는 통계량 
#sum fo sq는 모델1 rss - 모델2 rss
anova(reg2, reg1)
#순서가 바뀌어도 검정 결과 자체는 동일 
#단순선형회귀는 하나의 reg만 사용, 다중회귀는 reg 두개 사용.

#예측
predict(reg, newdata=data.frame(x=10)) #newdata는 독립변수로 데이터프레임형태로 입력해야함
reg
#x값 * data.frame(x=10)
predict(reg, newdata=data.frame(x=c(1,10,11,100,120)))
predict(reg, newdata=data.frame(x=10), se.fit=TRUE)                                
predict(reg, newdata=data.frame(x=10), interval= c("prediction"))

predict(reg, newdata=data.frame(x=c(10, 11, 100)), interval= "confidence", level=0.95)
predict(reg, newdata=data.frame(x=c(10, 11, 100)), interval= "prediction", level=0.95)

#interval은 예측값만 계산할건지, 예측구간도 예측할건지                                  
#interval = c("confidence", "prediction)
#confidence 평균반응에 대한 신뢰구간, 예측구간 
#prediction은 개별예측구간
#level = 0.95 신뢰도 