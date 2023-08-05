salary.data <- read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P130.txt", header=TRUE, sep='\t')

#scatter plot
with(data=salary.data, plot(X,S))
reg0=lm(S~X, data=salary.data)
abline(reg0, col='blue')
par(mfrow=c(2,2))
plot(reg0)

par(mfrow=c(1,1))
with(data=salary.data, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1) #점색깔이름(legend)
abline(reg0)

#특정조건을 만족하도록 회귀분석
lm(S~X, data=salary.data, subset=(E==1 & M==0))
lm(S~X, data=salary.data, subset=(E==2 & M==0))
lm(S~X, data=salary.data, subset=(E==3 & M==0))
lm(S~X, data=salary.data, subset=(E==1 & M==1))
lm(S~X, data=salary.data, subset=(E==2 & M==1))
lm(S~X, data=salary.data, subset=(E==3 & M==1))

#model1
reg1 <- lm(S~., data=salary.data)
reg1

with(data=salary.data, plot(X,S, col=E+M*3))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1) #점색깔이름(legend)

for(Elevel in 1:3){
  for(Mlevel in 0:1){
    b0 <- reg1$coefficients[1]+Elevel*reg1$coefficients[3]+Mlevel*reg1$coefficients[4]
    b1 <- reg1$coefficients[2]
    abline(b0,b1,col=Elevel+3*Mlevel)
  }
}
plot(reg1, which=1, col=salary.data$E+3*salary.data$M)

#model2: E->더미변수(지시변수) 2개 사용
E1 <- salary.data$E==1
E2 <- salary.data$E==2
reg2 <- lm(S~X+E1+E2+M, data=salary.data)

with(data=salary.data, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1)

for(Elevel in 1:3){
  for(Mlevel in 0:1){
    if(Elevel==1){E1level=1; E2level=0}
    if(Elevel==2){E1level=0; E2level=1}
    if(Elevel==3){E1level=0; E2level=0}
    b0 <- reg2$coefficients[1]+E1level*reg2$coefficients[3]+E2level*reg2$coefficients[4]+Mlevel*reg2$coefficients[5]
    b1 <- reg2$coefficients[2]
    abline(b0,b1,col=Elevel+3*Mlevel)
  }
}

plot(reg2, which=1, col=salary.data$E+3*salary.data$M)

#model3: 상호작용항
reg3 <- lm(S~X+E1+E2+M+E1*M+E2*M, data=salary.data)
reg3

with(data=salary.data, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1)

for(Elevel in 1:3){
  for(Mlevel in 0:1){
    if(Elevel==1){E1level=1; E2level=0}
    if(Elevel==2){E1level=0; E2level=1}
    if(Elevel==3){E1level=0; E2level=0}
    b0 <- reg3$coefficients[1]+E1level*reg3$coefficients[3]+E2level*reg3$coefficients[4]+Mlevel*reg3$coefficients[5]+E1level*Mlevel*reg3$coefficients[6]+E2level*Mlevel*reg3$coefficients[7]
    b1 <- reg3$coefficients[2]
    abline(b0,b1,col=Elevel+3*Mlevel)
  }
}
plot(reg3, which=1, col=salary.data$E+3*salary.data$M)

#33이라는 이상치 뺴고 진단
salary_33 <- cbind(salary.data, E1, E2)[-33,]
reg4 <- lm(S~X+E1+E2+M+E1*M+E2*M, data=salary_33)

with(data=salary_33, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1)

for(Elevel in 1:3){
  for(Mlevel in 0:1){
    if(Elevel==1){E1level=1; E2level=0}
    if(Elevel==2){E1level=0; E2level=1}
    if(Elevel==3){E1level=0; E2level=0}
    b0 <- reg4$coefficients[1]+E1level*reg4$coefficients[3]+E2level*reg4$coefficients[4]+Mlevel*reg4$coefficients[5]+E1level*Mlevel*reg4$coefficients[6]+E2level*Mlevel*reg4$coefficients[7]
    b1 <- reg4$coefficients[2]
    abline(b0,b1,col=Elevel+3*Mlevel)
  }
}
plot(reg4, which=1, col=salary_33$E+3*salary_33$M)

## factor() 뭐냐면 질적변수로 인식시키는거

EF <- factor(salary.data$E)
MF <- factor(salary.data$M)
lm(S~X+EF+MF+EF*MF, data=salary.data)

E3 <- salary.data$E==3
M0 <- salary.data$M==0

lm(S~X+E3+E2+M+E3*M+E2*M, data=salary.data)