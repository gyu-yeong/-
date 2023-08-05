########################
##### Salary data ######
########################

salary.data <- read.table("D:/O Na/2021-1/Regression/All_Data/P130.txt", header=TRUE, sep="\t")
head(salary.data)

## scatter plot

with(data=salary.data, plot(X,S))
with(data=salary.data, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1)


## Model1

reg1 <- lm(S~., data=salary.data)
reg1

with(data=salary.data, plot(X,S, col=E+3*M))
legend("bottomright", c("(E,M)","(1,0)","(2,0)","(3,0)","(1,1)","(2,1)","(3,1)"), col=0:6, pch=1)

for(Elevel in 1:3){
  for(Mlevel in 0:1){
    b0 <- reg1$coefficients[1]+Elevel*reg1$coefficients[3]+Mlevel*reg1$coefficients[4]
    b1 <- reg1$coefficients[2]
    abline(b0,b1,col=Elevel+3*Mlevel)
  }
}
plot(reg1, which=1, col=salary.data$E+3*salary.data$M)


## Model2 : E -> dummy variable

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



## Model3 : interaction term

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


## Model3 : 33 -> delete

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


## factor()

EF <- factor(salary.data$E)
MF <- factor(salary.data$M)
lm(S~X+EF+MF+EF*MF, data=salary.data) #ef2는 교육수준이 두번째, ef3는 세번째, 회귀계수 작은순서대로 나열됨
reg3 #얘랑 비교해보기기, 위에랑 밑에가 가변수가 달라서 회귀계수들 다르게 나옴 
E3 <- salary.data$E==3
M0 <- salary.data$M==0

lm(S~X+E3+E2+M+E3*M+E2*M, data=salary.data)


#######################
###### Test data ######
#######################

test.data <- read.table("D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P140.txt", header=TRUE, sep="\t")
test.data

X <- test.data$TEST
Y <- test.data$JPERF
Z <- test.data$RACE

plot(X,Y, col=2*(Z+1))
legend("topleft", c("RACE=1", "RACE=0"), col=c(4,2), pch=1)
abline(lm(Y~X, subset=(Z==0)), col="red") #백인종
abline(lm(Y~X, subset=(Z==1)), col="blue") #소수민족
abline(lm(Y~X), col="black") #인종구분x

plot(lm(Y~X), which=1, col=2*(Z+1))
lm(Y~X+Z+X*Z) #X*Z(인종별 기울기의 차)는 기울기도 바꿔줌, Z(y절편)는 Y절편바꿔줌

#인종에 따라 교육수준이 다르게 나타나는데 그게 유의한지 검정 
reg.race <- lm(Y~X+Z+X*Z)
reg.total <- lm(Y~X)
anova(reg.total, reg.race)
