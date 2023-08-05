repair = read.table(file="D:/경기대/3학년_1학기/회귀1/P031.txt", header=TRUE, sep="\t")
?read.table
repair
is.data.frame(repair) #repair가 data입니까?
is.matrix(repair) #repair가 matrix입니까?

#solution1
attach(repair)
plot(Units, Minutes)
detach(repair)

#solution2
with(data=repair, plot(Units, Minutes))

#solution3
x = repair$Units
y = repair$Minutes
plot(x,y)
plot(repair) #첫 번째 열은 세로줄, 두 번째는 가로줄, 세 개 이상 데이터는 모든 산점도 표현

#표본상관계수
cor(x,y)
cor(repair)
?cor.test()
cor.test(x,y) #상관성 검정

####2주차 과제
marry = read.table(file="D:/경기대/3학년_1학기/회귀1/All_Data/All_Data/P052.txt", head=TRUE, sep="\t")
marry

#미터와 센티미터로 전처리
x = marry$Husband
y = marry$Wife
plot(x, y)

x1 = marry$Husband/100
y1 = marry$Wife/100
plot(x1, y1)

#남편과 아내 키 사이의 공분산(센티미터)
cov(x, y)
#남편과 아내 키 사이의 공분산(미터)
cov(x1, y1)

#남편과 아내 키 사이의 표본상관계수(센티미터)
cor(x, y)
#남편과 아내 키 사이의 표본상관계수(미터)
cor(x1, y1)

#모든 남편과 아내의 키차이가 5센치일 경우의 표본상관계수
x2 = marry$Husband
y2 = marry$Husband-5
y2
cor(x2, y2)
