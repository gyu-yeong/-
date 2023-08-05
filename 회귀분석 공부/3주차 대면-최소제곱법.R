#최소제곱법 구하기 (2주차 실습)
repair = read.table(file="D:/경기대/3학년_1학기/회귀1/P031.txt", header=TRUE, sep="\t")
x = repair$Units
y = repair$Minutes
x=c(1,2,3,4,5,6,7,8,9)
x
y=c(110,130,150,170,160,180,140,130,140)

plot(x,y)
xbar = mean(x)
xbar
ybar = mean(y)
ybar
Sxx = sum((x-xbar)^2)
Sxx
Sxy = sum((x-xbar)*(y-ybar))
Sxy
####벡터로 구하기

b1 = Sxy/Sxx
b0 = ybar-(b1*xbar)
b0
b1
####행렬로 구하기
rep(1,length(x)) #자료의 개수
x.mat = cbind(rep(1,length(x)),x) #cbind는 세로줄로 출력하라
x.mat 
y
y.mat = solve(t(x.mat)%*%x.mat) #행렬의 곱은 %*%, 전치행렬은 t(), 정사각행렬의 역행렬 구하는 함수 solve()
y.mat = solve(t(x.mat)%*%x.mat)%*%(t(x.mat)%*%y)
y.mat

####그리드 서치
a0 =seq(4, 5, by =0.1) #등차수열 생성함수 seq(시작값, 끝나는 값, by는 간격)
#a0 = seq(0,10, length.out=10) #length.out는 전체 시퀀스 길이 
?seq
a0
a1 = seq(15, 16, by =0.1)
a1

#점과 직선과의 차의 제곱합을 11개 출력 해야함
sse = matrix(NA, nrow =length(a0), ncol=length(a1))  #NA결측값 행렬을 생성, 행 a0길이, 열a1길이 
sse
a0[1] + a1[1]*x 
sum(y-(a0[1] + a1[1]*x))^2) #이건 1행 1열에 그리드서치 계산식을 계산해서 넣어야함, 그래서 함수 필요
#그래서 for문 생성 
i = 1
j =1
inter =a0[i]  #y절편
inter
slope = a0[j] #기울기 
slope

for(i in 1:length(a0)) {
  for(j in 1:length(a1)) {
    inter = a0[i]
    slope = a0[j]
    D = sum((y-(inter+slope*x))^2)
    sse[i,j] = D
  }
}
sse
which(sse==min(sse), arr.ind=TRUE) #sse가 최소가 되는 값 찾기, arr.ind 행과 열 번호를 각각 출력
#위의 which로 찾은 n행 n열에 해당하는 값이 최솟값 
#a0, a1의 범위를 점점 줄이고, 간격을 점점 작게 해서 그리드 서치를 실행해나감
#sse = array(NA, c(3,3,2)) 이거는 3차원일 때, 

matrix(NA, nrow=length.a0, ncol=length.a1)
sse = array(NA, c(3,3,2)) #3x3행렬이 두 개 생긴다 

for(i in 1:3) {
  for(j in 1:3) {
    for(k in 1:2) {
    b0 = a0[i]
    b1 = a1[j]
    b2 = a2[k]
    D = sum((y-(b0+b1*x+b2*x^2))^2)
    sse[i,jm] = D
    }
  }
}