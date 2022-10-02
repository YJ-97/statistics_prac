### Poisson 분포

# dpois : 확률질량함수
# ppois : 누적분포함수
# qpois : 분위수함ㅅ
# rpois : 난수

# lambda = 3
Lambda <- 3

# P(X = 2)
dpois(2, Lambda)

# 확률질량함수 그리기
x <- 0:20
fx <- dpois(x, Lambda)
names(fx) <- x
barplot(fx, space=20, xlab='x', ylab='f(x)')
abline(h=0)

# P(X <= 2)
ppois(2, Lambda)

# P(X > 2) = P(X >= 3)
1-ppois(2, Lambda)
ppois(2, Lambda, lower.tail=F)

# 반도체
# 이항분포
n <- 1500
p <- 1/500
pbinom(2, n, p)


# 포아송근사
lambda <- n*p
ppois(2, lambda)

# min_x {P(X <= x) >= 0.6}
x <- 0:5
ppois(x, Lambda)
p <- ppois(x, Lambda)
names(p) <- x      # P(X <= 3) >= 0.6
qpois(0.6, Lambda)


# 음이항분포(실패횟수, 성공횟수, 성공확률)

### 기하분포 & 음이항분포
# geom 
# nbinom

# p = 0.3
p <- 0.3
dgeom(0, p)   #  ==> 실패횟수
dgeom(3, p)
p*(1 - p)^3

pgeom(5, p)
1 - (1 - p)^6

n <- 10000
x <- rgeom(n,p)
# E(X) = (1 - p)/p = 0.7/0.3 = 2.3333
# Var(X) = (1 - p)/p^2 = 0.7/0.09 = 7.7778
mean(x)
var(x)

# r =  3
r <- 3
# P(X = 4)
dnbinom(4, r, p)
choose(r + 4 - 1, r-1)*p^r*(1 - p)^4

# 가위바위보
pnbinom(5, 5, 1/3)

### 다항분포
# multinom
# dmultinom: 확률질량함수
# rmultinom: 난수

n <- 20
p <- c(0.3,0.4,0.3)
x <- c(5,6,9)
n == sum(x)

# d가 아닌 p, q는 계산이 매우 어려운 관계로 R에서 제공하지 않는다고 함
dmultinom(x,prob=p)    # p: 합이 1일 필요는 없음
                       # p=c(9,3,3,1)
rmultinom(50,size = n,prob=p)

### X1과 X2 상관계수 -sqrt(p1/(1-p1)* p2/(1-p2))
-sqrt(0.3/0.7 * 0.4/0.6)
x <- rmultinom(1000,size = n,prob=p)
x1 <- x[1,]
x2 <- x[2,]
cor(x1,x2)







































































