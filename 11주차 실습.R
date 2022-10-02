## 정규확률밀도함수 그리기

curve(dnorm, -3, 5,ylab="f(x)")
abline(h=0)
x <- seq(-3, 5, by=0.01)
# dnorm(x값, 평균, 표준편차)
lines(x, dnorm(x, 1.5, 1), col="red")

## 확률분포함수 계산
x <- seq(0, 1.99, by=0.01)
length(x)
Fx <- pnorm(x)
Fx <- matrix(Fx, 20, 10, byrow=T)
Fx <- round(Fx, 4)
colnames(Fx) <- 0:9
rownames(Fx) <- seq(0, 1.9, by=0.1)
Fx

# P(Z > 1.37)
1-pnorm(1.37)
pnorm(1.37, lower.tail=F)
# P(0.5 < Z < 1.2)
pnorm(1.2)-pnorm(0.5)
# P(|Z| > 1.96) = 2P(Z < -1.96)
2*pnorm(-1.96)

## 분위수 계산
# P(Z < z) = 0.975
qnorm(0.975)
# P(-z < Z < z) = 0.9 => P(Z < z) = 0.95
qnorm(0.95)

qnorm(0.025)
qnorm(0.05)

## X ~ N(60, 16) : sigma=4
# P(55 < X < 63) = P(-1.25 < Z < 0.75)
mu <- 60
sigma <- 4
pnorm(63, mu, sigma)-pnorm(55, mu, sigma)
pnorm(0.75)-pnorm(-1.25)
# P(X < x) = 0.025
qnorm(0.025, mu, sigma)
qnorm(0.025)*sigma+mu

## 아침식사
# 빵: X ~ N(200, 12^2)
# 우유 : Y ~ N(85, 9^2)

mu <- 200+85
sigma <- sqrt(12^2 + 9^2)
n <- 100000
x <- rnorm(n, 200, 12)
y <- rnorm(n, 85, 9)

# 300보다 클 확률
pnorm(300, mu, sigma, lower.tail = F)

cal <- x+y
hist(cal, nclass=50, main="아침식사 칼로리", prob=T)
z <- seq(230, 350, by=0.1)
lines(z, dnorm(z, mu, sigma), col="blue")

pnorm(300, mu, sigma, lower.tail=F)
mean(cal > 300)

## 확률분포 : (0, 1, 2) => (2/5, 2/5, 1/5)
# Y = max(X1, X2)
# c(4/25, 12/25, 9/25)
x <- c(0, 1, 2)
probx <- c(2/5, 2/5, 1/5) 
n <- 100000
x1 <- sample(x, n, replace=T, prob=probx)
x2 <- sample(x, n, replace=T, prob=probx)
max(x1, x2)  
y <- pmax(x1, x2)        # parallel max
ytbl <-table(y)
prop.table(ytbl)