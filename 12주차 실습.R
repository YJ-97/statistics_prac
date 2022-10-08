## 예제
x <- c(0, 1, 2)
probx <- c(2/5, 2/5, 1/5)
plot(x, probx, type='h', ylab='P(X=x)', ylim=c(0, 0.4))
abline(h=0)

## (x1+x2)/2의 표집분포
# P(0, 0.5, 1, 1.5, 2) = c(4/25, 8/25, 8/25, 4/25, 1/25)
반복 <- 100000
x1 <- sample(x, 반복, replace=T, prob=probx)
x2 <- sample(x, 반복, replace=T, prob=probx)
xx <- cbind(x1, x2)
xbar <- rowMeans(xx)
xbartbl <- table(xbar)/반복
plot(xbartbl, type='h', xlab=expression(bar(x)), ylab=expression(f(bar(x))))
abline(h=0)

## X ~ Poi(1)
# X1 + ... + X15 ~ ?
x <- c(3, 5, 10, 15, 20)
probT <- round(ppois(x, 15), 4)

# Monte Carlo simulation
반복 <- 100000
xsum <- rep(NA, 반복)
for (i in 1:반복){
  xsum[i] <- sum(rpois(15, 1))
}

prob <- NULL
for (i in 1:5){
  prob <- c(prob, mean(xsum <= x[i]))
}

probE <- round(prob, 4)
result <- rbind(probT, probE)
colnames(result) <- x

## 중심극한정리
x <- c(0, 1, 2)
probx <- c(2/5, 2/5, 1/5)
mu <- 0.8
sigma <- sqrt(14/25)

반복 <- 10000
n <- 50
xbar <- rep(NA, 반복)
for (i in 1:반복)
  xbar[i] <- sum(sample(x, n, replace=T, prob=probx))

fx <- table(xbar)/반복
plot(fx, xlab="합", ylab="f(합)")
abline(h=0)
xx <- seq(0, n*2, by=0.01)
lines(xx,dnorm(xx, n*mu, sqrt(n)*sigma), col="red")

## 이항분포의 정규근사
n <- 100
p <- 0.4
mu <- n*p
sigma <- sqrt(n*p*(1-p))
x <- 0:n

fx <- dbinom(x, n, p)
plot(x, fx, ylab='f(x)', type='h', xlim=c(20, 60))
abline(h=0)

x <- seq(20, 60, by=0.1)
fx <- dnorm(x, mu, sigma)
lines(x, fx, col='red')

# 여론조사
# P(x leq 78)

n <- 150
p <- 0.6
pbinom(78, n, p)

# 정규근사
mu <- n*p
sigma <- sqrt(n*p*(1-p))
pnorm(78, mu, sigma)
pnorm(78+1/2, mu, sigma)

## (n-1)S^2/sigma^2의 표집분포 (sigma를 알고 있다고 가정)
sigma <- 5
반복 <- 100000
n <- 10 # 10, 20
mains <- paste("N =", n)
test <- rep(NA, 반복)
for (i in 1:반복){
  x <- rnorm(n, 0, sigma)
  test[i] <- (n-1)*var(x)/sigma^2
}

hist(test, nclass=50, xlab='(n-1)s^2/sigma^2', prob=T, main=mains)
x <- seq(0.001, 60, by=0.01)
lines(x, dchisq(x, n-1), col='red')

## 최댓값 분포
n <- 5
반복 <- 10000
maxx <- rep(NA, 반복)
for (i in 1:반복){
  maxx[i] <- max(runif(n))
}
hist(maxx, nclass=50, xlab='max(x)', ylab='f(max(x))', prob=T)

x <- seq(0, 1, by=0.01)
fx <- function(x) 5*x^4
lines(x, fx(x), col='red')













