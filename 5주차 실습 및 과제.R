##### 경우의 수 계산

n <- 10
k <- 3

factorial(k)

# 순열
factorial(n)/factorial(n-k)
factorial(1000)
prod(n:(n-k+1))

# 중복순열
n^k

# 조합
choose(n, k)

# 중복조합
choose(n+k-1, k)

### Birthday Problem
n <- 365
nomembers <- c(5, 10, 20, 30, 40, 50)
probs <- NULL

for (k in nomembers){
  j <- 0:(k-1)
  probs <- c(probs, prod(1-j/n))
}

probs <- round(probs, 4)
names(probs) <- nomembers
probs

# 통계적 확률
daily <- scan()
1427 1309	1243 1225 1149 1105 1167 1174 1235 1163 1128 1031

monthday <- c(31,28,31,30,31,30,31,31,30,31,30,31)
births <- rep(daily, monthday)
births

length(births)
birthprob <- births/sum(births)
birthprob

# k=50일 떄 표본
k <- 50
# 365까지 중에 50개를 무작위 복원추출
x <- sample.int(n, k, replace=T, prob=birthprob)
x

result <- length(unique(x)) == k
result

# k=50일 때 10000개 표본
nosim <- 10000
result <- 0
for (i in 1:nosim){
  x <- sample.int(n, k, replace=T, prob=birthprob)
  result <- result + (length(unique(x)) == k)
}

cat('# of 생일이 모두 다른 경우:', result, "\n")
cat('생일이 모두 다를 확률:', result/nosim, '\n')

# k = 5, 10, 20, 30, 40, 50일 때 10000개 표본
nosim <- 10000

for (k in nomembers){
  result <- 0
  ''
  for (i in 1:nosim){
    x <- sample.int(n, k, replace=T, prob=birthprob)
    result <- result + (length(unique(x)) == k)
  }
  cat(k, '명 선택할 때\n ')
  cat('# of 생일이 모두 다른 경우:', result, "\n")
  cat('생일이 모두 다를 확률:', result/nosim, '\n')
}

## 표준정규분포 (-1, 1, 95)
# 정규분포의 확률밀도함수 : dnorm
# 정규분포의 누적분포함수 : pnorm
# 균일분포의 난수추출함수 : runif

z <- seq(-3, 3, by=0.01)
plot(z, dnorm(z), type='l', ylab='f(z)')
abline(h=0)
dnorm(0)

lines(c(-1, -1), c(0, 0.4))
lines(c(1.95, 1.95), c(0, 0.4))
lines(c(-1, 1.95), c(0.4, 0.4))

Area <- (1.95+1)*0.4
Nosim <- c(500, 1000, 5000, 10000, 50000)
for (n in Nosim){
  x <- runif(n,-1,1.95)
  y <- runif(n,0,0.4)
  fx <- dnorm(x)
  ratio <- sum(y < fx)/n
  cat("N =",n,"\t Ratio =",round(ratio,4),"\t Prob =",round(ratio*Area,4),"\n") 
}

cat("True Probability:",round(pnorm(1.95)-pnorm(-1),4))


# 과제
# 생일문제 : 상황 B에서 k = 5, 10, 25, 50, 100일 때 통계적 확률을 추정할 것. 반복수는 100000번




































