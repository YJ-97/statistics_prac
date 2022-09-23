## 취업률
# scan에 아무것도 없는 경우는 직접 입력
Job <- scan()
55.6 83.3 43.4 58.1 31.6 55.6 60.7 64.6 73.3 55.6 64.3
52.8 22.7 46.3 71.4 53.8 64.5 67.9 71.4 80.0 59.5 40.5
77.1 58.6 65.4 52.4 66.7 91.3 41.3 72.1 61.9 78.4 63.6
41.0 65.2 81.3 54.8 19.6 50.0 53.1 41.2 56.5

### 중심위치

# 평균
n <- length(Job)
sum(Job)/n
mean(Job)

# 가중평균
x <- c(0.28, -0.28)
w <- c(700, 300)
weighted.mean(x, w)

# 중앙값
median(Job)

# 절사평균
mean(Job, trim=0.1)
mean(Job, trim=0)
mean(Job, trim=0.5)

# 순서통계량 (앞뒤로 3개씩 제거하고 평균 계산)
Job <- sort(Job)
trim <- c(1:3, (n-2):n)
mean(Job[-trim])

# 최빈값
mode(Job)
freq <- table(Job)
maxfreq <- max(freq)
which(freq == maxfreq)

###산포

# 범위
ends <- range(Job)
diff(ends)
max(Job) - min(Job)

# 사분위수
quantile(Job)
Q <- quantile(Job, probs=c(0.25, 0.5, 0.75))
IQR(Job)
Q[3] - Q[1]

# 상자그림
boxplot(Job)
boxplot(Job, horizontal=TRUE, xlab='취업률')

# 분산 & 표준편차
var(Job)
sd(Job)

# 표준화
xbar <- mean(Job)
s <- sd(Job)
z <- (Job-xbar)/s
z
scale(Job)

# 절대편차평균
abdev <- abs(Job - median(Job))
sum(abdev)/(n-1)

# 변동계수
sd(Job)/mean(Job)

### 분포의 형태

# 왜도 & 첨도
skewness <- sum(z^3)/(n-1)
skewness
kurtosis <- sum(z^4)/(n-1)-3
kurtosis

dist.shape <- function(x){
  n <- length(z)
  result <- c(NA, NA)
  if (n >= 2){
    z <- (x-mean(x))/sd(x)
    skew <- sum(z^3)/(n-1)
    kurt <- sum(z^4)/(n-1)-3
    result <- c(skew, kurt)
  }
  return(result)
}

dist.shape(Job)

## 과제 1
# 'hit.txt'의 자료를 불러오기 : 연도별 프로야구 최고 타율
# 표본평균, 표본중앙값, 최댓값과 최솟값을 제외한 평균 계산
# 상자그림, 분산, 변동계수 계산
hit <- read.csv('./dataset/hit.txt', encoding='utf-8')
hit <- unlist(hit)

# 표본평균
mean(hit)

# 표본 중앙값
median(hit)

# 최대, 최솟값 제외 평균
hit <- sort(hit)
mean(hit[-c(1, 34)])

# 상자그림
boxplot(hit)

# 분산, 표준편차
var(hit)
sd(hit)

# 변동계수
sd(hit)/mean(hit)


## 과제 2 
# 수정된 왜도와 첨도 함수 만들기

dist.shape1 <- function(x){
  n <- length(z)
  result <- c(NA, NA)
  if (n >= 2){
    z <- (x-mean(x))/sd(x)
    skew <- (n*sum(z^3))/((n-1)*(n-2))
    kurt <- ((n*(n+1)*sum(z^4))/((n-1)*(n-2)*(n-3))) - ((3*((n-1)^2))/((n-2)*(n-3)))
    result <- c(skew, kurt)
  }
  return(result)
}
dist.shape(hit)
dist.shape1(hit)

