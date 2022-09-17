
# 데이터 불러오기
Pie <- scan('./dataset/pie.txt', what='character', encoding='UTF-8')
Pie
## 도수분포표
# 빈도수 측정
Sale <- table(Pie)
Sale
# 총합 계산
Total <- sum(Sale)
Total
# 도수분포표
SaleProp <- 100*Sale/Total
SaleProp <- round(SaleProp, 1)

# 빈도수와 도수분포표 결합한 테이블 생성
Pie.Freq <- cbind(Sale, SaleProp)
Pie.Freq

#컬럼 이름 지정
colnames(Pie.Freq) <- c('판매량', '판매비율')
Pie.Freq

## 원도표
# 원도표 그리고 도표에 표시되는 이름 지정
pie(SaleProp)
names(SaleProp)
names(SaleProp) <- c('고구마(11.5%)', '딸기(22.2%)', '바나나(7.3%)',
                     '블루베리(20.1%)', '애플(25.2%)', '초코(13.7%)')
pie(SaleProp)

## 막대그래프
# 막대그래프 지정 ylim은 y축 범위, space는 막대 사이 간격
barplot(Sale, ylim=c(0, 60), space=0.5)
# abline(h=0) 은 y축이 0인 수직선 생성
abline(h=0)
# lty는 line type
abline(h=c(20, 40, 60), lty=3)

## 취업률
# scan에 아무것도 없는 경우는 직접 입력
Job <- scan()
55.6 83.3 43.4 58.1 31.6 55.6 60.7 64.6 73.3 55.6 64.3
52.8 22.7 46.3 71.4 53.8 64.5 67.9 71.4 80.0 59.5 40.5
77.1 58.6 65.4 52.4 66.7 91.3 41.3 72.1 61.9 78.4 63.6
41.0 65.2 81.3 54.8 19.6 50.0 53.1 41.2 56.5

# R에서는 구간을 초과 ~ 이하로 설정 : (, ]
# cut이라는 함수는 지정한 구간사이에 각 값들을 배정
JobCut <- cut(Job, breaks = c(10, 40, 50, 60, 70, 80, 100))
JobCut
# right=FALSE는 [, )로 바뀜
JobCut <- cut(Job, breaks = c(10, 40, 50, 60, 70, 80, 100), right=FALSE)
JobCut

JobFreq <- table(JobCut)
JobFreq

JobProp <- round(JobFreq/sum(JobFreq), 3)
JobProp

# cumsum은 누적상대도수 생성
CumJobProp <- cumsum(JobProp)
CumJobProp

Result = cbind(JobFreq, JobProp, CumJobProp)
Result

colnames(Result) <- c('학과수', '상대도수', '누적상대도수')
rownames(Result) <- c('10%이상~40%미만', '40%이상~50%미만', '50%이상~60%미만',
              '60%이상~70%미만', '70%이상~80%미만', '80%이상~100%')
Result

## Histogram
hist(Job)
# 히스토그램이 막대그래프와 다른 점은 전체 면적이 1이 되게, 그 높이를 밀도로 지정인데
# 특별하게 지정하지 않으면 빈도수로 표시 해준다
hist(Job, freq=FALSE) # hist(Job, probability=TRUE) 이 경우는 확률로 나타내는 경우
hist(Job, breaks=c(10, 40, 50, 60, 70, 80, 100), right=FALSE,
     main='취업률 히스토그램', xlab='취업률', ylab='밀도')

## 줄기 잎 그림
stem(Job)

## 과제 1
# 'score.txt'의 자료를 불러오기(학점 자료)
# 도수분포표 만들기 : 도수, 상대도수, 누적상대도수 포함
# 막대그래프 그리기 : 상대도수 표시

# 데이터 불러오고 분포 확인
score = scan('./dataset/score.txt', encoding='utf-8', what='character')
score_table = table(score)
score_table

# 도수
score_total = sum(score_table)
score_total

# 상대도수
score_prop = 100*score_table/score_total
score_prop

# 누적 상대도수
score_cum = cumsum(score_prop)
score_cum

# 도수, 상대도수 ,누적상대도수 포함 테이블
score_Freq = cbind(score_table, score_prop, score_cum)
colnames(score_Freq) = c('도수', '상대도수', '누적상대도수')
score_Freq

# 막대그래프
barplot(score_prop, ylim=c(0, 30), space=0.5)
abline(h=0)
abline(h=c(5, 10, 15), lty=3)

## 과제 2
# 취업률 자료를 (,] 기준으로 변경
# 실습결과의 도수분포표와 히스토그램 비교

# 데이터 불러오고 구간별로 나눈뒤 분포 확인
Job1 <- scan()
55.6 83.3 43.4 58.1 31.6 55.6 60.7 64.6 73.3 55.6 64.3
52.8 22.7 46.3 71.4 53.8 64.5 67.9 71.4 80.0 59.5 40.5
77.1 58.6 65.4 52.4 66.7 91.3 41.3 72.1 61.9 78.4 63.6
41.0 65.2 81.3 54.8 19.6 50.0 53.1 41.2 56.5

JobCut <- cut(Job1, breaks = c(10, 40, 50, 60, 70, 80, 100))
Job_table = table(JobCut)
Job_table

# 도수
Job_total = sum(Job_table)
Job_total

# 상대도수
Job_prop = round(Job_table/Job_total, 3)
Job_prop

# 누적 상대도수
Job_cum = cumsum(Job_prop)
Job_cum

# 도수, 상대도수 ,누적상대도수 포함 테이블
Job_Freq = cbind(Job_table, Job_prop, Job_cum)
colnames(Job_Freq) <- c('학과수', '상대도수', '누적상대도수')
rownames(Job_Freq) <- c('10%이상~40%미만', '40%이상~50%미만', '50%이상~60%미만',
                      '60%이상~70%미만', '70%이상~80%미만', '80%이상~100%')

# (,] 일때의 테이블
Job_Freq

# [,) 일때의 테이블
Result

# (,] 일때의 히스토그램
hist(Job1, breaks=c(10, 40, 50, 60, 70, 80, 100),
     main='취업률 히스토그램', xlab='취업률', ylab='밀도')

# [,) 일때의 히스토그램
hist(Job, breaks=c(10, 40, 50, 60, 70, 80, 100), right=FALSE,
     main='취업률 히스토그램', xlab='취업률', ylab='밀도')

# 비교 결과
# 10%이상~40%미만, 60%이상~70%미만 구간을 제외하고 변동이 있었고 이에 따라 히스토그램도 달라졌다.




