
### Table 형태의 데이터 읽어오기
smart <- read.table('./dataset/smart.csv', header=T, fileEncoding = "CP949", encoding = "UTF-8")

### csv 형태의 데이터 읽어오기
smart <- read.csv('./dataset/smart.csv', fileEncoding = "CP949", encoding = "UTF-8")

head(smart)
tail(smart)

### 분할표
table(gender, model)

# 1
smarttable <- table(smart$gender, smart$model)
smarttable

# 2
with(smart, table(gender, model))

# 3
# attach는 데이터를 R의 경로에 추가
# 다른 파일에서 같은 이름을 가진 열이 존재하면 충돌할 수 있으므로 권장하지 않음
attach(smart)
table(gender, model)
detach(smart)

margin.table(smarttable, 1) ## row
margin.table(smarttable, 2) ## column

prop.table(smarttable) ## 전체 합
prop.table(smarttable, 1) ## row
prop.table(smarttable, 2) ## column
smart.prop <- round(100*prop.table(smarttable, 1), 1)
smart.prop

# 원도표
par(mfrow=c(1,2)) # c(행, 열)
pie(smart.prop[1,], main='남자')
pie(smart.prop[2,], main='여자')

### 3차원 분할표
titanic <- read.csv('./dataset/titanic.csv', fileEncoding = "CP949", encoding = "UTF-8")
head(titanic)
tail(titanic)

ttn.table <- with(titanic, table(Class, Survived, Group))
# 3차원 이상의 교차표에 사용
ftable(ttn.table)
table3way <- ftable(ttn.table, row.vars='Class', col.vars=c('Group', 'Survived'))
ttn.ftable <- data.frame(table3way)
ttn.survive <- ttn.ftable[ttn.ftable$Survived=='Yes',]
ttn.survive

# 등실별생존율
# tapply(대상변수, 그룹변수, 함수)
survive <- with(ttn.survive, tapply(Freq, Class, sum))
survive

total <- with(ttn.ftable, tapply(Freq, Class, sum))
total

round(100*survive/total, 1)

id <- 1:12
ttn.survive$Rate <- round(100*ttn.survive$Freq/(ttn.ftable$Freq[id]+ttn.ftable$Freq[id+12]), 1)
ttn.survive

par(mfrow=c(1,1))
ttnbar <- matrix(ttn.survive$Rate, 4, 3)
ttnbar

row.names(ttnbar) <- c('1등급', '2등급', '3등급', '승무원')
colnames(ttnbar) <- c('남자', '어린이', '여자')

# beside=TRUE 일경우 막대들을 옆으로 표시
barplot(ttnbar, beside=TRUE, legend=rownames(ttnbar), ylim=c(0, 100))
abline(h=c(20, 40, 60, 80, 100), lty=3)
abline(h=0)

### 지방선거 득표율
election <- read.csv('./dataset/election.csv', fileEncoding = "CP949", encoding = "UTF-8")
with(election, by(득표율, 정당, summary))

shortsummary <- function(x){
  result <- round(c(length(x), mean(x), sd(x), min(x), max(x)), 2)
  names(result) <- c("N", "평균", "표준편차", "최솟값", "최댓값")
  return(result)
}
with(election, by(득표율, 정당, shortsummary))

boxplot(득표율~정당, data=election)
boxplot(득표율~정당, data=election, horizontal=T)

### Olympic 육상 100m 우승기록
olympic <- read.csv('./dataset/100m.csv', fileEncoding = "CP949", encoding = "UTF-8")
head(olympic)
tail(olympic)

with(olympic, plot(year, record))
male <- subset(olympic, gender=='M')
female <- subset(olympic, gender=='F')
with(male, plot(year, record, ylim=c(9, 13), cex=1.2))
with(female, points(year, record, pch=16, cex=1.2))
legend(2000, 13, legend=c('남자', '여자'), pch=c(1, 16), bty='n', cex=1.2)

n <- nrow(male)
sxy <- sum(male$year*male$record)-sum(male$year)*sum(male$record)/n
sxy/(n-1) # 공분산

sxx <- sum(male$year^2)-sum(male$year)^2/n
syy <- sum(male$record^2)-sum(male$record)^2/n
sxy/sqrt(sxx*syy) # 상관계수

with(male, cov(year, record)) # 공분산
with(male, cor(year, record)) # 상관계수

### 월별 하루평균 출생아 수
birth <- scan()
1385	1309	1322	1278	1194	1201	1197	1238	1410	1407	1377	1146
1535	1363	1395	1346	1257	1226	1231	1270	1299	1237	1244	1096
1451	1400	1394	1336	1271	1272	1294	1337	1391	1351	1286	1107
1427	1309	1243	1225	1149	1105	1167	1174	1235	1163	1128	1031
1330	1313	1226	1239	1153	1139	1174	1179	1263	1176	1079	1055

# ts 는 timeseries
birth <- ts(birth, start=c(2010, 1), frequency=12)
birth

plot(birth, xlab='년월', ylab='출생아수', type='o')
abline(h=c(1100, 1300, 1500), lty=3)



