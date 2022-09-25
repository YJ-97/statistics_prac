# 과제 1
# 타이타닉호 예제에서 생존율 대신 사망률을 이용하여 정리할 것

titanic <- read.csv('./dataset/titanic.csv', fileEncoding = "CP949", encoding = "UTF-8")

ttn.table <- with(titanic, table(Class, Survived, Group))

table3way <- ftable(ttn.table, row.vars='Class', col.vars=c('Group', 'Survived'))
ttn.ftable <- data.frame(table3way)

ttn.dead <- ttn.ftable[ttn.ftable$Survived=='No',]
ttn.dead

dead <- with(ttn.dead, tapply(Freq, Class, sum))
dead

total <- with(ttn.ftable, tapply(Freq, Class, sum))
total

round(100*dead/total, 1)

id <- 1:12
ttn.dead$Rate <- round(100*ttn.dead$Freq/(ttn.ftable$Freq[id]+ttn.ftable$Freq[id+12]), 1)
ttn.dead

par(mfrow=c(1,1))
ttnbar <- matrix(ttn.dead$Rate, 4, 3)
ttnbar

row.names(ttnbar) <- c('1등급', '2등급', '3등급', '승무원')
colnames(ttnbar) <- c('남자', '어린이', '여자')

barplot(ttnbar, beside=TRUE, legend=rownames(ttnbar), ylim=c(0, 100))
abline(h=c(20, 40, 60, 80, 100), lty=3)
abline(h=0)



# 과제 2
# 올림픽 육상 여자 100m 우승기록을 이용하여 연도와 기록의 공분산, 상관계수 계산할 것

olympic <- read.csv('./dataset/100m.csv', fileEncoding = "CP949", encoding = "UTF-8")


female <- subset(olympic, gender=='F')

with(female, cov(year, record)) # 공분산
with(female, cor(year, record)) # 상관계수

# 과제 3
# 스마트폰 선호도 예제에서 A 모델 52, B 모델 56, C 모델 38개에 대해 남여의 선호 빈도가 조사되었다고 하면
# 해당 자료에 맞게 분할표의 비율과 원도표을 구할 것

smart <- read.csv('./dataset/smart.csv', fileEncoding = "CP949", encoding = "UTF-8")

smarttable <- table(smart)
smarttable

smart.prop <- prop.table(smarttable, 2)
smart.prop <- round(smart.prop*100, 2)
smart.prop

par(mfrow=c(1, 3)) # c(행, 열)
pie(smart.prop[,1], main='모델 A')
pie(smart.prop[,2], main='모델 B')
pie(smart.prop[,3], main='모델 C')


