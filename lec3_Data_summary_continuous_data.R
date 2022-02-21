# 기술 통계량 -  연속형 데이터를 요약하는 통계 분석 방법들

# 중심경향 지표: 중위수, 백분위수, 사분위수, 평균
# 변동성 지표: 범위, 사분위범위, 분산, 표준편차


library(MASS)

head(survey)

median(survey$Pulse, na.rm = T) # 중위수 = 50% 백분위 수

?quantile
quantile(survey$Pulse, probs = 0.5, na.rm = T)
quantile(survey$Pulse, probs = 0.05, na.rm = T) # 전체 데이터셋의 5%에 대응되는 백분위 수

quantile(survey$Pulse, probs = c(0.05, 0.95), na.rm = T)

seq(0, 1, 0.25)
quantile(survey$Pulse, na.rm = T) # 디폴트 = 사분위 수

mean(survey$Pulse, na.rm = T) # 맥박수 평균
mean(survey$Pulse <= 80, na.rm = T) # 맥박수가 80 이하인 응답자 비율 


head(iris)

summary(iris$Sepal.Width) # 연속변수를 넣어주면 사분위수, min max 출력

summary(iris$Species) # 범주형 변수 넣어주면 빈도표 출력

summary(as.character(iris$Species)) # 문자벡터 넣어주면 관측값의 갯수 출력

summary(iris) # 데이터 프레임 넣어주면 열 단위로 summary 출력

iris.list <- as.list(iris)
summary(iris.list) # 리스트 넣어주면 각 리스트의 원소크기와 형태 출력
lapply(iris.list, summary) # 각 원소별로 서머리 적용


range(survey$Pulse, na.rm = T) # min, max 출력

var(survey$Pulse, na.rm = T) # 분산
sd(survey$Pulse, na.rm = T) # 표준편차



head(tibble(mtcars))



library(pastecs)
stat.desc(mtcars[c('mpg', 'hp', 'wt')]) # 많은 요약 통계량 출력

library(psych)
describe(mtcars[c('mpg', 'hp', 'wt')]) # 많은 요약 통계량 출력




# 집단별 기술 통계량 비교 

head(tibble(survey))

## 범주별로 연속형 변수의 요약 통계량 비교해보기 
levels(survey$Exer) 

tapply(survey$Pulse, INDEX = survey$Exer, FUN = mean, na.rm=T) # na.rm = T는 mean의 인자 추가 한것
                                                                # 범주별 맥박수 평균

tapply(survey$Pulse, INDEX = survey$Sex, FUN = mean, na.rm=T) 

tapply(survey$Pulse, INDEX = list(survey$Exer, survey$Sex), FUN = mean, na.rm=T) # 여러 카테고리 적용  
aggregate(survey$Pulse, by=list(myExe=survey$Exer), FUN=mean, na.rm=T)
aggregate(survey$Pulse, by=list(myExe=survey$Exer, mySex=survey$Sex), FUN=mean, na.rm=T)

aggregate(survey[c('Pulse', 'Age')],
          by=list(myExe=survey$Exer),
          FUN=mean, na.rm=T)

aggregate(survey[c('Pulse', 'Age')],
          by=list(myExe=survey$Exer),
          FUN=summary)

by(survey[c('Pulse', 'Age')], INDICES = list(myExe=survey$Exer),
   FUN=summary)


describeBy(survey[c('Pulse', 'Age')], group = list(myExe=survey$Exer))






























































