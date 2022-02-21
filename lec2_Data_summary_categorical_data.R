# 기술통계 vs 추론통계

# 기술 통계


# 범주형 데이터를 요약하는 통계 분석 방법들

library(MASS)
library(tibble)

sv <- tibble(survey)
head(sv)

levels(sv$Smoke)
frq.tab <- table(sv$Smoke) # 범주별 빈도 구하기
frq.tab
class(frq.tab) # table 

frq.tab[2]
frq.tab==max(frq.tab) # 최빈값 인덱스
frq.tab[frq.tab==max(frq.tab)] # 인덱싱
names(frq.tab[frq.tab==max(frq.tab)])

which.max(frq.tab) # 최빈값 인덱스 출력 
frq.tab[which.max(frq.tab)]

frq.tab.prop <- prop.table(frq.tab)
frq.tab.prop # 비율로 출력
frq.tab.prop['Never']
frq.tab.prop*100

survey$Smoke=='Never' # 담배 안피우는 사람 인덱스
mean(survey$Smoke=='Never', na.rm = T) # 담배 안피는 사람 비율


arx <- tibble(anorexia)
arx

arx$Postwt > arx$Prewt # 치료 후 몸무게가 큰 경우 인덱스
mean(arx$Postwt > arx$Prewt)


mm <- tibble(mammals)
mm

abs(mm$brain - mean(mm$brain)) > 2*sd(mm$brain) # 두배의 표준편차보다 큰 편차를 가진 동물의 뇌 인덱스 
mean(abs(mm$brain - mean(mm$brain)) > 2*sd(mm$brain))


sp.500 <- tibble(SP500)
sp.500
diff(sp.500$SP500) # 주어진 벡터로 부터 연속된 두개에 대한 차이 
diff(sp.500$SP500) > 0
mean(diff(sp.500$SP500) > 0) # 수익률이 전일보다 증가한 퍼센트 



# 교차표: 두 범주형 변수간 관계 파악

library(vcd)

arth <- tibble(Arthritis)
arth

levels(arth$Treatment) # 치료 종류 - 범주형
levels(arth$Improved) # 치료 효과 - 범주형

cross.tab <- table(arth$Improved, arth$Treatment,
                   dnn=c('my_Improved', 'my_Treatment')) # 3x2
cross.tab
cross.tab['Marked', 'Treated']

cross.tab <- xtabs(~ Improved + Treatment, data=arth)
cross.tab


margin.table(cross.tab, margin = 1) # 1: 행의 합 / 2: 열의 합
margin.table(cross.tab, margin = 2)

prop.table(cross.tab, margin = 1) # 행이 100%인 비율
prop.table(cross.tab, margin = 2) # 열이 100%인 비율


cross.tab
prop.table(cross.tab) # 교차표의 각 셀의 비율


# 합 추가
addmargins(cross.tab, margin = 1) # 교차표에 행열 빈도합
addmargins(cross.tab, margin = 2) 
addmargins(cross.tab)

prop.table(cross.tab, margin = 2)
addmargins(prop.table(cross.tab, margin = 2), 1)
addmargins(prop.table(cross.tab, margin = 1), 2)



library(gmodels)
CrossTable(arth$Improved, arth$Treatment,
           prop.chisq = F,
           dnn=c('my_imp', 'my_trt'))

mult.tab <- table(arth$Improved, arth$Sex, arth$Treatment)
mult.tab

mult.tab2 <- xtabs(~ Improved + Sex + Treatment, data=arth)
mult.tab2

ftable(mult.tab2)
ftable(mult.tab2, row.vars = c(2, 3)) # 2, 3번째 차원을 행에 배치하고 지정하지 않은 차원을 열에 배치

ftable(arth[c('Improved', 'Sex', 'Treatment')],
       row.vars = c(2, 3))

# 각 차원빈도 합 
mult.tab2
margin.table(mult.tab2, 1)
margin.table(mult.tab2, 2)
margin.table(mult.tab2, 3)

mult.tab2
margin.table(mult.tab2, c(1, 3))

ftable(prop.table(mult.tab2, c(2, 3))) # 각 트리트먼트의 성별의 비율 합 = 1.0
ftable(addmargins(prop.table(mult.tab2, c(2, 3)), 1))




































