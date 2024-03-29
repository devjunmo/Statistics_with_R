
# 평균검정: 평균에 대한 가설검정. 
# 일표본 평균검정, 독립표본 평균검정, 대응표본 평균검정
# 평균에 대한 가설검정 = t test를 통해 수행
# 표본 평균이 모집단 평균과 동일한지 여부는 t값을 검정 통계량으로 사용하여 검정.
# t값은 t분포를 따름.
# t분포는 표본 크기에 따라 모양이 달라짐. 정규분포와 유사한 모양임. 
# 표본 크기가 클수록 모양이 뽀족해짐, 작을수록 꼬리부분이 두터워짖ㅁ. 
# 표본크기가 충분히 커지면 정규분포와 거의 똑같아짐.

# 일반인과 혈압이 다르다(양측검정 - 높, 낮 영역 둘다 해당됨) / 높.낮다 (단측검정 - 방향성의 한쪽만 만족)
# 양측 검정은 t값의 양수/음수에 해당하는 확률 부분의 합을 고려해서 0.05보다 큰지 작은지 확인.

# t값을 구하고, 그 t값이 귀무가설이 사실이라는 전제 하에 얼마나 흔히 관찰 가능한것인가로 가설 검정 수행 


# t분포에서 특정 t값에 해당하는 누적 확률 분포를 출력.
# 해당 t값 이하가 나올 확률이 얼마나 높은지.
# lower.tail = F을 주면 해당 t값보다 클 확률이 얼마나 높을지.
pt(3.58, df=20-1, lower.tail = F) # df = 자유도 = 표본크기-1 // 3.58 이상의 확률률
pt(3.58, df=20-1, lower.tail = F) * 2  # 양측검정


# 특정 확률에 해당하는 t값 출력 
# 양측검정일떄, 0.05/2 = 0.025니까, -2.09 or 2.09 바깥범위의 관측값은 귀무가설이 참일때 매우 드문 경우
qt(0.025, df=20-1) # 왼쪽 끝부분 -2.09
qt(0.025, df=20-1, lower.tail = F) # 오른쪽 끝부분 2.09


# 벤처기업 20명 벤처기업 경영자의 혈압 평균 135가 확실한 값인지 믿을수 있는가.
# 20명 선정 작업을 반복해서 그런 혈압의 평균을 t값으로 환산해보면 
# 신뢰구간 95%면 95개의 표본그룹은 모평균을 포함하고, 5개의 표본그룹은 모평균을 포함하지 않는다는것 

################################################################
# t검정을 이용한 일 표본 평균검정 (one sample t test)          #
################################################################

# 하나의 표본 데이터를 사용하여 모집단 평균이 특정값과 같은지 검정
# 표본집단이 특정 모집단과 일치하는지 아닌지를 알고 싶을때.
# 벤처기업 혈압 / 가구당 소득 표본데이터를 바탕으로 기존에 알려진 가구당 소득이 맞는지 검정 

library(MASS)
library(tibble)
tibble(cats) # 성별 / 몸무게 / 심장무게

# 고양이 몸무게가 평균 2.6kg라는데.. 내 생각에는 2.6kg는 아닌거같은데..? 라는 가설 검정 해보기
# 귀무가설: 고양이 몸무게는 평균 2.6이다 / 대립가설: 2.6이 아니다.
t.test(x=cats$Bwt, mu = 2.6) # 몸무게 표본 평균: 2.7, 해당에 대응되는 t값: 3.05, 자유도: 144-1, p가 0.05보다 작음 (-> 귀무가설 기각)
                            # alternative hypothesis: true mean is not equal to 2.6 <<대립가설>>
                            # 95 percent confidence interval: 2.643669 2.803553: 95% 믿음 하에서 미지의 모집단 평균은 2.64~2.80사이 


t.test(x=cats$Bwt, mu = 2.7) # 이런 경우면 귀무가설 채택

?t.test # 기본값: 양측검정 (alternative 옵션)
t.test(x=cats$Bwt, mu = 2.6, alternative = 'greater')
t.test(x=cats$Bwt, mu = 2.6, alternative = 'less')

cats.t <- t.test(x=cats$Bwt, mu = 2.6) 
str(cats.t)
cats.t$p.value
cats.t$conf.int
t.test(x=cats$Bwt, mu = 2.6, conf.level = 0.99) 


# 모집단의 성공 비율이 특정 값과 같은지 검정 해보기 
# 프로야구 A팀이 50%가 넘는다는 주장 검정해보기
# 30경기중 18경기 승리했으니 승률 50 이상임!!
prop.test(x=18, n=30, p=0.5, alternative = 'greater') # p = 0.1807 / 프로야구 팀의 승률이 50% 이상이라는 충분한 근거로 작용하지 못함.
                                                      # 승률 50% 이하 애들도 18%정도 확률로 승률 18/30정도 나올수 있다. 5% 기준으로 보면 흔하다.

prop.test(x=180, n=300, p=0.5, alternative = 'greater') # p = 0.0003



#######################################################################
# t검정을 이용한 독립 표본 평균검정 (two-independent samples t test)  #
#######################################################################

# 남녀간 시험 성적 차이, 흡연자 비흡연자 폐암률 차이
# 귀무가설: 남녀간 성적 차이 없다 = 남녀간 시험성적 평균 차이는 영이다.

library(MASS)
tibble(cats)

# 고양이의 성별에 따라 몸무게의 차이가 있을것이다. 라는 가설 검정
# 귀무: 성별에 따라 몸무게 차이 없다. = 암수 몸무게 평균 차이가 영이다
# 대립: 차이 있다 = 몸무게 평균 차이가 영이 아니다.

# formular 종속변수~독립변수수
t.test(formula= Bwt ~ Sex, data=cats) # mean in group F mean in group M  2.359574   2.900000 -> 표본 평균에 차이 존재. 
                                      # p-value = 8.831e-15 -> 귀무가 참일때 이런 데이터가 나올 확률이 개같이 낮다.
                                      # 95 percent confidence interval:  -0.6631268 -0.4177242: 신뢰구간 내 0이 포함되어 있지 않음.
                                      # 즉, 모집단에서 두 집단의 몸무게 차이가 영이 아니란말.

Bwt.f <- cats$Bwt[cats$Sex=='F']
Bwt.m <- cats$Bwt[cats$Sex=='M']

mean(Bwt.f) # 2.35
mean(Bwt.m) # 2.9
t.test(Bwt.f, Bwt.m)


# 집단간 비율이 동일한지 검정해보기 
# 집단들의 성공비율이 모두 동일한지검정 

# 폐 질환자 대비 흡연자 비율이 병원에 따라 차이가 있는거 같다 <<라는 가설 검정
patients <- c(86, 93, 136, 82)
smokers <- c(83, 90, 129, 70)

smokers/patients # 귀무 = 이 네개의 비율이 모집단과 같을것이다

prop.test(x=smokers, n=patients)  # x=사건이 발생한 횟수 n=대응되는 집단의 크기
                                  # p-value = 0.005585 / 해당 비율들이 병원에 따라 모두 같다면 위 경우가 나올 확률이 매우 낮다.




################################################################
# t검정을 이용한 대응 표본 평균검정 (paired - samples t test)  #
################################################################

# 두 집단이 서로 유관할때.
# 다이어트 프로그램 참가 전후의 몸무게 

## 독립표본 vs 대응표본
## 독립표본 실험: 무작위로 실험대상자 선정해서 두 군으로 나누기
## 대응표본 실험: 무작위로 실험대상자 선정 -> 한번은 아침식사를 하고 IQ테스트, 또한번은 아침안먹고 IQ테스트 

tibble(sleep) # 수면시간 증감 / 수면제 종류 / ID

# 수면제에 따라 수면시간 차이가 있을거같다... < 라는 가설 검정
# 귀무: 수면제에 따라 수면차이 없다

t.test(extra ~ group, data=sleep, paired=T) # paired=T 줘야 paired t test로 작동
                                            #  p-value = 0.002833 / 귀무가 참일때 이 데이터가 나올 확률이 개같이 낮다. -> 귀무 기각

# t.test 함수에 feature를 formula 형식으로 지정하려면 long format이어야 함.
# long format은 데이터 셋에 포함된 변수와 변수 값을 두개의 열로 표현함. 

library(tidyr)
sleep.wide <- spread(sleep, key=group, value=extra)
sleep.wide

t.test(sleep.wide$`1`, sleep.wide$`2`, paired = T) # wide format일때는 이렇게..




























