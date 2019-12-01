#Read data
kmperf<-read.csv(file="C:/R_DATA/km.subset.txt", header = TRUE)
kmperf$clusters<-as.factor(kmperf$clusters)
kmperf$clusters2<-as.factor(kmperf$clusters2)

View(kmperf)
View(kmperf$clusters)
View(kmperf$clusters2)


#One-way ANOVA
kmperf.aov<-aov(perf~clusters, data=kmperf)
summary(kmperf.aov)

#Multiple comparison
#Tukey HSD test
kmcom<-TukeyHSD(kmperf.aov)
kmcom
plot(TukeyHSD(kmperf.aov), col="red", las=1)

#Duncan test
install.packages("agricolae")
library(agricolae)
model<-aov(perf~clusters,data=kmperf)
out <- duncan.test(model,"clusters", 
                   main="KM strategies")
plot(out,variation="IQR")
duncan.test(model,"clusters",alpha=0.05,console=TRUE)

#Normality
library('car')
qqPlot(kmperf$perf)
shapiro.test(kmperf$perf)

#Outlier
outlierTest(kmperf.aov)

#Homoskedascity
leveneTest(perf~clusters, data=kmperf)
bartlett.test(perf~clusters, data=kmperf)

#oneway test 등분산성을 가정하지 않은 테스트 oneway테스트로 한다.
oneway.test(perf~clusters, data=kmperf)
#4개의 그룹은 차이가 있다.
#등분산성을 가정하나 하지 않아도 anova테스트는 상관이 없다.
#그룹을 나누는 기준은 하나이다.

#Two-way ANOVA
#그룹을 2개의 기준으로 나누는 방법
#
install.packages('xlsx')
library('xlsx')
tess <- read.xlsx(file = "C:/R_DATA/pain.xlsx", sheetIndex=1)
#인종과 성별이 타인이 느끼는 고통에 얼마나 영향을 미치냐
# 인종과 성별의 상호작용
#-----------------------
# 2-way anova
#-----------------------
# 인종
# 성별
# 인종과 성별

summary(obj.aov.m1)
#Df Sum Sq Mean Sq F value Pr(>F)    
#target.race      3     11    3.54   0.694  0.556          : 인종을 영향을 미치지 않는다
#target.female    1    100  100.08  19.606  1e-05 ***      : 성별을 여성일 경우 고통을 더 많이 느낀다.
#  Residuals     2123  10837    5.10                       : 인종의 성별을 영향을 미치지 않는다.
#--



View(tess)
#Gender & Race
tess$target.race <- as.factor(tess$target.race)
tess$target.female <- as.factor(tess$target.female)
obj.aov.m1 <- aov(pain.other ~ target.race + target.female, data = tess)
obj.aov.m2 <- aov(pain.other ~ target.race * target.female, data = tess)
summary(obj.aov.m1)
summary(obj.aov.m2)
TukeyHSD(obj.aov.m1, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m1, which = 'target.female', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.female', ordered = FALSE)

myrecode.race <- c('White','Black','Hispanic','Asian')
myrecode.gender <- c('Male','Female')
interaction.plot(x.factor = myrecode.race[tess$target.race], 
                 trace.factor = myrecode.gender[tess$target.female], 
                 response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
                 ylab = "Mean of subjective others' pain", xlab = "target's race",
                 trace.label = "target's gender")

#Party & Race
tess$party3 <- factor(tess$party3,levels=c('Republican','Independent','Democrat'))
tess$own.race<-factor(tess$own.race,levels=c('1','2','3','4'))

obj.aov.m3 <- aov(pain.other ~ own.race + party3, data = tess)

#상호작용 효과에 대한 분석
obj.aov.m4 <- aov(pain.other ~ own.race * party3, data = tess)

summary(obj.aov.m3)
summary(obj.aov.m4)

interaction.plot(x.factor = tess$own.race, trace.factor = tess$party3,
                 response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
                 ylab = "Mean of subjective others' pain", xlab = "respondent's race",
                 trace.label = "Party affiliation")

anova(obj.aov.m3, obj.aov.m4)


#ANCOVA
tess.subset <- tess[complete.cases(tess[,c('pain.other','pain.own')]),]
obj.aov.m5 <- aov(pain.other ~ target.race * target.female, data = tess.subset)
obj.aov.m6 <- aov(pain.other ~ target.race * target.female + pain.own, data = tess.subset)

#통제변수 : pain.own 
# pain.other ~ target.race * target.female + pain.own

summary(obj.aov.m5)
summary(obj.aov.m6)
anova(obj.aov.m5, obj.aov.m6)
