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

#oneway test ��л꼺�� �������� ���� �׽�Ʈ oneway�׽�Ʈ�� �Ѵ�.
oneway.test(perf~clusters, data=kmperf)
#4���� �׷��� ���̰� �ִ�.
#��л꼺�� �����ϳ� ���� �ʾƵ� anova�׽�Ʈ�� ����� ����.
#�׷��� ������ ������ �ϳ��̴�.

#Two-way ANOVA
#�׷��� 2���� �������� ������ ���
#
install.packages('xlsx')
library('xlsx')
tess <- read.xlsx(file = "C:/R_DATA/pain.xlsx", sheetIndex=1)
#������ ������ Ÿ���� ������ ���뿡 �󸶳� ������ ��ġ��
# ������ ������ ��ȣ�ۿ�
#-----------------------
# 2-way anova
#-----------------------
# ����
# ����
# ������ ����

summary(obj.aov.m1)
#Df Sum Sq Mean Sq F value Pr(>F)    
#target.race      3     11    3.54   0.694  0.556          : ������ ������ ��ġ�� �ʴ´�
#target.female    1    100  100.08  19.606  1e-05 ***      : ������ ������ ��� ������ �� ���� ������.
#  Residuals     2123  10837    5.10                       : ������ ������ ������ ��ġ�� �ʴ´�.
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

#��ȣ�ۿ� ȿ���� ���� �м�
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

#�������� : pain.own 
# pain.other ~ target.race * target.female + pain.own

summary(obj.aov.m5)
summary(obj.aov.m6)
anova(obj.aov.m5, obj.aov.m6)