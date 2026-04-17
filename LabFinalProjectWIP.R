#Lab Final Project
library(tidyverse)
PSID<-read_csv("psid.csv")

#Recoding numerically-represented categorical variables into factor variables.
#1. race -> Factor "raceCat"
PSID$raceCat<-factor(NA,levels=c("White","Black"))
PSID$raceCat[PSID$race==0]<-"White"
PSID$raceCat[PSID$race==1]<-"Black"
table(PSID$raceCat, PSID$race)
#2. sex -> Factor "sexCat"
PSID$sexCat<-factor(NA,levels=c("Male","Female"))
PSID$sexCat[PSID$sex==0]<-"Male"
PSID$sexCat[PSID$sex==1]<-"Female"
table(PSID$sexCat, PSID$sex)
#3. education_degree -> Factor "eduCat"
PSID$eduCat<-factor(NA,levels=c("Less than High School","High School", "College"))
PSID$eduCat[PSID$education_degree==1]<-"Less than High School"
PSID$eduCat[PSID$education_degree==2]<-"High School"
PSID$eduCat[PSID$education_degree==3]<-"College"
table(PSID$eduCat, PSID$education_degree)
#4. marital_status -> Factor "marriedCat"
PSID$marriedCat<-factor(NA,levels=c("Married","Single", "Divorced"))
PSID$marriedCat[PSID$marital_status==1]<-"Married"
PSID$marriedCat[PSID$marital_status==2]<-"Single"
PSID$marriedCat[PSID$marital_status==3]<-"Divorced"
table(PSID$marriedCat, PSID$marital_status)

#RQ1a: Are race and marital status associated?
#H0: Race and marital status are not associated. HA: Race and marital status are associated.
# Contingency Table
Race_Married_Table<-table(PSID$marriedCat,PSID$raceCat)
Race_Married_Table
addmargins(Race_Married_Table)
prop.table(Race_Married_Table,margin=1) #Race by Marital Status
prop.table(Race_Married_Table,margin=2) #Marital Status by Race
# Stacked proportional bar chart: marital status by race. 
PSID %>%
  filter(!is.na(marriedCat))%>%
  count(raceCat, marriedCat) %>%
  group_by(raceCat) %>%
  mutate(proportion=n/sum(n)) %>%
  ggplot(aes(x=raceCat,y=proportion,fill=marriedCat))+
  geom_col()+
  scale_fill_manual(values=c('#FA6868','#FACE68','#5A9CB5'))+
  labs(
    title="Proportion of Marital Status by Race",
    x="Race",
    y="Proportion",
    fill="Marital Status")+
  theme_light()+
  theme( #formats the text elements (size and typeface)
    plot.title=element_text(face="bold",size=14),
    axis.title=element_text(size=11)
  )
# Chi-Square Test for marital status and race (alpha = 0.05)
chisq.test(Race_Married_Table,correct=FALSE) #p-value = 0.2052
# Run and save the Chi-Square Test.
Race_Married_test<-chisq.test(Race_Married_Table,correct=FALSE)
# View expected cell count.
Race_Married_test$expected #All expected cell counts > 5.
#p-value > alpha; fail to reject H0.

#RQ1b: Are race and highest educational attainment associated?
#H0: Race and highest educational attainment are not associated. HA: Race and highest educational attainment are associated.
# Contingency Table
Race_Edu_Table<-table(PSID$eduCat,PSID$raceCat)
Race_Edu_Table
addmargins(Race_Edu_Table)
prop.table(Race_Edu_Table,margin=1) #Race by Education
prop.table(Race_Edu_Table,margin=2) #Education by Race
# Stacked proportional bar chart: education by race. 
PSID %>%
  filter(!is.na(eduCat))%>%
  count(raceCat, eduCat) %>%
  group_by(raceCat) %>%
  mutate(proportion=n/sum(n)) %>%
  ggplot(aes(x=raceCat,y=proportion,fill=eduCat))+
  geom_col()+
  scale_fill_manual(values=c('#FA6868','#FACE68','#5A9CB5'))+
  labs(
    title="Proportion of Educational Attainment by Race",
    x="Race",
    y="Proportion",
    fill="Education")+
  theme_light()+
  theme( #formats the text elements (size and typeface)
    plot.title=element_text(face="bold",size=14),
    axis.title=element_text(size=11)
  )
# Chi-Square Test for educational attainment and race (alpha = 0.05)
chisq.test(Race_Edu_Table,correct=FALSE) #p-value = 0.3762
# Run and save the Chi-Square Test.
Race_Edu_test<-chisq.test(Race_Edu_Table,correct=FALSE)
# View expected cell count.
Race_Edu_test$expected #All expected cell counts > 5.
#p-value > alpha; fail to reject H0.

#RQ1c: Are sex and highest educational attainment associated?
#H0: Sex and highest educational attainment are not associated. HA: Sex and highest educational attainment are associated.
# Contingency Table
Sex_Edu_Table<-table(PSID$eduCat,PSID$sexCat)
Sex_Edu_Table
addmargins(Sex_Edu_Table)
prop.table(Sex_Edu_Table,margin=1) #Sex by Education
prop.table(Sex_Edu_Table,margin=2) #Education by Sex
# Stacked proportional bar chart: education by sex. 
PSID %>%
  filter(!is.na(eduCat))%>%
  count(sexCat, eduCat) %>%
  group_by(sexCat) %>%
  mutate(proportion=n/sum(n)) %>%
  ggplot(aes(x=sexCat,y=proportion,fill=eduCat))+
  geom_col()+
  scale_fill_manual(values=c('#FA6868','#FACE68','#5A9CB5'))+
  labs(
    title="Proportion of Educational Attainment by Sex",
    x="Sex",
    y="Proportion",
    fill="Education")+
  theme_light()+
  theme( #formats the text elements (size and typeface)
    plot.title=element_text(face="bold",size=14),
    axis.title=element_text(size=11)
  )
# Chi-Square Test for educational attainment and sex (alpha = 0.05)
chisq.test(Sex_Edu_Table,correct=FALSE) #p-value = 0.4908
# Run and save the Chi-Square Test.
Sex_Edu_test<-chisq.test(Sex_Edu_Table,correct=FALSE)
# View expected cell count.
Sex_Edu_test$expected #All expected cell counts > 5.
#p-value > alpha; fail to reject H0.
