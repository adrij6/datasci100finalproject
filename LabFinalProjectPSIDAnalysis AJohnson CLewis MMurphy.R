#Lab Final Project | Adrian Johnson, Cherish Lewis, Marcy'Anna Murphy
library(tidyverse)
PSID<-read_csv("psid.csv")

# -------------------------------------------------------------------
# RECODING & CLEANING
# -------------------------------------------------------------------

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

# -------------------------------------------------------------------
# RQ1a and RQ1b: CHI SQUARE TEST
# -------------------------------------------------------------------

#RQ1a: Are race and marital status associated?
#Response Variable: marital status (marriedCat) (categorical)
#Explanatory Variable: race (raceCat) (categorical)
#H0: Race and marital status are not associated. HA: Race and marital status are associated.

# Contingency Table
Race_Married_Table<-table(PSID$marriedCat,PSID$raceCat)
Race_Married_Table
addmargins(Race_Married_Table)
prop.table(Race_Married_Table,margin=1) #Race by Marital Status
prop.table(Race_Married_Table,margin=2) #Marital Status by Race
# Ran calculations to check that expected cell counts are all >5.
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

# -------------------------------------------------------------------

#RQ1b: Are race and highest educational attainment associated?
#Response Variable: education (eduCat) (categorical)
#Explanatory Variable: race (raceCat) (categorical)
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

# -------------------------------------------------------------------
# RQ2: ANOVA
# -------------------------------------------------------------------

#RQ2: Are marital status and household income associated?
#Response Variable: household_income (numerical)
#Explanatory Variable: marital status (marriedCat) (categorical)
#H0: Household income and marital status are not associated. HA: Household income and marital status are associated.

# ANOVA Test for household income and marital status.
anova_model <- aov(household_income ~ marriedCat, data = PSID)
summary(anova_model)
#p-value < alpha; reject H0.
#Boxplot: household income by marital status.
boxplot(household_income ~ marriedCat,
        data = PSID,
        main = "Household Income by Marital Status",
        xlab = "Marital Status",
        ylab = "Household Income")

#Differences between groups: Tukey Model
TukeyHSD(anova_model)
plot(TukeyHSD(anova_model))
#Differences between Divorced / Single, Divorced / Married.

# -------------------------------------------------------------------
# RQ3: LINEAR REGRESSION
# -------------------------------------------------------------------

#RQ3: Are age and total household income associated?
#Response Variable: household_income (numerical)
#Explanatory Variable: age (numerical)
#H0: Age and household income are not associated. HA: Age and household income are associated.

#Scatterplot w/ Regression Line: household income by age.
ggplot(PSID, aes(x = age, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Age and Household Income",
    x = "Age",
    y = "Household Income"
  ) +
  theme_light()

#Linear Regression Model
RQ3_model <- lm(household_income ~ age, data = PSID)
summary(RQ3_model)
confint(RQ3_model)
#p-value > alpha; fail to reject H0.

#Conditions: Residuals and Fitted Values
RQ3_residuals <- residuals(RQ3_model)
RQ3_fitted <- fitted(RQ3_model)
#Conditions: Linearity and Constant Variance
ggplot(data.frame(RQ3_fitted, RQ3_residuals),
       aes(x = RQ3_fitted, y = RQ3_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_light()
#Conditions: Normality of Residuals
ggplot(data.frame(RQ3_residuals), aes(sample = RQ3_residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_light()