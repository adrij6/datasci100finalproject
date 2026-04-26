# LAB PROJECT: Q3 (marcy) #
# research question: Is there a relationship between age and total household income?
# response variable: household_income (numerical)
# explanatory variable: age (numerical)
# statistical method: simple linear regression

library(tidyverse)
setwd("~/Desktop/QTM/QTM Datasets")
PSID <- read_csv("PSID.csv")
view(PSID)
head(PSID)
names(PSID)

# cleaning the data
# keeping only relevant variables and removing missing values
RQ3_data <- PSID %>%
  select(age, household_income) %>%
  drop_na(age, household_income)

# -------------------------------------------------------------------
# scatterplot with regression line 
# -------------------------------------------------------------------

ggplot(RQ3_data, aes(x = age, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Age and Household Income",
    x = "Age",
    y = "Household Income"
  ) +
  theme_light()

# -------------------------------------------------------------------
# Linear regression model
# -------------------------------------------------------------------
RQ3_model <- lm(household_income ~ age, data = RQ3_data)

summary(RQ3_model)
confint(RQ3_model)

# ---------------------------------------------------------------------
# linear regression condition check
# ---------------------------------------------------------------------

# residuals and fitted values
RQ3_residuals <- residuals(RQ3_model)
RQ3_fitted <- fitted(RQ3_model)

# checking linearity and constant variance
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

# checking normality of residuals
ggplot(data.frame(RQ3_residuals), aes(sample = RQ3_residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_light()


