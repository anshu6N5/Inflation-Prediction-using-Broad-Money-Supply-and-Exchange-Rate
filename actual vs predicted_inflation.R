install.packages("MASS")
# loading of necessary libraries 
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(WDI)
library(nlme)
library(lmerTest)
library(performance)

# Modeling The CPI using  LLME test
#search the indicators for the project.
WDIsearch("Inflation")
WDIsearch("Exchange Rate")
WDIsearch("Broad Money Supply")


#Get data from the world bank 
data <- WDI(country = "all", indicator = c("FP.CPI.TOTL.ZG", "PA.NUS.FCRF", "FM.LBL.BMNY.GD.ZS"), start = 2000, end = 2022)
view(data)

#cleaning of data 
data <- data %>% 
  clean_names() %>% 
  rename(
    inflation = fp_cpi_totl_zg,
    exchange_rate = pa_nus_fcrf,
    broad_money_supply = fm_lbl_bmny_gd_zs
  ) %>% 
  drop_na(inflation, exchange_rate, broad_money_supply)
names(data)

# model 

model <-lmer(inflation~exchange_rate+broad_money_supply+(1|country), data = data)                                                    
summary(data)

#calculating R^2 
r2(model)


#predicted vs Actual 
data$predicted <- predict(model)

#graphical representation 
ggplot(data, aes(x = predicted, y = inflation)) + 
  geom_point(alpha = 1, color = "black") + 
  geom_abline(slope = 1, intercept = 0, color ="darkgreen") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(
    title = "The Inflation Analysis based on Broad Money Supply and Exchange Rate ",
    subtitle = "The Actual VS Predicted Inflation",
    source = "Data : WDI via World Bank"
  ) +
  theme_minimal()
