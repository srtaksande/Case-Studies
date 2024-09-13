## Setting Directory
setwd('/Users/aishwaryasingh/Desktop/MS_Proj')

## MEAN IMPORTANCE OF ATTRIBUTES
library(readxl)
relative<-read_xlsx('/Users/aishwaryasingh/Desktop/MS_Proj/Group 2_modified raw data.xlsx',sheet = "mean imp")
View(survey_data)

## Calculating Means for Attributes
round(colMeans(relative[sapply(relative, is.numeric)]),2)
mean_importance<-round(colMeans(relative[sapply(relative, is.numeric)]),2)
View(mean_importance)

write.csv(mean_importance,file = '/Users/aishwaryasingh/Desktop/MS_Proj/mean_importance.csv')

## CONJOINT ANALYSIS FOR COMBINATION OF ATTRIBUTES 
conjoint_all<-read_xlsx('/Users/aishwaryasingh/Desktop/MS_Proj/Group 2_modified raw data.xlsx',sheet = "dummy_new")
View(conjoint_all)

## Creating Dummy Variables for Price,Battery,Display,Health & Technology
conjoint2_dummy_all<-fastDummies::dummy_cols(conjoint_3,select_columns = c("Price","Battery","Display","Health","Technology"),remove_first_dummy = TRUE)
str(conjoint2_dummy_all)
View(conjoint2_dummy_all)

df2<-conjoint2_dummy_all[ ,-c(2:6)]
View(df2)
write.csv(df,file = '/Users/aishwaryasingh/Desktop/MS_Proj/All_Attributes_Dummy.csv')

## Regression
fit_dummy_all <- lm(Rating ~ Price_18k+Price_22k+Price_25k+
                  Battery_7+Battery_9+Battery_12+
                  Display_1.5+Display_1.7+Display_2+
                  Health_Elite+Health_Premium+
                  Technology_b+Technology_c+Technology_d, data = df2)

summary(fit_dummy_all)

#Getting the co-efficients, standrad errors, t-values and p-values for all regression
Coefficients_all <- round(summary(fit_dummy_all)$coefficients[,1], digits = 3)
StandardError_all <- round(summary(fit_dummy_all)$coefficients[,2], digits = 3)
Tvalues_all <-  round(summary(fit_dummy_all)$coefficients[,3], digits = 3)
Pvalues_all <- round(summary(fit_dummy_all)$coefficients[,4], digits = 3)
Estimates_All <- data.frame(Coefficients_all, StandardError_all, Tvalues_all, Pvalues_all)
View(Estimates_All)

write.csv(Estimates_All,file = '/Users/aishwaryasingh/Desktop/MS_Proj/Estimates_All_Attributes.csv')



