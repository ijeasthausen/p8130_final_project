library(tidyverse)
data_no_outliers = read.csv("./data/data_no_outliers.csv")
data_with_outliers = read.csv("./data/cleaned_data_for_analyses.csv")
model_no_outliers = lm(log_length_of_stay ~ is30dayreadmit + 
                         cindex + 
                         evisit + 
                         icu_flag + 
                         age + 
                         gender + 
                         religion +  
                         maritalstatus + 
                         insurancetype + 
                         bmi + 
                         o2sat_cat + 
                         temperature_cat + 
                         heartrate_transformed + 
                         respirationrate_transformed + 
                         mean_arterial_pressure, 
           data = data_no_outliers)
model_with_outliers = lm(log_length_of_stay ~ is30dayreadmit +
                           cindex + 
                           evisit +
                           age +
                           temperature_cat +
                           insurancetype +
                           heartrate_transformed +
                           respirationrate_transformed +
                           mean_arterial_pressure,
                         data = data_with_outliers)
mse_no_outliers = mean(model_no_outliers$residuals^2)
mse_with_outliers = mean(model_with_outliers$residuals^2)
