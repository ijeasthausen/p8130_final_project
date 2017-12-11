##############################
# Criterion based selection
##############################

## PREPARATION
# lib
library(tidyverse)
library(leaps)

# load the data
data_full = read.csv("./data/cleaned_data_for_analyses.csv") %>% 
  select(-X, -postalcode,-facilityname,-facilityzip, -patientid, -visitid, -date) %>% 
  mutate(is30dayreadmit = as.factor(is30dayreadmit), icu_flag = as.factor(icu_flag))

# fit
fit_bwd = lm(formula = log_length_of_stay ~ is30dayreadmit + evisit + age +
               gender + maritalstatus + insurancetype + heartrate_transformed +
               respirationrate_transformed + mean_arterial_pressure, data = data_full)

fit_fwd = lm(formula = log_length_of_stay ~ is30dayreadmit + evisit + age  + insurancetype + heartrate_transformed +respirationrate_transformed + mean_arterial_pressure + race, data = data_full)

# delete NAs
data_test = data_full %>% na.omit()

## ANALYSIS
## Cp method
leaps(x = model.matrix(fit_bwd)[,-1],
      y = data_test$log_length_of_stay,
      method = "Cp") %>% 
  faraway::Cpplot()

leaps(x = model.matrix(fit_fwd)[,-1],
y = data_test$log_length_of_stay,
method = "Cp") %>%
faraway::Cpplot()

## Adjusted R2 method
leaps(x = model.matrix(fit_bwd)[,-1],
      y = data_test$log_length_of_stay,
      method = "adjr2") %>% 
  faraway::maxadjr(10)

leaps(x = model.matrix(fit_fwd)[,-1],
y = data_test$log_length_of_stay,
method = "adjr2") %>%
faraway::maxadjr(10)

## best function
best <- function(model, ...)
{
    subsets <- regsubsets(formula(model), model.frame(model), ..., nvmax = 15)
    subsets <- with(summary(subsets),
    cbind(n_predictor = as.numeric(rownames(which)), cp, adjr2, bic, rss, rsq, which))
    return(subsets)
}
# Select the 'best' 2 models of each model size
best_bwd = round(best(fit_bwd, nbest = 1), 4) %>% as.tibble()
write.csv(best_bwd, file = "results_best_bwd.csv",row.names=FALSE)
best_fwd = round(best(fit_fwd, nbest = 1), 4) %>% as.tibble()
write.csv(best_fwd, file = "results_best_fwd.csv",row.names=FALSE)

# plot cp~p
plot(x = best_bwd$n_predictor + 1, y = best_bwd$cp)
abline(a = 0, b = 1, col="purple")

plot(x = best_fwd$n_predictor + 1, y = best_fwd$cp)
abline(a = 0, b = 1, col="purple")


###########################
# Forward selection
###########################

# - start with no variables in the model
# - check predictors' p-values if added to model, choose the lowest p-value <= **alpha_crit (= 0.05)**
# - continue until no predictor can be added

library(dplyr)
library(broom)
data_fwd = data_full
# str(data_fwd)
attach(data_fwd)

# step 1, exam p.values for each of log_length_of_stay ~ predictor

fwd_1 = sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ x))$p.value)
fwd_p1 = plyr::ldply(fwd_1, rbind)[,-2]

# step 2, add age, log_length_of_stay ~ age + predictor
fwd_2 = sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + x))$p.value)
fwd_p2 = plyr::ldply(fwd_2, rbind)[,-(2:3)]

# step 3, add evisit, log_length_of_stay ~ age + evisit + predictor
fwd_3 = sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + x))$p.value)
fwd_p3 = plyr::ldply(fwd_3, rbind)[,-(2:4)]

# step 4, add respirationrate_transformed, log_length_of_stay ~ age + evisit + respirationrate_transformed + predictor
fwd_4 = sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + respirationrate_transformed + x))$p.value)
fwd_p4 = plyr::ldply(fwd_4, rbind)[,-(2:5)]

# step 5, add heartrate_transformed, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + predictor
fwd_5 = sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + respirationrate_transformed + heartrate_transformed + x))$p.value)
fwd_p5 = plyr::ldply(fwd_5, rbind)[,-(2:6)]

# step 6, add mean_arterial_pressure, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + predictor
fwd_6 = 
sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
respirationrate_transformed + 
heartrate_transformed + 
mean_arterial_pressure + x))$p.value) 
fwd_p6 = plyr::ldply(fwd_6, rbind)[,-(2:7)]

# step 7, add cindex, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + predictor
fwd_7 = 
sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
respirationrate_transformed + 
heartrate_transformed + 
mean_arterial_pressure + 
cindex + x))$p.value) 
fwd_p7 = plyr::ldply(fwd_7, rbind)[,-(2:10)]

# step 8, add temperature_cat, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + temperature_cat+ predictor
fwd_8 = 
  sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
                                         respirationrate_transformed + 
                                         heartrate_transformed + 
                                         mean_arterial_pressure + 
                                         cindex + temperature_cat + x))$p.value) 
fwd_p8 = plyr::ldply(fwd_8, rbind)[,-(2:12)]

# step 9, add is30dayreadmit, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + temperature_cat + 
# is30dayreadmit +predictor
fwd_9 = 
  sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
                                         respirationrate_transformed + 
                                         heartrate_transformed + 
                                         mean_arterial_pressure + 
                                         cindex + temperature_cat + is30dayreadmit + x))$p.value) 
fwd_p9 = plyr::ldply(fwd_9, rbind)[,-(2:13)]

# step 10, add insurancetype, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + temperature_cat + 
# is30dayreadmit + insurancetype + predictor
fwd_10 = 
  sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
                                         respirationrate_transformed + 
                                         heartrate_transformed + 
                                         mean_arterial_pressure + 
                                         cindex + temperature_cat + is30dayreadmit + insurancetype + x))$p.value) 
fwd_p10 = plyr::ldply(fwd_10, rbind)[,-(2:15)]

# step 11, add maritalstatus, log_length_of_stay ~ age + evisit + 
# respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + temperature_cat + 
# is30dayreadmit + insurancetype + maritalstatus + predictor
fwd_11 = 
  sapply(data_fwd, function(x) tidy(lm(log_length_of_stay ~ age + evisit + 
                                         respirationrate_transformed + 
                                         heartrate_transformed + 
                                         mean_arterial_pressure + 
                                         cindex + temperature_cat + is30dayreadmit + insurancetype + 
                                         maritalstatus + x))$p.value) 
fwd_p11 = plyr::ldply(fwd_11, rbind)[,-(2:16)]

# model: life_exp ~ murder + hs_grad + log_population + frost
fwd_fit = lm(log_length_of_stay ~ age + evisit + 
               respirationrate_transformed + heartrate_transformed + mean_arterial_pressure + cindex + temperature_cat + 
               is30dayreadmit + insurancetype + maritalstatus)
summary(fwd_fit)
detach(data_fwd)

