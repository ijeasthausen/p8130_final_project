# lib
library(tidyverse)
library(leaps)

#load the data
data_full = read.csv("./data/cleaned_data_for_analyses.csv") %>% 
  select(-X, -postalcode,-facilityname,-facilityzip, -patientid, -visitid, -date) %>% 
  mutate(is30dayreadmit = as.factor(is30dayreadmit), icu_flag = as.factor(icu_flag))

# fit
fit_bwd = lm(formula = log_length_of_stay ~ is30dayreadmit + evisit + age +
               gender + maritalstatus + insurancetype + heartrate_transformed +
               respirationrate_transformed + mean_arterial_pressure, data = data_full)

# delete NAs
data_test = data_full %>% na.omit()

# Cp method
leaps(x = model.matrix(fit_bwd)[,-1],
      y = data_test$log_length_of_stay,
      method = "Cp") %>% 
  faraway::Cpplot()

# Adjusted R2 method
leaps(x = model.matrix(fit_bwd)[,-1],
      y = data_test$log_length_of_stay,
      method = "adjr2") %>% 
  faraway::maxadjr(8)

# best function
best <- function(model, ...)
{
    subsets <- regsubsets(formula(model), model.frame(model), ..., nvmax = 10)
    subsets <- with(summary(subsets),
    cbind(n_predictor = as.numeric(rownames(which)), cp, adjr2, bic, rss, rsq, which))
    return(subsets)
}
# Select the 'best' 2 models of each model size
best_res = round(best(fit_bwd, nbest = 1), 4) %>% as.tibble()

# plot cp~p
plot(x = best_res$n_predictor + 1, y = best_res$cp)
abline(a = 0, b = 1, col="purple")

