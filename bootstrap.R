library(tidyverse)

# load the data
data_bootstrap = read.csv("./data/data_no_outliers.csv") %>% 
  select(-X, -postalcode,-facilityname,-facilityzip, -patientid, -visitid, -date) %>% 
  mutate(is30dayreadmit = as.factor(is30dayreadmit), icu_flag = as.factor(icu_flag)) %>% 
  select(log_length_of_stay,
         is30dayreadmit,
         cindex,
         evisit,
         age,
         gender,
         maritalstatus,
         temperature_cat,
         insurancetype,
         heartrate_transformed, 
         respirationrate_transformed,
         mean_arterial_pressure)
  
# final model
# model_sw_0.10 = lm(log_length_of_stay ~ is30dayreadmit + cindex 
#                       + evisit + age + gender + maritalstatus + temperature_cat 
#                       + insurancetype + heartrate_transformed 
#                       + respirationrate_transformed 
#                       + mean_arterial_pressure)

# bootstrap to obtain the distribution
# raw code
boot_res1 = 
  data_bootstrap %>% 
  modelr::bootstrap(n = 10000) %>% 
  mutate(models = map(strap, ~lm(log_length_of_stay ~ ., data = .x) ),
         results = map(models, broom::tidy)) %>% 
  select(-strap, -models) %>% 
  unnest() %>% 
  select(id = `.id`, term, estimate) %>% 
  spread(key = term, value = estimate)
# boot function
boot.fn = function(data, index){
  return(coef(lm(log_length_of_stay ~ ., data = data, subset = index)))
}
boot_res2 = boot(data_bootstrap, boot.fn, 10000)