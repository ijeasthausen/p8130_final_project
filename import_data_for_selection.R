#############################
# import data for selection #
#############################

library(tidyverse)
data_bwd = read.csv("./data/cleaned_data_for_analyses.csv") %>% 
  select(-X, -postalcode,-facilityname,-facilityzip, -patientid, -visitid, -date) %>% 
  mutate(is30dayreadmit = as.factor(is30dayreadmit), icu_flag = as.factor(icu_flag))
str(data_bwd)