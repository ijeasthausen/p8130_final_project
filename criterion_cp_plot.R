#　simple code to plot Cp values

library(leaps)

data_cp = data_bwd %>% select(log_length_of_stay,
is30dayreadmit,
evisit,
age,
gender,
maritalstatus,
insurancetype,
heartrate_transformed,
respirationrate_transformed,
mean_arterial_pressure) %>%
distinct()
leaps(x = data_cp[,-1],
y = data_cp$log_length_of_stay,
method = "Cp") %>%
Cpplot()