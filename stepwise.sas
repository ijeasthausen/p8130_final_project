PROC IMPORT OUT = data
            DATAFILE= "/folders/myfolders/bm1_sas/cleaned_data_for_analyses.csv"
            DBMS=CSV REPLACE;                                     
     GETNAMES=YES;
     DATAROW=2; 
RUN;
*stepwise selection with slstay = 0.05;
*variables selected: is30dayreadmit cindex evisit age insurancetype temperature_cat 
heartrate_transformed respirationrate_transformed mean_arterial_pressure;
proc glmselect data=data;
class is30dayreadmit(ref='0') cindex(ref="normal") evisit(ref='0') icu_flag(ref='0') 
gender race(ref="White") religion(ref="Christian") maritalstatus insurancetype(ref="Private") 
o2sat_cat(ref="normal") temperature_cat(ref="normal"); 
model log_length_of_stay = is30dayreadmit cindex  
evisit icu_flag age gender race religion 
maritalstatus insurancetype bmi 
o2sat_cat temperature_cat heartrate_transformed
respirationrate_transformed mean_arterial_pressure /
selection=stepwise(select = sl) slstay = 0.05 stats = all;
run; 

*stepwise selection with slstay = 0.10;
*variables selected: is30dayreadmit cindex evisit age gender marital status insurancetype temperature_cat 
heartrate_transformed respirationrate_transformed mean_arterial_pressure;
proc glmselect data=data;
class is30dayreadmit(ref='0') cindex(ref="normal") evisit(ref='0') icu_flag(ref='0') 
gender race(ref="White") religion(ref="Christian") maritalstatus insurancetype(ref="Private") 
o2sat_cat(ref="normal") temperature_cat(ref="normal"); 
model log_length_of_stay = is30dayreadmit cindex  
evisit icu_flag age gender race religion 
maritalstatus insurancetype bmi 
o2sat_cat temperature_cat heartrate_transformed
respirationrate_transformed mean_arterial_pressure /
selection=stepwise(select = sl) slstay = 0.10 stats = all;
run; 




proc glmselect data=no_outliers;
class is30dayreadmit(ref='0') cindex(ref="normal") evisit(ref='0') icu_flag(ref='0') 
gender race(ref="White") religion(ref="Christian") maritalstatus insurancetype(ref="Private") 
o2sat_cat(ref="normal") temperature_cat(ref="normal"); 
model log_length_of_stay = is30dayreadmit cindex  
evisit icu_flag age gender race religion 
maritalstatus insurancetype bmi 
o2sat_cat temperature_cat heartrate_transformed
respirationrate_transformed mean_arterial_pressure /
selection=stepwise (select = sl) slstay = 0.05 stats = all;
run; 

proc glmselect data=no_outliers;
class is30dayreadmit(ref='0') cindex(ref="normal") evisit(ref='0') icu_flag(ref='0') 
gender race(ref="White") religion(ref="Christian") maritalstatus insurancetype(ref="Private") 
o2sat_cat(ref="normal") temperature_cat(ref="normal"); 
model log_length_of_stay = is30dayreadmit cindex  
evisit icu_flag age gender race religion 
maritalstatus insurancetype bmi 
o2sat_cat temperature_cat heartrate_transformed
respirationrate_transformed mean_arterial_pressure /
selection=forward (select = sl) slstay = 0.05 stats = all;
run; 

proc glmselect data=no_outliers;
class is30dayreadmit(ref='0') cindex(ref="normal") evisit(ref='0') icu_flag(ref='0') 
gender race(ref="White") religion(ref="Christian") maritalstatus insurancetype(ref="Private") 
o2sat_cat(ref="normal") temperature_cat(ref="normal"); 
model log_length_of_stay = is30dayreadmit cindex  
evisit icu_flag age gender race religion 
maritalstatus insurancetype bmi 
o2sat_cat temperature_cat heartrate_transformed
respirationrate_transformed mean_arterial_pressure /
selection=backward (select = sl) slstay = 0.05 stats = all;
run;

