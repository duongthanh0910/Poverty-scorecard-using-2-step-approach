# scorecard: Rural
# 23.02.12


1.1 #Fundamental setting up
### install necessary packages

install.packages("scorecard")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("ClusOfVar")
install.packages("data.table")
install.packages("caret") # create summary with table by command sumtable/st
install.packages("vtable")
install.packages("dplyr")
install.packages("tidyr")
install.packages("splitTools")  #stratified random sampling

library(vtable)
library(haven)
library(data.table)
library(tidyverse) 
library(scorecard)
library(Hmisc)
library(ClustOfVar)
library(dplyr)
library(tidyr)
library(splitTools)

#Load data from STATA data set
load("data_23.04.10_RURAL_scorecard.RData") 

X00_2023_02_12_RURAL_selected_var_R1_R2 -> df.01_original 

1.1.4 # change class of variables
df.02_check_class <-data.frame(sapply(df.01_original, class))  #check the type to Factor

#nummeric variables
df_num.var <- c("poor_0m7","poor_0m7_1m","av_income","nonagri_labor_n","aqua_area", "cow_buf_hors", "HouseSize_mean",
                "mBike2_S", "house_size_total", "electr_kw","poultry_n","pig_goat_n","peren_land",
                "annual_land", "garden_area", "pensioner_n", "hh_size", "depen_16_s") 
df.03_classed <- df.01_original %>% mutate_at(df_num.var, as.numeric)
#factor variables
df_fact.var <- c("region18_6b", "true_poverty" ,"poor_16", "WorkFar_h", "livestock", "Car1_H", "mBike2_H" ,
                  "BoatM4_H" ,"Tivi15_H", "Stereo17_H", "Fridge22_H" ,"AirC23_H", "WashM24_H", "WaterH26_H",
                  "StoveG27_H", "e_cooker_pot", "MicroW34_H", "House_Type2",  "HouseQuality",
                   "pillar_material", "toilet_type7", "Water_sh4e", "hand_wash_place","aqua_act","wall_material",
                  "civil_servant", "non_agri", "voc_edu_dip" ,"edu_diploma", "internet" ,"latrine_q10",
                   "water_sourc", "pillar_unstable", "handwash_soap")
df.03_classed <- df.03_classed %>% mutate_at(df_fact.var, as.factor)
#check the class again
df.04_check_class <-data.frame(sapply(df.03_classed, class))  

1.1.5  # remove old ID var and create a new one
df.05_newID <- df.03_classed %>% select(-id) 
df.05_newID$id <- 1:nrow(df.05_newID)   

1.2 #CLEAN DATA:  set class of variables + replace missing value
1.2.1 # remove var has over 10% missing values
df.06_filterNA <- df.05_newID[, !sapply(df.05_newID, function(x) mean(is.na(x)) > 0.1)]  

1.2.2 #Replace NA value 
# missing values are replaced by 02 ways:
## nummeric variables: NA are replaced by mean values
## category/dummy variables: NA are replaced so that the new proportion of var is similar to its original proportion.

# check the NA rate
#create fuction to calculate NA rate
check_na <- function(x) {x %>% is.na() %>% sum()*100 / length(x)} 
#save NA result (% NA)                                       
df.07_checkNA <- sapply(df.06_filterNA, check_na) %>% round(3) %>% data.frame  

1.2.3 #Create formular to change NA
# Function replaces NA by mean:
replace_by_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

# generate a function imputes NA observations for categorical variables: 
replace_na_categorical <- function(x) {
  x %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) ->> my_df
  
  n_obs <- sum(my_df$Freq)
  pop <- my_df$. %>% as.character()
  set.seed(29)
  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
  return(x)
}

1.2.4 #Change NA by combining the two functions: 

df.07_NAreplaced <- df.06_filterNA %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    mBike2_H = case_when(mBike2_H == "" ~ NA_character_, TRUE ~ mBike2_H),
    Tivi15_H = case_when(Tivi15_H == "" ~ NA_character_, TRUE ~ Tivi15_H),
    Fridge22_H = case_when(Fridge22_H == "" ~ NA_character_, TRUE ~ Fridge22_H),
    WashM24_H = case_when(WashM24_H == "" ~ NA_character_, TRUE ~ WashM24_H),
    WaterH26_H = case_when(WaterH26_H == "" ~ NA_character_, TRUE ~ WaterH26_H),
    StoveG27_H = case_when(StoveG27_H == "" ~ NA_character_, TRUE ~ StoveG27_H),
    House_Type2 = case_when(House_Type2 == "" ~ NA_character_, TRUE ~ House_Type2),
    HouseQuality = case_when(HouseQuality == "" ~ NA_character_, TRUE ~ HouseQuality),
    pillar_unstable = case_when(pillar_unstable == "" ~ NA_character_, TRUE ~ pillar_unstable),
    Car1_H = case_when(Car1_H == "" ~ NA_character_, TRUE ~ Car1_H),
    BoatM4_H = case_when(BoatM4_H == "" ~ NA_character_, TRUE ~ BoatM4_H),
    Stereo17_H = case_when(Stereo17_H == "" ~ NA_character_, TRUE ~ Stereo17_H),
    AirC23_H = case_when(AirC23_H == "" ~ NA_character_, TRUE ~ AirC23_H),
    e_cooker_pot = case_when(e_cooker_pot == "" ~ NA_character_, TRUE ~ e_cooker_pot),
    MicroW34_H = case_when(MicroW34_H == "" ~ NA_character_, TRUE ~ MicroW34_H),
    pillar_material = case_when(pillar_material == "" ~ NA_character_, TRUE ~ pillar_material),
    wall_material = case_when(wall_material == "" ~ NA_character_, TRUE ~ wall_material),
    voc_edu_dip = case_when(voc_edu_dip == "" ~ NA_character_, TRUE ~ voc_edu_dip),
    edu_diploma = case_when(edu_diploma == "" ~ NA_character_, TRUE ~ edu_diploma)
  ) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.numeric, replace_by_mean) %>% 
  mutate_if(is.factor, replace_na_categorical)
#review result
   df.07_NAresult <- sapply(df.07_NAreplaced, check_na) %>% round(2) %>% data.frame    

1.2.5 #Change the class of some variable to integer (no decimal)
df_integ.var <- c("av_income","nonagri_labor_n","aqua_area", "cow_buf_hors", "HouseSize_mean",
                    "mBike2_S", "house_size_total", "electr_kw","poultry_n","pig_goat_n","peren_land",
                    "annual_land", "garden_area", "pensioner_n", "hh_size", "depen_16_s")
df.08_classed2 <- df.07_NAreplaced %>% mutate_at(df_integ.var, as.integer) #save
#check the data type again
df.08_integ.c <-data.frame(sapply(df.08_classed2, class)) 
     
   
1.3. #SCORING VN SCORECARD
1.3.1 #select variables for scoring and calculate accuracy performance
#create a new dataframe that includes only the variables needed for scoring
df.09_vn <- df.08_classed2 %>% select(id, poor_0m7, poor_0m7_1m, true_poverty, region18_6b, hh_size,
                                       depen_16_s, edu_diploma, voc_edu_dip, civil_servant, non_agri,
                                       pensioner_n, wall_material, pillar_material, HouseSize_mean, electr_kw, 
                                       water_sourc, latrine_q10, Tivi15_H, Stereo17_H, Car1_H, mBike2_H,
                                       Fridge22_H, AirC23_H, WashM24_H, WaterH26_H, MicroW34_H, BoatM4_H, 
                                       garden_area, annual_land, peren_land, aqua_area, cow_buf_hors, pig_goat_n, 
                                       poultry_n, aqua_act
                                       )  
# convert all var to numeric type to calculate score in ease
df.09_vn[] <- lapply(df.09_vn, function(x) {     
  if (is.factor(x)) {                        
    as.numeric(levels(x))[x]
  } else {
    as.numeric(x)
  }
})
1.3.2 #scoring for 30 questions
#q1 -number of member in household, (all member = dependent => 0 points)
df.09_vn$q1  <- ifelse(df.09_vn$hh_size == df.09_vn$depen_16_s, 0, # this condition is prority
                   ifelse(df.09_vn$hh_size == 1 & df.09_vn$region18_6b %in% c(3, 5), 70,  #if hh_size=1 & region18_6b = 3 or 5 => q1 = 70
                   ifelse(df.09_vn$hh_size == 1 & df.09_vn$region18_6b == 6, 65,          #if hh_size=1 & region18_6b = 6 or 5 => q1 = 65
         ifelse(df.09_vn$hh_size == 2 & df.09_vn$region18_6b == 2, 65,
         ifelse(df.09_vn$hh_size == 2 & df.09_vn$region18_6b == 1, 60,
         ifelse(df.09_vn$hh_size == 2 & df.09_vn$region18_6b %in% c(5, 6), 55,  #if hh_size=1 & region18_6b = 3 or 5 => q1 = 55
          ifelse(df.09_vn$hh_size == 2 & df.09_vn$region18_6b %in% c(3, 4), 50,  # similar
          ifelse(df.09_vn$hh_size == 3 & df.09_vn$region18_6b == 2, 50,
          ifelse(df.09_vn$hh_size == 3 & df.09_vn$region18_6b %in% c(5, 6), 45,
          ifelse(df.09_vn$hh_size == 3 & df.09_vn$region18_6b %in% c(1, 3, 4), 40,
          ifelse(df.09_vn$hh_size == 4 & df.09_vn$region18_6b %in% c(4, 6), 35,
          ifelse(df.09_vn$hh_size == 4 & df.09_vn$region18_6b %in% c(1, 2, 3, 5), 30,
          ifelse(df.09_vn$hh_size == 5 & df.09_vn$region18_6b == 6, 30,
          ifelse(df.09_vn$hh_size == 5 & df.09_vn$region18_6b %in% c(1, 2, 5), 20,
          ifelse(df.09_vn$hh_size == 6 & df.09_vn$region18_6b == 6, 20,
            ifelse(df.09_vn$hh_size == 5 & df.09_vn$region18_6b %in% c(3, 4), 15,
            ifelse(df.09_vn$hh_size == 6 & df.09_vn$region18_6b == 1, 15,
            ifelse(df.09_vn$hh_size == 6 & df.09_vn$region18_6b %in% c(3, 2, 5), 10,
            ifelse(df.09_vn$hh_size == 6 & df.09_vn$region18_6b == 4, 5,
              ifelse(df.09_vn$hh_size >= 7, 0,
              ifelse(df.09_vn$hh_size == 1 & df.09_vn$region18_6b %in% c(1, 2, 4), 75, NA)))))))))))))))))))))
#q2 - number of dependents in household: out of labor age +disability+children (<16 year olds)
df.09_vn$q2  <- ifelse(df.09_vn$depen_16_s==0 & (df.09_vn$region18_6b==1 | df.09_vn$region18_6b==3), 15,
                       ifelse(df.09_vn$depen_16_s==0 & df.09_vn$region18_6b==2, 10,
                       ifelse(df.09_vn$depen_16_s==0 & (df.09_vn$region18_6b==4 | df.09_vn$region18_6b==5 | df.09_vn$region18_6b==6), 20,
                             ifelse(df.09_vn$depen_16_s==1 & (df.09_vn$region18_6b==1 | df.09_vn$region18_6b==2 | df.09_vn$region18_6b==5), 5,
                             ifelse(df.09_vn$depen_16_s==1 & df.09_vn$region18_6b==3, 10,
                             ifelse(df.09_vn$depen_16_s==1 & (df.09_vn$region18_6b==4 | df.09_vn$region18_6b==6), 15,
                       ifelse(df.09_vn$depen_16_s>=2 | is.na(df.09_vn$depen_16_s), 0, NA)))))))
#q3 Highest education diploma?
df.09_vn$q3  <- ifelse((df.09_vn$edu_diploma >= 7| df.09_vn$voc_edu_dip >= 7) & df.09_vn$region18_6b == 5, 20,
                       ifelse((df.09_vn$edu_diploma >= 7| df.09_vn$voc_edu_dip >= 7) & df.09_vn$region18_6b %in% c(1,4), 10,
                       ifelse((df.09_vn$edu_diploma >= 7| df.09_vn$voc_edu_dip >= 7) & df.09_vn$region18_6b %in% c(2,3,6), 15,
                       ifelse((df.09_vn$voc_edu_dip == 5| df.09_vn$voc_edu_dip == 6) & df.09_vn$region18_6b %in% c(1,3,4,6), 5,
                       ifelse((df.09_vn$voc_edu_dip == 5| df.09_vn$voc_edu_dip == 6) & df.09_vn$region18_6b == 2, 10,
                       ifelse((df.09_vn$voc_edu_dip == 5| df.09_vn$voc_edu_dip == 6) & df.09_vn$region18_6b == 5, 15,
                       ifelse(df.09_vn$edu_diploma == 3 & df.09_vn$region18_6b %in% c(1,2,3), 0,
                       ifelse(df.09_vn$edu_diploma == 3 & df.09_vn$region18_6b %in% c(4,6), 5,
                       ifelse(df.09_vn$edu_diploma == 3 & df.09_vn$region18_6b == 5, 10, 
                       ifelse((df.09_vn$edu_diploma < 3| df.09_vn$voc_edu_dip < 5), 0, NA))))))))))
#q4 - Number of civil servant or non-agri labors in household?
df.09_vn$q4  <-  ifelse(df.09_vn$civil_servant == 1 & df.09_vn$region18_6b %in% c(1,3,5), 25,
                        ifelse(df.09_vn$civil_servant == 1 & df.09_vn$region18_6b %in% c(2), 45,
                        ifelse(df.09_vn$civil_servant == 1 & df.09_vn$region18_6b %in% c(4), 30,
                        ifelse(df.09_vn$civil_servant == 1 & df.09_vn$region18_6b %in% c(6), 20,
                               ifelse(df.09_vn$non_agri == 1 & df.09_vn$region18_6b %in% c(1,3), 20,
                               ifelse(df.09_vn$non_agri == 1 & df.09_vn$region18_6b %in% c(2), 25,
                               ifelse(df.09_vn$non_agri == 1 & df.09_vn$region18_6b %in% c(4), 15,
                               ifelse(df.09_vn$non_agri == 1 & df.09_vn$region18_6b %in% c(5), 5,
                               ifelse(df.09_vn$non_agri == 1 & df.09_vn$region18_6b %in% c(6), 10,
                               ifelse((df.09_vn$non_agri == 0 & df.09_vn$civil_servant==0), 0, NA))))))))))
#q5 number of pensioners?
df.09_vn$q5  <-  ifelse(df.09_vn$pensioner_n == 1 & df.09_vn$region18_6b %in% c(1), 20,
                        ifelse(df.09_vn$pensioner_n == 1 & df.09_vn$region18_6b %in% c(2), 30, 
                        ifelse(df.09_vn$pensioner_n == 1 & df.09_vn$region18_6b %in% c(4,3,6), 25,
                        ifelse(df.09_vn$pensioner_n == 1 & df.09_vn$region18_6b %in% c(5), 15,
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(1), 35,
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(2), 50,
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(3), 45, 
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(4), 30,
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(5), 25,
                               ifelse(df.09_vn$pensioner_n >= 2 & df.09_vn$region18_6b %in% c(6), 40,
                                      ifelse(df.09_vn$pensioner_n <1, 0, NA)))))))))))
#q6.1 type of wall material?
df.09_vn$q6.1  <-  ifelse(df.09_vn$wall_material<= 3 & df.09_vn$region18_6b %in% c(1,5,6), 0,
                          ifelse(df.09_vn$wall_material<= 3 & df.09_vn$region18_6b %in% c(2), 5,
                          ifelse(df.09_vn$wall_material<= 3 & df.09_vn$region18_6b %in% c(3), 20,
                          ifelse(df.09_vn$wall_material<= 3 & df.09_vn$region18_6b %in% c(4), 10,
                                 ifelse(df.09_vn$wall_material > 3 , 0, NA)))))
#q6.2 type of pillar material?
df.09_vn$q6.2  <-  ifelse(df.09_vn$pillar_material<= 3 & df.09_vn$region18_6b %in% c(1), 15,
                          ifelse(df.09_vn$pillar_material<= 3 & df.09_vn$region18_6b %in% c(2,4), 5,
                          ifelse(df.09_vn$pillar_material<= 3 & df.09_vn$region18_6b %in% c(3), 0,
                          ifelse(df.09_vn$pillar_material<= 3 & df.09_vn$region18_6b %in% c(5,6), 10,
                                ifelse(df.09_vn$pillar_material > 3 , 0, NA)))))
#q7 housesize mean?
 df.09_vn$q7  <-  ifelse((df.09_vn$HouseSize_mean>=8 &df.09_vn$HouseSize_mean<20) & df.09_vn$region18_6b %in% c(3,2,1), 0,
               ifelse((df.09_vn$HouseSize_mean>=8 &df.09_vn$HouseSize_mean<20) & df.09_vn$region18_6b %in% c(4,6), 15,
               ifelse((df.09_vn$HouseSize_mean>=8 &df.09_vn$HouseSize_mean<20) & df.09_vn$region18_6b %in% c(5), 10,
                       ifelse((df.09_vn$HouseSize_mean>=20&df.09_vn$HouseSize_mean<30) & df.09_vn$region18_6b %in% c(1), 5,
                        ifelse((df.09_vn$HouseSize_mean>=20&df.09_vn$HouseSize_mean<30) & df.09_vn$region18_6b %in% c(2,3), 10,
                        ifelse((df.09_vn$HouseSize_mean>=20&df.09_vn$HouseSize_mean<30) & df.09_vn$region18_6b %in% c(4,6), 25,
                        ifelse((df.09_vn$HouseSize_mean>=20&df.09_vn$HouseSize_mean<30) & df.09_vn$region18_6b %in% c(5), 15,
                              ifelse((df.09_vn$HouseSize_mean>=30&df.09_vn$HouseSize_mean<40) & df.09_vn$region18_6b %in% c(1), 5,
                              ifelse((df.09_vn$HouseSize_mean>=30&df.09_vn$HouseSize_mean<40) & df.09_vn$region18_6b %in% c(2,3,5), 15,
                              ifelse((df.09_vn$HouseSize_mean>=30&df.09_vn$HouseSize_mean<40) & df.09_vn$region18_6b %in% c(4), 35,
                              ifelse((df.09_vn$HouseSize_mean>=30&df.09_vn$HouseSize_mean<40) & df.09_vn$region18_6b %in% c(6), 30,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(1), 15,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(2), 35,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(3), 25,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(4), 45,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(5), 20,
                                     ifelse((df.09_vn$HouseSize_mean>=40) & df.09_vn$region18_6b %in% c(6), 40,
                        ifelse(df.09_vn$HouseSize_mean <8 , 0, NA))))))))))))))))))
#q8 volume of electricity using?
 df.09_vn$q8  <-  ifelse((df.09_vn$electr_kw>=25 &df.09_vn$electr_kw<=49) & df.09_vn$region18_6b %in% c(1),  30,
                        ifelse((df.09_vn$electr_kw>=25 &df.09_vn$electr_kw<=49) & df.09_vn$region18_6b %in% c(2,4),20,
                        ifelse((df.09_vn$electr_kw>=25 &df.09_vn$electr_kw<=49) & df.09_vn$region18_6b %in% c(3,6),25,
                        ifelse((df.09_vn$electr_kw>=25 &df.09_vn$electr_kw<=49) & df.09_vn$region18_6b %in% c(5),  10,
                               ifelse((df.09_vn$electr_kw>=50 &df.09_vn$electr_kw<=99) & df.09_vn$region18_6b %in% c(1), 40,
                               ifelse((df.09_vn$electr_kw>=50 &df.09_vn$electr_kw<=99) & df.09_vn$region18_6b %in% c(2), 35,
                               ifelse((df.09_vn$electr_kw>=50 &df.09_vn$electr_kw<=99) & df.09_vn$region18_6b %in% c(3), 45,
                               ifelse((df.09_vn$electr_kw>=50 &df.09_vn$electr_kw<=99) & df.09_vn$region18_6b %in% c(5), 20,
                               ifelse((df.09_vn$electr_kw>=50 &df.09_vn$electr_kw<=99) & df.09_vn$region18_6b %in% c(4,6),30,
                        ifelse((df.09_vn$electr_kw>=100 &df.09_vn$electr_kw<=149) & df.09_vn$region18_6b %in% c(1,2),50, 
                        ifelse((df.09_vn$electr_kw>=100 &df.09_vn$electr_kw<=149) & df.09_vn$region18_6b %in% c(4,6),40, 
                        ifelse((df.09_vn$electr_kw>=100 &df.09_vn$electr_kw<=149) & df.09_vn$region18_6b %in% c(3),55, 
                        ifelse((df.09_vn$electr_kw>=100 &df.09_vn$electr_kw<=149) & df.09_vn$region18_6b %in% c(5),25, 
                               ifelse(df.09_vn$electr_kw>=150& df.09_vn$region18_6b %in% c(1,4),55, 
                               ifelse(df.09_vn$electr_kw>=150& df.09_vn$region18_6b %in% c(2),50, 
                               ifelse(df.09_vn$electr_kw>=150& df.09_vn$region18_6b %in% c(3),70, 
                               ifelse(df.09_vn$electr_kw>=150& df.09_vn$region18_6b %in% c(5),25, 
                               ifelse(df.09_vn$electr_kw>=150& df.09_vn$region18_6b %in% c(6),45, 
                        ifelse(df.09_vn$electr_kw < 25 , 0, NA)))))))))))))))))))
#q9 source of water using?
 df.09_vn$q9  <-  ifelse(df.09_vn$water_sourc ==1 & df.09_vn$region18_6b %in% c(1,4), 15,
                        ifelse(df.09_vn$water_sourc ==1 & df.09_vn$region18_6b %in% c(2,5), 20,
                        ifelse(df.09_vn$water_sourc ==1 & df.09_vn$region18_6b %in% c(3,6), 10,
                               ifelse(df.09_vn$water_sourc ==2 & df.09_vn$region18_6b %in% c(1,4), 10,
                               ifelse(df.09_vn$water_sourc ==2 & df.09_vn$region18_6b %in% c(2,5), 15,
                               ifelse(df.09_vn$water_sourc ==2 & df.09_vn$region18_6b %in% c(3,6), 5,
                        ifelse(df.09_vn$water_sourc ==3 & df.09_vn$region18_6b %in% c(1,4), 10,
                        ifelse(df.09_vn$water_sourc ==3 & df.09_vn$region18_6b %in% c(2,6), 5,
                        ifelse(df.09_vn$water_sourc ==3 & df.09_vn$region18_6b %in% c(3,5), 0,
                               ifelse(df.09_vn$water_sourc ==4, 0, NA))))))))))
#q10 type of latrine using?
df.09_vn$q10  <-  ifelse(df.09_vn$latrine_q10 ==1 & df.09_vn$region18_6b %in% c(1,2,3,6), 15,
                         ifelse(df.09_vn$latrine_q10 ==1 & df.09_vn$region18_6b %in% c(4,5), 20,
                                ifelse(df.09_vn$latrine_q10 ==2 & df.09_vn$region18_6b %in% c(1), 5,
                                ifelse(df.09_vn$latrine_q10 ==2 & df.09_vn$region18_6b %in% c(4,2,3,6), 10,
                                ifelse(df.09_vn$latrine_q10 ==2 & df.09_vn$region18_6b %in% c(5), 15,
                        ifelse(df.09_vn$latrine_q10 ==3, 0, NA))))))
#q11.1 owing a TV or not?
df.09_vn$q11.1  <-  ifelse(df.09_vn$Tivi15_H ==1 & df.09_vn$region18_6b %in% c(1,4), 10,
                           ifelse(df.09_vn$Tivi15_H ==1 & df.09_vn$region18_6b %in% c(2,6), 15,
                           ifelse(df.09_vn$Tivi15_H ==1 & df.09_vn$region18_6b %in% c(3), 5,
                           ifelse(df.09_vn$Tivi15_H ==1 & df.09_vn$region18_6b %in% c(5), 20,
                                  ifelse(df.09_vn$Tivi15_H ==0, 0, NA)))))
#q11.2 owing a STEREO or not?
df.09_vn$q11.2  <-  ifelse(df.09_vn$Stereo17_H ==1 & df.09_vn$region18_6b %in% c(1,3,6), 10,
                           ifelse(df.09_vn$Stereo17_H ==1 & df.09_vn$region18_6b %in% c(2,4), 0,
                           ifelse(df.09_vn$Stereo17_H ==1 & df.09_vn$region18_6b %in% c(5), 5,
                                 ifelse(df.09_vn$Stereo17_H ==0, 0, NA))))
#q11.3 owing a CAR or not?
df.09_vn$q11.3  <-  ifelse(df.09_vn$Car1_H ==1 , 50,
                                  ifelse(df.09_vn$Car1_H ==0, 0, NA))
#q11.4 owing a Motor Bike or not?
df.09_vn$q11.4  <-  ifelse(df.09_vn$mBike2_H ==1 & df.09_vn$region18_6b %in% c(1,2), 15,
                           ifelse(df.09_vn$mBike2_H ==1 & df.09_vn$region18_6b %in% c(3,6), 20,
                           ifelse(df.09_vn$mBike2_H ==1 & df.09_vn$region18_6b %in% c(5), 30,
                           ifelse(df.09_vn$mBike2_H ==1 & df.09_vn$region18_6b %in% c(4), 25,
                                  ifelse(df.09_vn$mBike2_H ==0, 0, NA)))))
#q11.5 owing a FRIDGE or not?
df.09_vn$q11.5  <-  ifelse(df.09_vn$Fridge22_H ==1 & df.09_vn$region18_6b %in% c(3,4), 15,
                           ifelse(df.09_vn$Fridge22_H ==1 & df.09_vn$region18_6b %in% c(1,2,5,6), 10,
                                  ifelse(df.09_vn$Fridge22_H ==0, 0, NA)))
#q11.6 owing a AIR CONDITIONER or not?
df.09_vn$q11.6  <-  ifelse(df.09_vn$AirC23_H ==1 & df.09_vn$region18_6b %in% c(4,2), 10,
                           ifelse(df.09_vn$AirC23_H ==1 & df.09_vn$region18_6b %in% c(3,5), 15,
                           ifelse(df.09_vn$AirC23_H ==1 & df.09_vn$region18_6b %in% c(6), 25,
                           ifelse(df.09_vn$AirC23_H ==1 & df.09_vn$region18_6b %in% c(1), 20,
                                  ifelse(df.09_vn$AirC23_H ==0, 0, NA)))))
#q11.7 owing a WASHING MACHINE or not?
df.09_vn$q11.7  <-  ifelse(df.09_vn$WashM24_H ==1 & df.09_vn$region18_6b %in% c(1,2), 10,
                           ifelse(df.09_vn$WashM24_H ==1 & df.09_vn$region18_6b %in% c(3,4,5,6), 15,
                                  ifelse(df.09_vn$WashM24_H ==0, 0, NA)))
#q11.8 owing a WATER HEATER or not?
df.09_vn$q11.8  <-  ifelse(df.09_vn$WaterH26_H ==1 & df.09_vn$region18_6b %in% c(1,3,4), 10,
                           ifelse(df.09_vn$WaterH26_H ==1 & df.09_vn$region18_6b %in% c(2,5,6), 15,
                                   ifelse(df.09_vn$WaterH26_H ==0, 0, NA)))
#q11.9 owing a MICRO WAVE or not?
df.09_vn$q11.9  <-  ifelse(df.09_vn$MicroW34_H ==1 & df.09_vn$region18_6b %in% c(2), 10,
                           ifelse(df.09_vn$MicroW34_H ==1 & df.09_vn$region18_6b %in% c(3,1,5,6), 15,
                           ifelse(df.09_vn$MicroW34_H ==1 & df.09_vn$region18_6b %in% c(4), 25,
                                  ifelse(df.09_vn$MicroW34_H ==0, 0, NA))))
#q11.10 owing a BOAT OR SHIP with motor ?
df.09_vn$q11.10  <-  ifelse(df.09_vn$BoatM4_H ==1 & df.09_vn$region18_6b %in% c(6), 10,
                            ifelse(df.09_vn$BoatM4_H ==1 & df.09_vn$region18_6b %in% c(2,1,5,4), 0,
                            ifelse(df.09_vn$BoatM4_H ==1 & df.09_vn$region18_6b %in% c(3), 25,
                                   ifelse(df.09_vn$BoatM4_H ==0, 0, NA))))
#q12.1 area of GARDEN?
df.09_vn$q12.1  <-  ifelse(df.09_vn$garden_area >=300 & df.09_vn$region18_6b %in% c(1,3,4,2,6), 5,
                           ifelse(df.09_vn$garden_area >=300& df.09_vn$region18_6b %in% c(5), 15,
                                 ifelse(df.09_vn$garden_area <300, 0, NA)))
#q12.2 area of ANNUAL CROP land ?
df.09_vn$q12.2  <-  ifelse(df.09_vn$annual_land >=5000 & df.09_vn$region18_6b %in% c(1,3,4,5), 5,
                           ifelse(df.09_vn$annual_land >=5000 & df.09_vn$region18_6b %in% c(2), 10,
                           ifelse(df.09_vn$annual_land >=5000 & df.09_vn$region18_6b %in% c(6), 15,
                                  ifelse(df.09_vn$annual_land <5000 , 0, NA))))
#q12.3 area of PERENNIAL CROP land ?
df.09_vn$q12.3  <-  ifelse((df.09_vn$peren_land>=1000 & df.09_vn$peren_land<5000) & df.09_vn$region18_6b %in% c(1,6,4,5), 5,
                           ifelse((df.09_vn$peren_land>=1000 & df.09_vn$peren_land<5000) & df.09_vn$region18_6b %in% c(2), 10,
                           ifelse((df.09_vn$peren_land>=1000 & df.09_vn$peren_land<5000) & df.09_vn$region18_6b %in% c(3), 15,
                                  ifelse(df.09_vn$peren_land >=5000 & df.09_vn$region18_6b %in% c(2,3), 20,
                                  ifelse(df.09_vn$peren_land >=5000 & df.09_vn$region18_6b %in% c(1,6), 10,
                                  ifelse(df.09_vn$peren_land >=5000 & df.09_vn$region18_6b %in% c(5,4), 15,
                           ifelse(df.09_vn$peren_land <1000 , 0, NA)))))))
#q12.4 area of AQUACULTURE land ?
df.09_vn$q12.4  <-  ifelse(df.09_vn$aqua_area >=5000 & df.09_vn$region18_6b %in% c(1,3,6), 15,
                           ifelse(df.09_vn$aqua_area >=5000 & df.09_vn$region18_6b %in% c(2,4), 20,
                           ifelse(df.09_vn$aqua_area >=5000 & df.09_vn$region18_6b %in% c(5), 0,
                                   ifelse(df.09_vn$aqua_area <5000 , 0, NA))))
#q13.1 Quantity of COW or HORSE or BUFFALO owned?
df.09_vn$q13.1  <-  ifelse(df.09_vn$cow_buf_hors==1 & df.09_vn$region18_6b %in% c(1,5,6), 0,
                           ifelse(df.09_vn$cow_buf_hors==1 & df.09_vn$region18_6b %in% c(2,4), 15,
                           ifelse(df.09_vn$cow_buf_hors==1 & df.09_vn$region18_6b %in% c(3), 10,
                                  ifelse(df.09_vn$cow_buf_hors>=2 & df.09_vn$region18_6b %in% c(2,4,5), 25,
                                  ifelse(df.09_vn$cow_buf_hors>=2 & df.09_vn$region18_6b %in% c(6), 20,
                                  ifelse(df.09_vn$cow_buf_hors>=2 & df.09_vn$region18_6b %in% c(1,3), 15,
                           ifelse(df.09_vn$cow_buf_hors <1 , 0, NA)))))))
#q13.2 Quantity of PIG or GOAT or SHEEP owned?
df.09_vn$q13.2  <-  ifelse((df.09_vn$pig_goat_n>=5 & df.09_vn$pig_goat_n<=10) & df.09_vn$region18_6b %in% c(1,5,4), 0,
                           ifelse((df.09_vn$pig_goat_n>=5 & df.09_vn$pig_goat_n<=10) & df.09_vn$region18_6b %in% c(3,6), 10,
                           ifelse((df.09_vn$pig_goat_n>=5 & df.09_vn$pig_goat_n<=10) & df.09_vn$region18_6b %in% c(2), 5,
                                  ifelse((df.09_vn$pig_goat_n>=11) & df.09_vn$region18_6b %in% c(4,2,3), 20,
                                  ifelse((df.09_vn$pig_goat_n>=11) & df.09_vn$region18_6b %in% c(1), 15,
                                  ifelse((df.09_vn$pig_goat_n>=11) & df.09_vn$region18_6b %in% c(5), 10,
                                  ifelse((df.09_vn$pig_goat_n>=11) & df.09_vn$region18_6b %in% c(6), 25,
                           ifelse(df.09_vn$pig_goat_n <5 , 0, NA))))))))
#q13.3 Quantity of POULTRY owned: chicken, duck, bird, goose?
df.09_vn$q13.3  <-  ifelse(df.09_vn$poultry_n >=100 & df.09_vn$region18_6b %in% c(2,3,4), 20,
                           ifelse(df.09_vn$poultry_n >=100 & df.09_vn$region18_6b %in% c(6), 25,
                           ifelse(df.09_vn$poultry_n >=100 & df.09_vn$region18_6b %in% c(5), 10,
                           ifelse(df.09_vn$poultry_n >=100 & df.09_vn$region18_6b %in% c(1), 15,
                                                ifelse(df.09_vn$poultry_n <100 , 0, NA)))))
#q13.4 engage in aquaculture activity or not ?
df.09_vn$q13.4  <-  ifelse(df.09_vn$aqua_act ==1 & df.09_vn$region18_6b %in% c(5,4), 0,
                           ifelse(df.09_vn$aqua_act ==1 & df.09_vn$region18_6b %in% c(2,1,3,6), 5,
                                    ifelse(df.09_vn$aqua_act ==0, 0, NA)))
1.3.3 #CALCULATE TOTAL SCORE
 #generate new row by sum other variables
df.09_vn$score_NA0 <- rowSums(df.09_vn[,37:65]) 

 #generate the variable of predict poverty status
df.09_vn <- df.09_vn %>% mutate(pred_poor = ifelse(score_NA0 <=120, "1. poor", "2. non-poor"))
df.09_vn <- df.09_vn %>% mutate(pred_n_poor = ifelse(score_NA0 >120 & score_NA0<=150, "1. n-poor", "2. non-n-poor"))
df.09_vn <- df.09_vn %>% mutate(pred_poverty = ifelse(score_NA0 <=120, "1. poor", 
                                                  ifelse(score_NA0 >120 & score_NA0<=150,"2. n-poor", "3. better-off")))

1.3.4 #Calculate all accuracy measure
1.3.4.1 #Estimate accuracy of predicted poor
df.10a_vn.poor <- data.frame(table(true_status = df.09_vn$poor_0m7, pred_status = df.09_vn$pred_poor))
df.10a_vn.poor <- df.10a_vn.poor %>% select(contains("Freq"))
df.10a_vn.poor <- df.10a_vn.poor %>% rename(poor = Freq) #rename

df.10a_vn.poor <- as.data.frame(t(df.10a_vn.poor), )    #convert row to column
names(df.10a_vn.poor)[1:4] <- c("C","A","D","B")       #rename columns for calculation

# Estimate accuracy performance
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (ABCD= C+A+D+B)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (TA  = (A+D)/(ABCD)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (PA  = (A)/(A+B)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (NPA = (D)/(C+D)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (U  = (B)/(A+B)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (L   = (C)/(A+B)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (PIE = ((A+C)/ABCD - (A+B)/ABCD)*100)
df.10a_vn.poor <- df.10a_vn.poor %>% mutate (BPAC= (PA-abs(U-L)))

df.10a_vn.poor <- as.data.frame(t(df.10a_vn.poor), row.names = FALSE)   # convert back to row
df.10a_vn.poor <- df.10a_vn.poor %>% mutate_if(is.numeric, round, digits = 2) #only use 2 digits

df.10b_vn.poor <- data.frame(true_status=c("non poor","poor","non poor","poor","ACCURACY MEASURES","Total Accuracy",
                                        "Poverty Accuracy","Non-poverty Accu","Under-coverage","Leakage","PIE","BPAC"),
                          pred_status = c("poor","poor","non poor","non poor","ABREVIATION","TA","PA","NPA","U","L","PIE","BPAC"),
                          value = c("C","A","D","B","CALCULATION (%)","(A+D)/ABCD","A/(A+B)","D/(C+D)","B/(A+B)",
                                    "C/(A+B)","AC/ABCD-AB/ABCD","PA-|U-L|")
                          )        
df.10c_vn.poor <- cbind(df.10b_vn.poor, df.10a_vn.poor)  
#save the result as a csv file                                     
fwrite(df.10c_vn.poor, file="01_rural_1st_VN_scorecard_accuracy.csv") 
     
1.3.4.2 #Estimate accuracy of predicted NEAR poor
df.11a_vn.npoor <- df.09_vn %>% select(poor_0m7_1m, pred_poor, pred_n_poor)
# generate sub_sample (only predicted non poor)                                        
df.11a_vn.npoor <- subset(df.11a_vn.npoor, pred_poor == "2. non-poor")    
df.11b_vn.npoor <- data.frame(table(df.11a_vn.npoor$poor_0m7_1m, df.11a_vn.npoor$pred_n_poor))
df.11b_vn.npoor <- df.11b_vn.npoor %>% select(contains("Freq"))
df.11b_vn.npoor <- df.11b_vn.npoor %>% rename(n_poor = Freq) 
df.11b_vn.npoor <- as.data.frame(t(df.11b_vn.npoor), )   
names(df.11b_vn.npoor)[1:4] <- c("C","A","D","B")       
# Estimate Accuracy performance
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (ABCD= C+A+D+B)     
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (TA  = (A+D)/(ABCD)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (PA  = (A)/(A+B)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (NPA = (D)/(C+D)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (U  = (B)/(A+B)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (L   = (C)/(A+B)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (PIE = ((A+C)/ABCD - (A+B)/ABCD)*100)
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate (BPAC= (PA-abs(U-L)))
df.11b_vn.npoor <- as.data.frame(t(df.11b_vn.npoor), row.names = FALSE)   
df.11b_vn.npoor <- df.11b_vn.npoor %>% mutate_if(is.numeric, round, digits = 2) #only use 2 digits

df.11c_vn.npoor <- data.frame(true_status=c("non n-poor","near poor","non n-poor","near poor","ACCURACY MEASURES","Total Accuracy",
                                        "Poverty Accuracy","Non-poverty Accu","Under-coverage","Leakage","PIE","BPAC"),
                            pred_status = c("near poor","near poor","non n-poor","non n-poor","ABREVIATION","TA","PA","NPA","U","L","PIE","BPAC"),
                            value = c("C","A","D","B","CALCULATION (%)","(A+D)/ABCD","A/(A+B)","D/(C+D)","B/(A+B)",
                                    "C/(A+B)","AC/ABCD-AB/ABCD","PA-|U-L|"))       
df.11d_vn.npoor <- cbind(df.11c_vn.npoor, df.11b_vn.npoor) 
#save file                                        
fwrite(df.11d_vn.npoor, file="01_rural_2nd_VN_scorecard_accuracy.csv")

1.3.4.3 #Estimate accuracy of all poor and near poor and better-off
df.12a_vn.comb <- data.frame(table(true_status = df.09_vn$true_poverty, pred_status = df.09_vn$pred_poverty))
df.12a_vn.comb <- df.12a_vn.comb %>% select(contains("Freq"))
df.12a_vn.comb <- df.12a_vn.comb %>% rename(poverty = Freq) 
df.12a_vn.comb <- as.data.frame(t(df.12a_vn.comb), )    
names(df.12a_vn.comb)[1:9] <- c("A","D","G","B","E","H","C","F","I")      
# Estimate Accuracy performance
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (TT   = rowSums(df.12a_vn.comb[,1:9]))
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (TA   = (A+E+I)/(TT)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (PA   = (A+E)/(A+B+C+D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (P    = (A)/(A+B+C)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (LP   = (E)/(E+D+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (NP   = (I)/(G+H+I)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (U    = (C+F)/(A+B+C+D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (U1   = (B+C)/(A+B+C)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (U2   = (D+F)/(D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (M    = (B+D)/(A+B+C+D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (PPE  = (G+H)/(A+D+G+B+E+H)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (L    = (D+G+B+H)/(A+B+C+D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (L1   = (D+G)/(A+B+C)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (L2   = (B+H)/(D+E+F)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (PIE  = ((G+H-C-F)/TT)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (PIE1 = ((D+G-B-C)/TT)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (PIE2 = ((B+H-D-F)/TT)*100)
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (BPAC = (PA-abs(U-L)))
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (BPAC1= (P- abs(U1-L1)))
df.12a_vn.comb <- df.12a_vn.comb %>% mutate (BPAC2= (LP-abs(U2-L2)))

df.12a_vn.comb <- as.data.frame(t(df.12a_vn.comb), row.names = FALSE)   
df.12a_vn.comb <- df.12a_vn.comb %>% mutate_if(is.numeric, round, digits = 2) #only use 2 digits
df.12b_vn.comb <- data.frame(true_status=c("Poor","Near-poor","Better-off","Poor","Near-poor","Better-off","Poor","Near-poor","Better-off",
                                           "MEASURES OF ACCURACY PERFORMANCE","Total Accuracy","Poverty Accuracy","Poor Accuracy","Near-poor Accuracy",
                                           "Better-off Accu","Under-coverage","Poor Under-coverage","Near-poor Under-coverage",
                                           "Mixture of poverty status","Poverty Prediction Error","Leakage","Poor Leakage","Near-poor Leakage",
                                           "PIE", "PIE of Poor","PIE of Near-poor","BPAC","BPAC of Poor","BPAC of Near-poor"),
                             pred_status = c("Poor","Poor","Poor","Near-poor","Near-poor","Near-poor","Better-off","Better-off","Better-off",
                                             "ABREVIATION","TA","PA","P","LP","NP","U","U1","U2","M","PPE",
                                             "L","L1","L2","PIE","PIE1","PIE2","BPAC","BPAC1","BPAC2"),
                             value = c("A","D","G","B","E","H","C","F","I","CALCULATION (%) - TT","AEI/TT","AE/ABCDEF","A/ABC","E/DEF","I/GHI",
                                       "CF/ABCDEF","BC/ABC","DF/DEF","BD/ABCDEF","GH/ADGBEH","DGBH/ABCDEF","DG/ABC","BH/DEF",
                                       "ADGBEH/TT-ABCDEF/TT","ADG/TT-ABC/TT","BEH/TT-DEF/TT","PA-|U-L|","P-|U1-L1|","LP-|U2-L2|"))       
df.12c_vn.comb <- cbind(df.12b_vn.comb, df.12a_vn.comb) #HERE is the result
#save as a csv file
fwrite(df.12c_vn.comb, file="02_VN_RURAL_scorecard_accuracy.csv") 

          
          ##########                                                                     ##########  
####################### END SCORING AND COMPUTING THE ACCURACY OF VN GOVERNMENT SCORECARD ####################
          ##########                                                                     ##########  
######################### NEXT PART IS TO GENERATE A NEW POVERTY SCORECARD TO PREDICT THE POOR and MODERATE POOR #######################
          ##########                                                                     ##########  
          
2.1 #Load data and change values for variables
df.08_classed2  ->  r1.00_data_value   
2.1.1 #change values of factor variables
r1.00_data_value$region18_6b <- ifelse(r1.00_data_value$region18_6b == "1", "Red river delta",
                                ifelse(r1.00_data_value$region18_6b == "2", "Northern mountainous Region",
                                ifelse(r1.00_data_value$region18_6b == "3", "North&South Central Coast",
                                ifelse(r1.00_data_value$region18_6b == "4", "Central Highlands",
                                ifelse(r1.00_data_value$region18_6b == "5", "Southeast",
                                       ifelse(r1.00_data_value$region18_6b == "6", "Mekong River Delta", NA))))))
r1.00_data_value$true_poverty <- ifelse(r1.00_data_value$true_poverty == "1", "1. Poor",
                                 ifelse(r1.00_data_value$true_poverty == "2", "2.Near poor",
                                       ifelse(r1.00_data_value$true_poverty == "3", "3.Better-off", NA)))
r1.00_data_value$House_Type2 <- ifelse(r1.00_data_value$House_Type2 == "1", "Temporary and other houses",
                                ifelse(r1.00_data_value$House_Type2 == "2", "Less temporary house",
                                ifelse(r1.00_data_value$House_Type2 == "3", "Semi permanent house",
                                      ifelse(r1.00_data_value$House_Type2 == "4", "Permanent house", NA))))
r1.00_data_value$HouseQuality <- ifelse(r1.00_data_value$HouseQuality == "1", "0 non-durable material",
                                 ifelse(r1.00_data_value$HouseQuality == "2", "1 non-durable material",
                                 ifelse(r1.00_data_value$HouseQuality == "3", "2 non-durable material",
                                       ifelse(r1.00_data_value$HouseQuality == "4", "3 non-durable material", NA))))
r1.00_data_value$pillar_material <- ifelse(r1.00_data_value$pillar_material == "1", "Reinforced concrete",
                                    ifelse(r1.00_data_value$pillar_material == "2", "Stone or Brick",
                                    ifelse(r1.00_data_value$pillar_material == "3", "Iron|Steel|Durable wood",
                                    ifelse(r1.00_data_value$pillar_material == "4", "less durable wood|Bamboo",
                                           ifelse(r1.00_data_value$pillar_material == "5", "other", NA)))))
r1.00_data_value$toilet_type7 <- ifelse(r1.00_data_value$toilet_type7 == "1", "Flush toilet",
                                 ifelse(r1.00_data_value$toilet_type7 == "2", "Hand-make/pit latrine",
                                 ifelse(r1.00_data_value$toilet_type7 == "3", "Composting cathole",
                                 ifelse(r1.00_data_value$toilet_type7 == "4", "Bucket latrine/chamber pot",
                                 ifelse(r1.00_data_value$toilet_type7 == "5", "Toilet on river/lake",
                                 ifelse(r1.00_data_value$toilet_type7 == "6", "No toilet",
                                        ifelse(r1.00_data_value$toilet_type7 == "7", "other", NA)))))))
r1.00_data_value$Water_sh4e <- ifelse(r1.00_data_value$Water_sh4e == "1", "Other unhygienic water source",
                               ifelse(r1.00_data_value$Water_sh4e == "2", "hand-dug well|spring/lake protected|rain water",
                               ifelse(r1.00_data_value$Water_sh4e == "3", "Water from drill well",
                               ifelse(r1.00_data_value$Water_sh4e == "4", "Tap water and water bought", NA))))
r1.00_data_value$hand_wash_place <- ifelse(r1.00_data_value$hand_wash_place == "1", "a fixed place in house",
                                    ifelse(r1.00_data_value$hand_wash_place == "2", "a fixed place in yard",
                                    ifelse(r1.00_data_value$hand_wash_place == "3", "Have non-fixed place",
                                    ifelse(r1.00_data_value$hand_wash_place == "4", "Don't have",
                                           ifelse(r1.00_data_value$hand_wash_place=="5"|r1.00_data_value$hand_wash_place=="9", "Other type", NA)))))
r1.00_data_value$wall_material <- ifelse(r1.00_data_value$wall_material == "1", "Reinforced concrete",
                                  ifelse(r1.00_data_value$wall_material == "2", "Stone or brick",
                                  ifelse(r1.00_data_value$wall_material == "3", "Iron/steel or wood",
                                  ifelse(r1.00_data_value$wall_material == "4", "Mud with straw",
                                  ifelse(r1.00_data_value$wall_material == "5", "bamboo net/less-durable plywoood",
                                         ifelse(r1.00_data_value$wall_material == "6", "Other type", NA))))))
r1.00_data_value$voc_edu_dip <- ifelse(r1.00_data_value$voc_edu_dip == "0", "No diploma",
                                ifelse(r1.00_data_value$voc_edu_dip == "4", "Vocational elementary",
                                ifelse(r1.00_data_value$voc_edu_dip == "5", "Vocational intermediate ",
                                ifelse(r1.00_data_value$voc_edu_dip == "6", "Professional intermediate",
                                       ifelse(r1.00_data_value$voc_edu_dip == "7", "Vocational college", NA)))))
r1.00_data_value$edu_diploma <- ifelse(r1.00_data_value$edu_diploma == "0", "No diploma",
                                ifelse(r1.00_data_value$edu_diploma == "1", "Primary",
                                ifelse(r1.00_data_value$edu_diploma == "2", "Secondary",
                                ifelse(r1.00_data_value$edu_diploma == "3", "Highschool",
                                ifelse(r1.00_data_value$edu_diploma == "8", "Degree of Associate",
                                ifelse(r1.00_data_value$edu_diploma == "9", "Bachelor/Engineer",
                                ifelse(r1.00_data_value$edu_diploma == "10", "Master",
                                ifelse(r1.00_data_value$edu_diploma == "11", "Doctorate",
                                         ifelse(r1.00_data_value$edu_diploma == "12", "Other type", NA)))))))))
r1.00_data_value$latrine_q10 <- ifelse(r1.00_data_value$latrine_q10 == "1", "hygiene latrine",
                                ifelse(r1.00_data_value$latrine_q10 == "2", "less-hygiene latrine",
                                         ifelse(r1.00_data_value$latrine_q10 == "3", "unhygiene|no latrine", NA)))
r1.00_data_value$water_sourc <- ifelse(r1.00_data_value$water_sourc == "1", "tap water or water bought",
                                ifelse(r1.00_data_value$water_sourc == "2", "water from drilled well",
                                ifelse(r1.00_data_value$water_sourc == "3", "rain or spring/lake protected",
                                       ifelse(r1.00_data_value$water_sourc == "4", "other unhygiene sources", NA))))
2.1.2 #change variables' class to factor 
r1_fact.var <- c("region18_6b", "true_poverty", "House_Type2",  "HouseQuality",
                  "pillar_material", "toilet_type7", "Water_sh4e", "hand_wash_place","wall_material",
                   "voc_edu_dip" ,"edu_diploma" ,"latrine_q10","water_sourc")
r1.01_data_classed <- r1.00_data_value %>% mutate_at(r1_fact.var, as.factor)          
          
2.2  #Filter variables with IV < 0.02
r1.02_data_filter <- var_filter(r1.01_data_classed, y="poor_0m7", iv_limit = 0.02, missing_limit = 0.1, 
                                 positive = 1, var_kp = "id", var_rm = c("poor_0m7_1m", "av_income")) 

2.3. #randomly splitting the origninal data into training and validation data set
set.seed(123)
r1.03.id.list <- partition(r1.02_data_filter$poor_0m7, p = c(train = 0.6, valid = 0.4))

# create training and validation data
r1.07_data_dev  <- r1.02_data_filter[r1.03.id.list$train, ]
r1.07_data_vali <- r1.02_data_filter[r1.03.id.list$valid, ]

2.4 #select best vars for scorecards (already selected by manual forward stepwise based on "c" statistic)
r1.07_data_vali.F <- r1.07_data_vali %>% select(id, poor_0m7, region18_6b, poor_16, hh_size, nonagri_labor_n, WorkFar_h,
                                                mBike2_S, e_cooker_pot, House_Type2, house_size_total, electr_kw, depen_16_s,
                                                internet, cow_buf_hors, aqua_area, hand_wash_place)
r1.07_data_dev.F <- r1.07_data_dev %>% select(id, poor_0m7, region18_6b, poor_16, hh_size, nonagri_labor_n, WorkFar_h,
                                              mBike2_S, e_cooker_pot, House_Type2, house_size_total, electr_kw, depen_16_s,
                                              internet, cow_buf_hors, aqua_area, hand_wash_place)
2.5. # Create a breaklist
r1.08_list_break.F <- list(
  region18_6b = c("Red river delta","Northern mountainous Region%,%North&South Central Coast%,%Central Highlands","Southeast","Mekong River Delta"), 
  hh_size = c("4","5","7"),nonagri_labor_n = c("1","2","3"),electr_kw = c("10", "50", "100","150"),internet= c("0","1"),
  house_size_total = c("40","100"), e_cooker_pot= c("0","1"), cow_buf_hors= c("1"), aqua_area= c("400"),
  House_Type2= c("Temporary and other houses","Less temporary house","Semi permanent house%,%Permanent house"), 
  WorkFar_h=c("0","1"),mBike2_S = c("1", "2"), depen_16_s = c("1","2","3"), poor_16 = c("0", "1"),
  hand_wash_place = c("a fixed place in house","a fixed place in yard","Have non-fixed place%,%Don't have%,%Other type"))
                                        
2.6 # generate Bins
# get the list of variables' name                                      
r1.09.list.dev.var <- r1.07_data_dev.F %>% select(-poor_0m7, -id) %>%  names()

r1.10_bins.F = woebin(r1.07_data_dev.F, y="poor_0m7", x= r1.09.list.dev.var, positive = 1, bin_num_limit = 20, breaks_list = r1.08_list_break.F)
r1.10_bins.F <- rbindlist(r1.10_bins.F, fill=TRUE)
# save bins                                        
fwrite(r1.10_bins.F, file="r1.10_bins.F_0m7_23_01_20.csv")
                                        
2.7 # gen woe data and Logit model
r1.11_data_woe <- woebin_ply(r1.07_data_dev.F, r1.10_bins.F)
r1.12_logit.1 <- glm(I(poor_0m7 == "1") ~ ., family = binomial(), data = r1.11_data_woe %>% select(-id)) 
summary(r1.12_logit.1)
                                        
#can use stepwise approach to find the best logit model based on min AIC, but here the best model is found, so do not need to run it     
        m_step = step(r1.12_logit.1, direction="both", trace = FALSE)
        r1.12_logit.best = eval(m_step$call)
        summary(r1.12_logit.best)

2.8 #Generate final scorecard
#calculate poor rate and odds of poor in the data set (<= 0m7 VND)
table(r1.02_data_filter$poor_0m7)     #poor rate = 1256/(5794+1256) = 0.178156
                                      #poor odd =0.178156/(1-0.178156)=0.2167759
#generate scorecard
r1.13_score_card <- scorecard(r1.10_bins.F, #list of bins generated
                              r1.12_logit.1, #logit specification
                              points0 = 100, #target points
                              pdo = 4,       #point to double the odd
                              odds0 = 0.217, 
                              basepoints_eq0 = TRUE) 
r1.13_score_card <- rbindlist(r1.13_score_card, fill=TRUE)
#save the poverty scorecard                                    
fwrite(r1.13_score_card, file="r1.13_score_card_11_30_3rd.csv") 

2.9 # Create a report containing all information of validation and development data sets
report(list(dt1 = r1.07_data_dev.F, dt2 = r1.07_data_vali.F), 
       y = "poor_0m7", 
       x = r1.09.list.dev.var, 
       seed = NULL, 
       breaks_list = r1.08_list_break.F,
       basepoints_eq0 = TRUE, 
       positive = 1, #work when poor_0m7(Y) is numeric
       points0 = 100, #target point
       odds0 = 0.217, 
       pdo = 4, #point to double the odd
       basepoints_eq0 = TRUE,
       show_plot = c('roc')) #create graph for ks, roc and gini values to check the validation

2.10 # Measure the accuracy performance of scorecard
2.10.1 #using the scorecard generated for scoring households
r1.14_score <- scorecard_ply(r1.01_data_classed, r1.13_score_card, only_total_score = F)

2.10.2 #Generate  variables of predicted poverty status  (predicted poor/non-poor) based on  various thresholds
#create new data frame contain true value of nonpoor and total score
r1.15_thred <- data.frame(r1.01_data_classed$poor_0m7, r1.14_score$score)
#rename variables                                        
names(r1.15_thred) <- c("poor_0m7", "score")  

# create 10 predicted variables for the cut-off points from 92 to 107
r1.15_thred[paste0("p", 92:107)] <- lapply(0:15, function(i) ifelse(r1.15_thred$score <= 92 + i, "a poor", "non poor")) #Short command

2.10.3 #Create crosstab/matrix between 10 predicted poor Y2 & true poor Y1
# creat a list or a data frame contain all values of cross table
r1.16_matrix <- data.frame(lapply(92:107,function(i) table(r1.15_thred$poor_0m7, r1.15_thred[, paste0("p", i)])))
r1.16_matrix <- r1.16_matrix %>% select(contains("Freq"))
r1.16_matrix <- r1.16_matrix %>%  rename_at(vars(matches("^Freq")), ~paste0("p", 92:107))

2.10.4 #Estimate Accuracy performance
r1.17a_accu_r <- r1.16_matrix    
r1.17b_accu_c <- as.data.frame(t(r1.17a_accu_r), )    
names(r1.17b_accu_c)[1:4] <- c("C","A","D","B")       
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (ABCD= C+A+D+B)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (TA  = (A+D)/(ABCD)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (PA  = (A)/(A+B)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (NPA = (D)/(C+D)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (U  = (B)/(A+B)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (L   = (C)/(A+B)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (PIE = ((A+C)/ABCD - (A+B)/ABCD)*100)
r1.17b_accu_c <- r1.17b_accu_c %>% mutate (BPAC= (PA-abs(U-L)))
r1.18a_accu_r <- as.data.frame(t(r1.17b_accu_c), row.names = FALSE)   
r1.18a_accu_r <- r1.18a_accu_r %>% mutate_if(is.numeric, round, digits = 2) 
r1.18b_accu_e <- data.frame(true_status=c("non poor","poor","non poor","poor","ACCURACY MEASURES","Total Accuracy",
                                          "Poverty Accuracy","Non-poverty Accu","Under-coverage","Leakage","PIE","BPAC"),
                            pred_status = c("poor","poor","non poor","non poor","ABREVIATION","TA","PA","NPA","U","L","PIE","BPAC"),
                            value = c("C","A","D","B","CALCULATION (%)","(A+D)/ABCD","A/(A+B)","D/(C+D)","B/(A+B)",
                                      "C/(A+B)","AC/ABCD-AB/ABCD","PA-|U-L|") )       
r1.19_accu_re <- cbind(r1.18b_accu_e, r1.18a_accu_r)
r1.19_accu_re$VN_scorecard <- df.10c_vn.poor$poor #compare with Vietnamese government scorecard

#save the result
fwrite(r1.19_accu_re, file="03_rural_1st_scorecard_poor.csv") 

                ###########                              ###########
################################# END first step ##############################################
                ###########                             #############
#################### NEXT STEP IS TO BUILD A SECOND SCORECARD TO TARGET THE NEAR POOR ##########
                ###########                            ##############

3. # Generate 2nd scorecard predict NEAR - POOR by sub sample
3.1 #Create sub sample       
3.1.1 #Filter the 2nd time after change type and NA
r1.01_data_classed -> r2.01_data_classed
3.1.2 # generate sub sample from 1st scorecard
r2.01_data_classed -> r2.02_data_score
r2.02_data_score$score_1st <- r1.14_score$score #add score var

r2.03_data_drop97 <- subset(r2.02_data_score, score_1st > 97)    # generate sub_sample (only observation >97 points)
#drop old ID variable and create a new one
r2.04_data_ID <- r2.03_data_drop97 %>% select(-id)
r2.04_data_ID$id <- 1:nrow(r2.04_data_ID) 

3.2 # filter variables for the first time after setting the class
r2.05_data_filter <- var_filter(r2.04_data_ID, y="poor_0m7_1m", iv_limit = 0.02, missing_limit = 0.1,
                                 positive = 1, var_kp = "id", var_rm = "poor_0m7")

3.3 # randomly split the data into 02 sub-sample
set.seed(1234)
r2.07.id.list <- partition(r2.05_data_filter$poor_0m7_1m, p = c(train = 0.6, valid = 0.4))
# get training and validation data
r2.07_data_dev  <- r2.05_data_filter[r2.07.id.list$train, ]
r2.07_data_vali <- r2.05_data_filter[r2.07.id.list$valid, ]

3.4 #select only variables (selected based "MAXC" method before)
r2.07_data_vali.F <- r2.07_data_vali %>% select(id, poor_0m7_1m,
                                                WashM24_H,pillar_unstable,Water_sh4e,handwash_soap,toilet_type7,
                                                poor_16,hh_size, WorkFar_h,mBike2_S,StoveG27_H, HouseSize_mean,
                                                electr_kw, depen_16_s, internet, nonagri_labor_n)
r2.07_data_dev.F <- r2.07_data_dev %>% select(id, poor_0m7_1m,
                                              WashM24_H,pillar_unstable,Water_sh4e,handwash_soap,toilet_type7,
                                              poor_16,hh_size, WorkFar_h,mBike2_S,StoveG27_H, HouseSize_mean,
                                              electr_kw, depen_16_s, internet, nonagri_labor_n)
3.5 # Create breaklist for bins
r2.08_list_break.F <- list(poor_16 = c("0", "1"),hh_size = c("4","7"),WorkFar_h=c("0","1"),electr_kw = c("50", "100","150"),
                           depen_16_s = c("2","4"),mBike2_S = c("2", "3"),internet= c("0","1"),nonagri_labor_n = c("2","3"),
                           StoveG27_H= c("0","1"),HouseSize_mean = c("8","24"), handwash_soap = c("0","1"), toilet_type7= c("Flush toilet",
                           "Hand-make/pit latrine%,%Composting cathole%,%Bucket latrine/chamber pot%,%Toilet on river/lake%,%No toilet%,%other"),
                           Water_sh4e= c("Other unhygienic water source%,%hand-dug well|spring/lake protected|rain water",
                                         "Water from drill well%,%Tap water and water bought"),
                           pillar_unstable= c("0","1"),WashM24_H= c("0","1"))
3.6 # Generate Bins
#create a list of variables' name                                  
r2.09.list.dev.var <- r2.07_data_dev.F %>% select(-poor_0m7_1m, -id) %>%  names() 
#create bins based on list & breaklists                                  
r2.10_bins.F = woebin(r2.07_data_dev.F, y="poor_0m7_1m", x= r2.09.list.dev.var,  
                      positive = 1, breaks_list = r2.08_list_break.F)  

r2.10_bins.F <- rbindlist(r2.10_bins.F, fill=TRUE)
# save the bins                                 
fwrite(r2.10_bins.F, file="r2.10_bins.F_23_01_24.csv") 
                                  
3.7 # Gen woe data and run Logit
r2.11_data_woe <- woebin_ply(r2.07_data_dev.F, r2.10_bins.F)
r2.12_logit.1 <- glm(I(poor_0m7_1m == "1") ~ .,  # set the event as good, e.g. nonpoor_1m == 1
                     family = binomial(),  # binomial is logistic regression
                     data = r2.11_data_woe %>% select(-id)) # remove id column
summary(r2.12_logit.1) #call result
#can use step approach to find the best logit model, but here is the best model found, so do not need to run it     
    m_step = step(r2.12_logit.1, direction="both", trace = FALSE)
    r2.12_logit.best = eval(m_step$call)
    summary(r2.12_logit.best)

3.8 #Generate final scorecard
#calculate poor rate and odds of poor in the data set (>0.7 - 1 million VND/person/month)
table(r2.05_data_filter$poor_0m7_1m)     #poor rate = 831 /(831+4299 ) = 0.1619883
                                         #poor odd =0.1619883/(1-0.1619883)=0.1933008
#generate scorecard
r2.13_score_card <- scorecard(r2.10_bins.F, #break list
                              r2.12_logit.1, #logit model
                              points0 = 100, #target points
                              pdo = 6,       #point to double the odd
                              odds0 = 0.193, #targeted poverty odds
                              basepoints_eq0 = TRUE)
r2.13_score_card <- rbindlist(r2.13_score_card, fill=TRUE)
fwrite(r2.13_score_card, file="r2.13_score_card_12_01 .csv") 

3.9 # Create a report 
report(list(dt1 = r2.07_data_dev.F, dt2 = r2.07_data_vali.F), 
       y = "poor_0m7_1m", 
       x = r2.09.list.dev.var, 
       seed = NULL, 
       breaks_list = r2.08_list_break.F,
       basepoints_eq0 = TRUE, 
       positive = 1, #work with 1 if y is numeric
       points0 = 100, 
       odds0 = 0.193,
       pdo = 6,
       basepoints_eq0 = TRUE,
       show_plot = c('roc')) 

3.10 # Check the Accuracy of predicting poor 0m7 in the sub-data set
3.10.1 #Scoring all variables in the sub sample 
r2.14_score <- scorecard_ply(r2.05_data_filter, r2.13_score_card, only_total_score = F) 

3.10.2 #Generate Y2 variables (predicted near-poor variable) based on thresholds
#create new data frame contain true value of nonpoor and total score
r2.15_thred <- data.frame(r2.05_data_filter$poor_0m7_1m, r2.14_score$score) 
names(r2.15_thred) <- c("poor_0m7_1m", "score")   

#generate 16 predicted variables from p0:p15, with the cut-off for y0 <= 92, cut-off for y15 <= 107 (92 + i=15)
r2.15_thred[paste0("p", 90:105)] <- lapply(0:15, function(i) ifelse(r2.15_thred$score <= 90 + i, "a n-poor", "non n-poor")) 

3.10.3 #Create crosstab/matrix between predicted non-poor Y2 & true non-poor Y1
r2.16_matrix <- data.frame(lapply(90:105,function(i) table(r2.15_thred$poor_0m7_1m, r2.15_thred[, paste0("p", i)])))
r2.16_matrix <- r2.16_matrix %>% select(contains("Freq"))
r2.16_matrix <- r2.16_matrix %>%  rename_at(vars(matches("^Freq")), ~paste0("p", 90:105))

#Estimate accuracy performance
r2.17a_accu_r <- r2.16_matrix[]    
r2.17b_accu_c <- as.data.frame(t(r2.17a_accu_r), )    
names(r2.17b_accu_c)[1:4] <- c("C","A","D","B")      
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (ABCD= C+A+D+B)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (TA  = (A+D)/(ABCD)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (PA  = (A)/(A+B)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (NPA = (D)/(C+D)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (U  = (B)/(A+B)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (L   = (C)/(A+B)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (PIE = ((A+C)/ABCD - (A+B)/ABCD)*100)
r2.17b_accu_c <- r2.17b_accu_c %>% mutate (BPAC= (PA-abs(U-L)))

r2.18a_accu_r <- as.data.frame(t(r2.17b_accu_c), row.names = FALSE)   
r2.18a_accu_r <- r2.18a_accu_r %>% mutate_if(is.numeric, round, digits = 2) 
r2.18b_accu_e <- data.frame(true_status=c("non n-poor","near-poor","non n-poor","near-poor","ACCURACY MEASURES","Total Accuracy",
                                          "Poverty Accuracy","Non-poverty Accu","Under-coverage","Leakage","PIE","BPAC"),
                            pred_status = c("near-poor","near-poor","non n-poor","non n-poor","ABREVIATION","TA","PA","NPA","U","L","PIE","BPAC"),
                            value = c("C","A","D","B","CALCULATION (%)","(A+D)/ABCD","A/(A+B)","D/(C+D)","B/(A+B)",
                                      "C/(A+B)","AC/ABCD-AB/ABCD","PA-|U-L|"))       
r2.19_accu_re <- cbind(r2.18b_accu_e, r2.18a_accu_r) 
r2.19_accu_re$VN_scorecard <- df.11d_vn.npoor$n_poor #compare with Vietnamese government scorecard
#save the result
fwrite(r2.19_accu_re, file="03_rural_2nd_scorecard_nearpoor.csv") 

        
#################### CALCULATE ACCURACY PERFORMANCE OF TWO SCORECARDS  ################
        
4. # Combine 02 scorecards to measure accuracy performance
4.1 #using 1st scorecard and 2nd scorecard for scoring
r1.20_score_full <- scorecard_ply(r1.01_data_classed, r1.13_score_card, only_total_score = F) #1st scorecard
r2.20_score_full <- scorecard_ply(r1.01_data_classed, r2.13_score_card, only_total_score = F) #2nd corecard

4.2 #Generate a new threshold
#create new data frame contain true value of poverty status and total score
r2.21_thred_full <- data.frame(r1.01_data_classed$id, r1.01_data_classed$poor_0m7, r1.01_data_classed$poor_0m7_1m, 
                               r1.01_data_classed$true_poverty, r1.20_score_full$score, r2.20_score_full$score)
names(r2.21_thred_full) <- c("id", "poor_0m7", "poor_0m7_1m", "true_status", "score_1st", "score")      

# create  variables of predicted poverty (Y2) status based on cut-off from 94 to 103
## if score of 1st scorecard <= 97 => predicted poor (accept the poor based on 1st scorecard cut-off)
## if score of 1st scorecard >97 (non-poor predicted by 1st scorecard) & score of 2nd scorecard <= cut-off (from 94:103) => predicted near-poor
                                  
r2.21_thred_full[paste0("p", 90:102)] <- lapply(0:12, function(i) ifelse(r2.21_thred_full$score_1st<=97, "1.Poor",
                                                                 ifelse((r2.21_thred_full$score_1st>97) & (r2.21_thred_full$score<=90 + i),
                                                                         "2.Near poor","3.Better-off"))) 
4.3 # Create crosstab/matrix between predicted poverty status Y2 & true poverty status Y1
r2.22_matrix_full <- data.frame(lapply(90:102,function(i) table(r2.21_thred_full$true_status, r2.21_thred_full[, paste0("p", i)])))
r2.22_matrix_full <- r2.22_matrix_full %>% select(contains("Freq"))
r2.22_matrix_full <- r2.22_matrix_full %>%  rename_at(vars(matches("^Freq")), ~paste0("p", 90:102))

4.4  #Measure accuracy performance
r2.23a_accu_r <- r2.22_matrix_full[]   
r2.23b_accu_c <- as.data.frame(t(r2.23a_accu_r), )   
names(r2.23b_accu_c)[1:9] <- c("A","D","G","B","E","H","C","F","I")      

# Estimate Measures of accuracy performance
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (TT   = rowSums(r2.23b_accu_c[,1:9]))
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (TA   = (A+E+I)/(TT)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (PA   = (A+E)/(A+B+C+D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (P    = (A)/(A+B+C)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (LP   = (E)/(E+D+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (NP   = (I)/(G+H+I)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (U    = (C+F)/(A+B+C+D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (U1   = (B+C)/(A+B+C)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (U2   = (D+F)/(D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (M    = (B+D)/(A+B+C+D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (PPE  = (G+H)/(A+D+G+B+E+H)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (L    = (D+G+B+H)/(A+B+C+D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (L1   = (D+G)/(A+B+C)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (L2   = (B+H)/(D+E+F)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (PIE  = ((G+H-C-F)/TT)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (PIE1 = ((D+G-B-C)/TT)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (PIE2 = ((B+H-D-F)/TT)*100)
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (BPAC = (PA-abs(U-L)))
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (BPAC1= (P- abs(U1-L1)))
r2.23b_accu_c <- r2.23b_accu_c %>% mutate (BPAC2= (LP-abs(U2-L2)))

r2.24a_accu_r <- as.data.frame(t(r2.23b_accu_c), row.names = FALSE)   
r2.24a_accu_r <- r2.24a_accu_r %>% mutate_if(is.numeric, round, digits = 2) 

r2.24b_accu_e <- data.frame(true_status=c("Poor","Near-poor","Better-off","Poor","Near-poor","Better-off","Poor","Near-poor","Better-off",
                                           "MEASURES OF ACCURACY PERFORMANCE","Total Accuracy","Poverty Accuracy","Poor Accuracy","Near-poor Accuracy",
                                           "Better-off Accu","Under-coverage","Poor Under-coverage","Near-poor Under-coverage",
                                           "Mixture of poverty status","Poverty Prediction Error","Leakage","Poor Leakage","Near-poor Leakage",
                                           "PIE", "PIE of Poor","PIE of Near-poor","BPAC","BPAC of Poor","BPAC of Near-poor"),
                             pred_status = c("Poor","Poor","Poor","Near-poor","Near-poor","Near-poor","Better-off","Better-off","Better-off",
                                             "ABREVIATION","TA","PA","P","LP","NP","U","U1","U2","M","PPE",
                                             "L","L1","L2","PIE","PIE1","PIE2","BPAC","BPAC1","BPAC2"),
                             value = c("A","D","G","B","E","H","C","F","I","CALCULATION (%) - TT","AEI/TT","AE/ABCDEF","A/ABC","E/DEF","I/GHI",
                                       "CF/ABCDEF","BC/ABC","DF/DEF","BD/ABCDEF","GH/ADGBEH","DGBH/ABCDEF","DG/ABC","BH/DEF",
                                       "ADGBEH/TT-ABCDEF/TT","ADG/TT-ABC/TT","BEH/TT-DEF/TT","PA-|U-L|","P-|U1-L1|","LP-|U2-L2|"))        

r2.25_accu_re <- cbind(r2.24b_accu_e, r2.24a_accu_r) #combine
r2.25_accu_re$VN_scorecard <- df.12c_vn.comb$poverty #compare with Vietnamese government scorecard
#save the result
fwrite(r2.25_accu_re, file="04_Rural_dualscorecard_accuracy.csv") 
    
############# END ##############################

