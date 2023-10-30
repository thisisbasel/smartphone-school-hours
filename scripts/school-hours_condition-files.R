# README ------------------------------------------------------------------------------------------------------
# Program reads in each participant's school-hours data
# Will generate files with metrics data for each instructional condition [online, non-school, f2f, blended]
# Program uses crit1 (10-mins per hour) as cut off for usable data 


# IMPORT LIBRARIES --------------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)


# IMPORT IDS --------------------------------------------------------------------------------------------------
id_student <- c(200215, 1002321, 1002621, 1002911,1004381,201142,1006542,201912,1006841,202055,202815,1027365,
                203055,203225,1010935,1011411 ,204185, 1011551,1012315,205165,1015951,1010221,206736,206737,
                206866,207675,1018335,1018975,202152,209385,209715,209675,1025501,210675,210775,211025,211275,
                211125,1028595,1028596,212325,212326,211515,212955,212681,213425,1031351,1035691,213545,213181,
                213975,214135,1032065,1037635,214765,216915,216245,216525,217845,218866,1038575,223135,
                228185,228186,228755,228825,229425,229535,229745,230035,230235,251655,251735,251835,
                231555,231656,232455,232456,232575,249625,249785,249786,249985,250555,250625,250845,251166,
                253715,253716,255455,255576,256025,256026,256335,256425,256426,257235,258145,258645,
                258965,259035,260085,260115,260865,261115,261225,1041555,1041635,1043405,1043505,
                1045385,222115,221275,220575,1047965,1048015,1048635,1048745,1049815,1050095,1051515,1051725,1051285,
                1051855,1052055,1052285,1052975,1054736,1053845,1054745,1058685,1059355,1060115,236525,1060675,
                1062035,1063315,1064755,1064835,1065015,1065205,1065705,1065706,1066235)

#1002321

# TAG CONDITION -----------------------------------------------------------------------------------------------
# Init empty dataframes
df_non_school_participants <- data.frame()
df_f2f_participants <- data.frame()
df_online_participants <- data.frame()
df_blended_participants <- data.frame()


# Loop through IDs
for (i in 1:length(id_student)){
  ID = id_student[i]
  #ID = 229535
  
  # Import data 
  df <- read_csv(paste0("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/processed/df_school_year_", ID, ".csv"))
  
  
  # Filter data ----------------------------------------------------------------------------------------------
  # Select metrics data 
  df_metrics <- df 
  
  # %>% 
  #   select(1:13, (ncol(.) - 5):ncol(.))
  
  # Select crit1 data 
  df_crit1 <- df_metrics %>%
    filter(crit1 == 'crit1')
  
  # Select 'non-school' data 
  df_non_school <- df_crit1 %>%
    filter(day_type == 'Weekend' | holiday == 'winter-break' | holiday == 'holiday')
  
  # Select 'online' days
  df_online <- df_crit1 %>%
    filter(instruction_condition == 'online' & day_type == 'Weekday' & holiday == 'None')
  
  # Select 'f2f' days
  df_f2f <- df_crit1 %>%
    filter(instruction_condition == 'face-to-face' & day_type == 'Weekday' & holiday == 'None')
  
  # Select 'blended' days
  df_blended <- df_crit1 %>%
    filter(instruction_condition == 'blended' & day_type == 'Weekday' & holiday == 'None')
  
  
  # Merge data based on condition ----------------------------------------------------------------------------
  #Merge non-school data
  df_non_school_participants <- df_non_school_participants %>%
    merge(df_non_school, all = TRUE)

  # Merge f2f
  df_f2f_participants <- df_f2f_participants %>%
    merge(df_f2f, all = TRUE)

  # Merge online
  df_online_participants <- df_online_participants %>%
    merge(df_online, all = TRUE)

  # Merge blended data
  df_blended_participants <- df_blended_participants %>%
    merge(df_blended, all = TRUE)

}

df_non_school_participants_fill_nan <- df_non_school_participants %>%
  mutate_all(~ replace(., is.nan(.), 0))

df_f2f_participants_fill_nan <- df_f2f_participants %>%
  mutate_all(~ replace(., is.nan(.), 0))

df_online_participants_fill_nan <- df_online_participants %>%
  mutate_all(~ replace(., is.nan(.), 0))

df_blended_participants_fill_nan <- df_blended_participants %>%
  mutate_all(~ replace(., is.nan(.), 0))

# EXPORT DATA -------------------------------------------------------------------------------------------------
write.csv(df_non_school_participants_fill_nan, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_non_school_participants.csv", row.names = FALSE)
write.csv(df_f2f_participants_fill_nan, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_f2f_participants.csv", row.names = FALSE)
write.csv(df_online_participants_fill_nan, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_online_participants.csv", row.names = FALSE)
write.csv(df_blended_participants_fill_nan, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_blended_participants.csv", row.names = FALSE)
