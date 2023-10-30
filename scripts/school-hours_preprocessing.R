# README -------------------------------------------------------------------------------------------------------------------------
# This file preprocesses metrics data for school-hours project
# Will output file for each ID with data tagged based on holiday, survey responses, school-hours, and crit1 


# IMPORT LIBRARIES ---------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dplyr)


# IMPORT IDS ---------------------------------------------------------------------------------------------------------------
id_student <- c(200215, 1002321, 1002621, 1002911,1004381,201142,1006542,201912,1006841,202055,202815,
                203055,203225,1010935,1011411 ,204185, 1011551,1012315,205165,1015951,1010221,206736,206737,
                206866,207675,1018335,1018975,202152,209385,209715,209675,1025501,210675,210775,211025,211275,1027365,
                211125,1028595,1028596,212325,212326,211515,212955,212681,213425,1031351,1035691,213545,213181,
                213975,214135,1032065,1037635,214765,216915,216245,216525,217845,218866,1038575,223135,
                228185,228186,228755,228825,229425,229535,229745,230035,230235,
                231555,231656,232455,232456,232575,249625,249785,249786,249985,250555,250625,250845,251166,251655,251735,251835,
                253715,253716,255455,255576,256025,256026,256335,256425,256426,257235,258145,258645,
                258965,259035,260085,260115,260865,261115,261225,1041555,1041635,1043405,1043505,
                1045385,222115,221275,220575,1047965,1048015,1048635,1048745,1049815,1050095,1051515,1051725,1051285,
                1051855,1052055,1052285,1052975,1054736,1053845,1054745,1058685,1059355,1060115,236525,1060675,
                1062035,1063315,1064755,1064835,1065015,1065205,1065705,1065706,1066235)

# IMPORT DATA ---------------------------------------------------------------------------------------------------------------
df_surveys <- read_csv("/Users/baselhussein/Projects/ttf/data/survey/No_DOB_Clean_Teen_Surveys_2022-12-08.csv") # teen data 


# PREPROCESS STUDENT DATA ---------------------------------------------------------------------------------------------------------------
for (i in 1:length(id_student)){
  ID = id_student[i]
  #ID = 1037635

  # Import data 
  df <- read_csv(paste0("/Users/baselhussein/Projects/ttf/osu-metrics/metrics_impute/df_metrics_impute_", ID, ".csv"))

  
  ### TAG SCHOOL VS. ONLINE DAYS 
  # Face-to-face learning 
  cschftf01 <- df_surveys$cschftf01[df_surveys$participant_id == ID] # baseline survey, day 1 of participation 
  cschftf03 <- df_surveys$cschftf03[df_surveys$participant_id == ID] # survey 2, week 4 of participation 
  cschftf05 <- df_surveys$cschftf05[df_surveys$participant_id == ID] # survey 4, week 8 of participation 
  cschftf07 <- df_surveys$cschftf07[df_surveys$participant_id == ID] # survey 6, week 12 of participation 
  cschftf09 <- df_surveys$cschftf09[df_surveys$participant_id == ID] # survey 8, week 16 of participation  
  cschftf11 <- df_surveys$cschftf11[df_surveys$participant_id == ID] # survey 10, week 20 of participation  
  cschftf13 <- df_surveys$cschftf13[df_surveys$participant_id == ID] # survey 12, week 24 of participation  
  
  # Online learning 
  cschonli01 <- df_surveys$cschonli01[df_surveys$participant_id == ID] # baseline survey, day 1 of participation 
  cschonli03 <- df_surveys$cschonli03[df_surveys$participant_id == ID] # survey 2, week 4 of participation 
  cschonli05 <- df_surveys$cschonli05[df_surveys$participant_id == ID] # survey 4, week 8 of participation 
  cschonli07 <- df_surveys$cschonli07[df_surveys$participant_id == ID] # survey 6, week 12 of participation  
  cschonli09 <- df_surveys$cschonli09[df_surveys$participant_id == ID] # survey 8, week 16 of participation  
  cschonli11 <- df_surveys$cschonli11[df_surveys$participant_id == ID] # survey 10, week 20 of participation  
  cschonli13 <- df_surveys$cschonli13[df_surveys$participant_id == ID] # survey 12, week 24 of participation 
  
  # Survey dates 
  survey_baseline_date <- as.POSIXct(df_surveys$baseline_survey_part_1_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_2_wk4_date <- as.POSIXct(df_surveys$survey_2_wk_4_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_4_wk8_date <- as.POSIXct(df_surveys$survey_4_wk_8_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_6_wk12_date <- as.POSIXct(df_surveys$survey_6_wk_12_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_8_wk16_date <- as.POSIXct(df_surveys$survey_8_wk_16_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_10_wk20_date <- as.POSIXct(df_surveys$survey_10_wk_20_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")
  survey_12_wk24_date <- as.POSIXct(df_surveys$survey_12_wk_24_timestamp[df_surveys$participant_id == ID], format = "%Y-%m-%d %H:%M:%S")


  
  # Add counter for participation 
  df <- df %>%
    mutate(days_since_start = as.numeric(date_local_original - min(date_local_original)) + 1) %>%
    select(ID, days_since_start = days_since_start, everything())
  
  # Convert df datetime column
  df$date_local_original <- as.POSIXct(df$date_local_original, format = "%Y-%m-%d %H:%M:%S")
  #str(df)
  
  # Add school-hours to student data 
  df$face_to_face_instruction <- NA
  df <- df %>%
    mutate(
      face_to_face_instruction = case_when(
        date_local_original < survey_baseline_date ~ cschftf01,
        date_local_original >= survey_baseline_date & date_local_original <= survey_2_wk4_date - days(14) ~ cschftf01, #baseline cschftf01
        date_local_original >= survey_2_wk4_date - days(14) & date_local_original <= survey_4_wk8_date - days(14) ~ cschftf03, #week4
        date_local_original >= survey_4_wk8_date - days(14) & date_local_original <= survey_6_wk12_date - days(14) ~ cschftf05, #week8
        date_local_original >= survey_6_wk12_date - days(14) & date_local_original <= survey_8_wk16_date - days(14) ~ cschftf07, #week12
        date_local_original >= survey_8_wk16_date - days(14) & date_local_original <= survey_10_wk20_date - days(14) ~ cschftf09, #week16
        date_local_original >= survey_10_wk20_date - days(14) & date_local_original <= survey_12_wk24_date - days(14) ~ cschftf11, #week20
        date_local_original >= survey_12_wk24_date - days(14) ~ cschftf13, #week24
        TRUE ~ face_to_face_instruction  
      )
    )
  
  # Add online-instruction to student data cschonli03
  df$online_instruction <- NA
  df <- df %>%
    mutate(
      online_instruction = case_when(
        date_local_original < survey_baseline_date ~ cschonli01,
        date_local_original >= survey_baseline_date & date_local_original <= survey_2_wk4_date - days(14) ~ cschonli01, #baseline cschftf01
        date_local_original >= survey_2_wk4_date - days(14) & date_local_original <= survey_4_wk8_date - days(14) ~ cschonli03, #week4
        date_local_original >= survey_4_wk8_date - days(14) & date_local_original <= survey_6_wk12_date - days(14) ~ cschonli05, #week8
        date_local_original >= survey_6_wk12_date - days(14) & date_local_original <= survey_8_wk16_date - days(14) ~ cschonli07, #week12
        date_local_original >= survey_8_wk16_date - days(14) & date_local_original <= survey_10_wk20_date - days(14) ~ cschonli09, #week16
        date_local_original >= survey_10_wk20_date - days(14) & date_local_original <= survey_12_wk24_date - days(14) ~ cschonli11, #week20
        date_local_original >= survey_12_wk24_date - days(14) ~ cschonli13, #week24
        TRUE ~ online_instruction  # Keep the original value if none of the conditions match
      )
    )

  # Add instruction-condition column 
  df$instruction_condition <- NA
  df <- df %>%
    mutate(
      instruction_condition = case_when(
        face_to_face_instruction == 1 & online_instruction == 0 ~ "face-to-face",
        face_to_face_instruction == 0 & online_instruction == 1 ~ "online",
        face_to_face_instruction == 1 & online_instruction == 1 ~ "blended",
        # add non-school condition, 0 and 0?
        TRUE ~ NA_character_  # For all other cases, the value will be NA
      )
    )
  
  # FILTER DATES & HOURS ---------------------------------------------------------------------------------------------------------------
  # Filter data based on hours 
  df$hour_original <- as.numeric(df$hour_original)
  df_school_hours <- df %>%
    filter(hour_original >= 09 & hour_original < 15) # hours between 9am and 3pm 
  
  # Tag weekdays 
  df_weekdays <- df_school_hours %>%
    mutate(day_type = ifelse(weekdays(date_local_original) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
  
  ### Tag holidays 
  # Convert dates to lubridate format
  df_weekdays$date_local_original <- ymd(df_weekdays$date_local_original)
  
  # Tag Christmas break 
  # df_holiday_drop_winter <- df_weekdays %>%
  #   filter(!(month(date_local_original) == 12 & day(date_local_original) >= 15) & 
  #          !(month(date_local_original) == 1 & day(date_local_original) <= 2)) # filter days after Dec 15th and before Jan 2nd
  
  df_holiday_winter <- df_weekdays %>%
    mutate(holiday = ifelse((month(date_local_original) == 12 & day(date_local_original) >= 15) | 
                                   (month(date_local_original) == 1 & day(date_local_original) <= 2),
                                    "winter_break", "None"))
  
  # Tag Summer break
  df_holiday_summer <- df_holiday_winter %>%
    mutate(holiday = ifelse((month(date_local_original) == 6 & day(date_local_original) >= 1) | 
                                   (month(date_local_original) == 7) | 
                                   (month(date_local_original) == 8) | 
                                   (month(date_local_original) == 9 & day(date_local_original) <= 5),
                                    "summer_break", holiday))
  
  # Drop Spring break
  df_holiday_spring <- df_holiday_summer %>%
    mutate(holiday = ifelse((month(date_local_original) == 3 & day(date_local_original) >= 20) | 
                                   (month(date_local_original) == 4 & day(date_local_original) <= 10),
                                    "spring_break", holiday)) # filter days after Mar 20th and before Apr 10th

  # Define & drop misc. holidays
  holiday_dates <- c(
    "2021-09-25",
    "2021-11-03",
    "2021-11-22", "2021-11-23", "2021-11-24",
    "2021-12-08",
    "2021-01-01",
    "2021-01-15",
    "2021-01-26",
    "2021-02-19",
    "2021-03-01",
    "2021-03-29",
    "2021-04-10",
    "2021-05-27"
  )
  
  df_school_year <- df_holiday_spring %>%
    mutate(holiday = ifelse(date_local_original %in% holiday_dates, "holiday", holiday))
  
  #df_school_year <- subset(df_holiday_drop_spring, !(date_local_original %in% holiday_dates))
  
  
  ### TAG DATA WITH CRIT 1
  # CRIT1: 10 MINS DATA PER HOUR, 9/9 HOURS ####################################################################
  # Convert the 'Date' column to Date type 
  df_school_year$date_local_original <- as.Date(df_school_year$date_local_original)
  
  # Create a new column 'Tag' to store the tags
  df_school_year$crit1 <- ""
  
  # Iterate over unique dates in the dataframe
  for (date in unique(df_school_year$date_local_original)) {
    # Subset the dataframe for the current date
    subset_df <- df_school_year[df_school_year$date_local_original == date, ]
    
    # Count the number of hours with at least 10 minutes of missing data 
    hours_with_usage <- sum(subset_df$missing_time_minutes > 50)
    
    # If the count is greater than or equal to 9, tag the night
    if (hours_with_usage >= 1) {
      df_school_year$crit1[df_school_year$date_local_original == date] <- "missing"
    } else {
      df_school_year$crit1[df_school_year$date_local_original == date] <- "crit1"
    }
  }
  
  #df_school_year_nonmissing <- subset(df_school_year, crit1 != "missing")

  # EXPORT ---------------------------------------------------------------------------------------------------------------
  write.csv(df_school_year, paste0("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/processed/df_school_year_", ID, ".csv"), row.names = FALSE)

}

