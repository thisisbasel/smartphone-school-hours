# README -------------------------------------------------------------------------------------------------------
# Program reads in participant files and outputs top10 apps used by each participant
# Counts how often an app appears in top10 


# IMPORT LIBRARIES -------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dplyr)
library(tibble)


# IMPORT IDS -------------------------------------------------------------------------------------------------------
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


# INIT SUMMARY TABLE -------------------------------------------------------------------------------------------------------
# Init summary_table
columns = c("app", "usage_day")

df_school_hours_app_summary = data.frame(matrix(nrow = 0, ncol = length(columns)))  # init empty dataframe
colnames(df_school_hours_app_summary) = columns 

### IMPORT DATA 
df_categories <- read_csv("/Users/baselhussein/Projects/ttf/data/app-categories/df_categories_tagged.csv")


# PREPROCESS STUDENT DATA -------------------------------------------------------------------------------------------------------
for (i in 1:length(id_student)){
  ID = id_student[i]
  #ID = 200215
  
  # Import data 
  df <- read_csv(paste0("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/processed/df_school_year_", ID, ".csv"))
  
  # Unique days 
  total_unique_days <- df %>% 
    summarise(total_unique_days = n_distinct(date_local_original))
  
  # Keep only imputed app-usage
  df_app_usage <- df %>%
    select(contains("_mins_impute"))

  col_sums <- colSums(df_app_usage)
  
  # Create a new data frame to store the column names and their sum values
  df_apps <- data.frame(column_name = names(col_sums), sum_value = col_sums)
  df_apps$sum_value <- df_apps$sum_value / total_unique_days$total_unique_days
  
  df_apps <- df_apps %>% 
    rownames_to_column(var = "new_index")
  
  df_apps_top_10 <- df_apps %>%
    arrange(desc(sum_value)) %>% 
    head(10)

  # Drop the 'new_index' column
  df_apps_top_10 <- subset(df_apps_top_10, select = -new_index)
  
  # Rename columns 
  colnames(df_apps_top_10)[colnames(df_apps_top_10) == "sum_value"] <- "usage_day"
  colnames(df_apps_top_10)[colnames(df_apps_top_10) == "column_name"] <- "app"
  
  # Add to df_school_hours_summary 
  df_school_hours_app_summary <- df_school_hours_app_summary %>%
    rbind(df_apps_top_10)
  
}

# EXPORT LIST -----------------------------------------------------------------------------------------------------------
# Merge package names to df_app_summary 
colnames(df_school_hours_app_summary)[colnames(df_school_hours_app_summary) == "app"] <- "package"
 
df_school_hours_app_summary$package <- as.character(df_school_hours_app_summary$package)
df_categories$package <- as.character(df_categories$package)
df_categories$package <- paste(df_categories$package, "_mins_impute", sep = "")


# Merge the data frames based on the 'package' column
merged_df <- merge(df_school_hours_app_summary, df_categories, by = "package", all.x = TRUE)


# Count how many times specific apps appear in data 
df_top_10_counts <- merged_df %>% 
  group_by(app_name) %>% 
  summarize(count = n())


write.csv(df_top_10_counts, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/school-hours_top10-apps.csv", row.names = FALSE)
