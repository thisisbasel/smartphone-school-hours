# README ------------------------------------------------------------------------------------------------------
# This program is to investigate patterns of smartphone use in 3 participants 
# We look at their metrics [usage, sessions, switches] across their respective conditions [online, f2f, blended, non-school]


# IMPORT LIBRARIES --------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)


# IMPORT DATA -------------------------------------------------------------------------------------------------
# Import 4 conditions 
df_non_school <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_non_school_participants.csv")
df_online <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_online_participants.csv")
df_f2f <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_f2f_participants.csv")
df_blended <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_blended_participants.csv")

# Import app categories and packages
df_app_categories <- read_csv("/Users/baselhussein/Projects/ttf/data/app-categories/df_categories_tagged.csv")
df_app_categories$package <- paste(df_app_categories$package, "_mins_impute", sep = "")


# 229425 ------------------------------------------------------------------------------------------------------
# Subset data 
df_non_school_229425 <- subset(df_non_school, ID == 229425)
df_online_229425 <- subset(df_online, ID == 229425)
df_f2f_229425 <- subset(df_f2f, ID == 229425)
df_blended_229425 <- subset(df_blended, ID == 229425)

# Top Apps ---------------------------------------------------------------------------------------------------- 
# f2f ----
# Select hourly f2f data 
df_school_year_229425 <- read.csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/processed/df_school_year_229425.csv")
df_school_year_229425_crit1 <- df_school_year_229425 %>%
  filter(crit1 == 'crit1') 

df_school_year_229425_crit1_f2f <- df_school_year_229425_crit1 %>%
  filter(instruction_condition == 'face-to-face' & day_type == 'Weekday' & holiday == 'None') # is.nan(holiday) not working...? 

# Unique days 
total_unique_days_f2f <- df_school_year_229425_crit1_f2f %>% 
  summarise(total_unique_days = n_distinct(date_local_original))

# Keep only imputed app-usage
df_app_usage_f2f <- df_school_year_229425_crit1_f2f %>%
  select(contains("_mins_impute"))

col_sums_f2f <- colSums(df_app_usage_f2f)

# Create a new data frame to store the column names and their sum values
df_apps_f2f <- data.frame(column_name = names(col_sums_f2f), sum_value = col_sums_f2f)
df_apps_f2f$avg_usage_day <- df_apps_f2f$sum_value / total_unique_days_f2f$total_unique_days

df_apps_f2f <- df_apps_f2f %>% 
  rownames_to_column(var = "new_index")

# Find top 20 apps 
df_apps_top_20_f2f <- df_apps_f2f %>%
  arrange(desc(avg_usage_day)) %>% 
  head(20)

# Drop the 'new_index' column
df_apps_top_20_f2f <- subset(df_apps_top_20_f2f, select = -new_index)

# Rename columns 
colnames(df_apps_top_20_f2f)[colnames(df_apps_top_20_f2f) == "column_name"] <- "package"

# Merge the data frames based on the 'package' column
df_top_apps_merged_f2f <- merge(df_apps_top_20_f2f, df_app_categories, by = "package", all.x = TRUE)

df_top_apps_f2f_usage <- df_top_apps_merged_f2f %>%
  select(avg_usage_day, app_name)


# Online ----
df_school_year_229425_crit1_online <- df_school_year_229425_crit1 %>%
  filter(instruction_condition == 'online' & day_type == 'Weekday' & holiday == 'None')

# Unique days 
total_unique_days_online <- df_school_year_229425_crit1_online %>% 
  summarise(total_unique_days = n_distinct(date_local_original))

# Keep only imputed app-usage
df_app_usage_online <- df_school_year_229425_crit1_online %>%
  select(contains("_mins_impute"))

col_sums_online <- colSums(df_app_usage_online)

# Create a new data frame to store the column names and their sum values
df_apps_online <- data.frame(column_name = names(col_sums_online), sum_value = col_sums_online)
df_apps_online$avg_usage_day <- df_apps_online$sum_value / total_unique_days_online$total_unique_days

df_apps_online <- df_apps_online %>% 
  rownames_to_column(var = "new_index")

# Find top 20 apps 
df_apps_top_20_online <- df_apps_online %>%
  arrange(desc(avg_usage_day)) %>% 
  head(20)

# Drop the 'new_index' column
df_apps_top_20_online <- subset(df_apps_top_20_online, select = -new_index)

# Rename columns 
colnames(df_apps_top_20_online)[colnames(df_apps_top_20_online) == "column_name"] <- "package"

# Merge the data frames based on the 'package' column
df_top_apps_merged_online <- merge(df_apps_top_20_online, df_app_categories, by = "package", all.x = TRUE)

df_top_apps_online_usage <- df_top_apps_merged_online %>%
  select(avg_usage_day, app_name)


# nonSchool ----
df_school_year_229425_crit1_nonSchool <- df_school_year_229425_crit1 %>%
  filter(day_type == 'Weekend' | holiday == 'winter-break' | holiday == 'holiday')

# Unique days 
total_unique_days_nonSchool <- df_school_year_229425_crit1_nonSchool %>% 
  summarise(total_unique_days = n_distinct(date_local_original))

# Keep only imputed app-usage
df_app_usage_nonSchool <- df_school_year_229425_crit1_nonSchool %>%
  select(contains("_mins_impute"))

col_sums_nonSchool <- colSums(df_app_usage_nonSchool)

# Create a new data frame to store the column names and their sum values
df_apps_nonSchool <- data.frame(column_name = names(col_sums_nonSchool), sum_value = col_sums_nonSchool)
df_apps_nonSchool$avg_usage_day <- df_apps_nonSchool$sum_value / total_unique_days_nonSchool$total_unique_days

df_apps_nonSchool <- df_apps_nonSchool %>% 
  rownames_to_column(var = "new_index")

# Find top 20 apps 
df_apps_top_20_nonSchool <- df_apps_nonSchool %>%
  arrange(desc(avg_usage_day)) %>% 
  head(20)

# Drop the 'new_index' column
df_apps_top_20_nonSchool <- subset(df_apps_top_20_nonSchool, select = -new_index)

# Rename columns 
colnames(df_apps_top_20_nonSchool)[colnames(df_apps_top_20_nonSchool) == "column_name"] <- "package"

# Merge the data frames based on the 'package' column
df_top_apps_merged_nonSchool <- merge(df_apps_top_20_nonSchool, df_app_categories, by = "package", all.x = TRUE)

df_top_apps_nonSchool_usage <- df_top_apps_merged_nonSchool %>%
  select(avg_usage_day, app_name)


# Descriptives -------------------------------------------------------------------
# Non-school  
df_non_school_stats_229425 <- df_non_school_229425 %>%
  select(use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            #sessions_daily = sum(session_count_impute),
            #switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute),
            condition = "non_school") %>%
  group_by(condition) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            # sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            # switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE), 
            total_observations = length(date_local_original))

# Online
df_online_stats_229425 <- df_online_229425 %>%
  select(use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            #sessions_daily = sum(session_count_impute),
            #switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute),
            condition = "online") %>%
  group_by(condition) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            # sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            # switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE), 
            total_observations = length(date_local_original))

# f2f
df_f2f_stats_229425 <- df_f2f_229425 %>%
  select(use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            #sessions_daily = sum(session_count_impute),
            #switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute),
            condition = "f2f") %>%
  group_by(condition) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            # sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            # switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE),
            total_observations = length(date_local_original))

# Blended
df_blended_stats_229425 <- df_blended_229425 %>%
  select(use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            #sessions_daily = sum(session_count_impute),
            #switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute),
            condition = "blended") %>%
  group_by(condition) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            # sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            # switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE), 
            total_observations = length(date_local_original))

# Combine summary stats 
df_summary_stats_229425 <- df_non_school_stats_229425 %>%
  rbind(df_online_stats_229425) %>%
  rbind(df_f2f_stats_229425) %>%
  rbind(df_blended_stats_229425)

df_summary_stats_229425$ID <- "229425"

# Export 229425
write.csv(df_summary_stats_229425, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/case-studies/descriptives_229425.csv", row.names = FALSE)


# Vis ----------------------------------------------------------------------------
# Import raw hourly data 
df_hourly_229425 <- read.csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/processed/df_school_year_229425.csv")


# Add modality column to raw hourly data 
df_hourly_229425_modality <- df_hourly_229425 %>%
  mutate(
    modality = case_when(
      instruction_condition == "online" & day_type == "Weekday" & holiday == 'None' ~ "online",
      instruction_condition == "face-to-face" & day_type == "Weekday" & holiday == 'None' ~ "f2f",
      day_type == "Weekend" | holiday == 'winter-break' | holiday == 'holiday' ~ "non_school",
      TRUE ~ "other"
    )
  ) %>%
  mutate(
    use_duration_minutes_impute = ifelse(crit1 == "missing", NaN, use_duration_minutes_impute), 
    session_count_impute = ifelse(crit1 == "missing", NaN, session_count_impute), 
    app_switches_impute = ifelse(crit1 == "missing", NaN, app_switches_impute), 
    
    use_duration_minutes_impute = ifelse(modality == "other", NaN, use_duration_minutes_impute), 
    session_count_impute = ifelse(modality == "other", NaN, session_count_impute), 
    app_switches_impute = ifelse(modality == "other", NaN, app_switches_impute), 
    
  )


df_vis_229425 <- df_hourly_229425_modality %>%
  select(days_since_start, use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original, modality) %>%
  group_by(date_local_original) %>%
  summarise(use_duration_daily = sum(use_duration_minutes_impute),
            avg_session_length = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            avg_switches_velocity = sum(use_duration_minutes_impute) / sum(app_switches_impute),
            modality = paste(modality))



df_vis_229425_daily <- df_vis_229425 %>%
  distinct()

# Convert the date_local_original column to Date format
df_vis_229425_daily$date_local_original <- as.Date(df_vis_229425_daily$date_local_original, format = "%Y-%m-%d")

# Create the ggplot
gg_usage_229425 <- ggplot(df_vis_229425_daily, aes(x = date_local_original, y = use_duration_daily)) +
  geom_point(aes(color = modality)) +  
  geom_line(color = "#BBBBBB") +
  labs(x = "", y = "Smartphone Usage (Mins)", title = "Case-Study 1: Daily Usage (9AM-3PM)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "30 days", date_labels = "%Y-%m-%d") +
  scale_color_manual(name = "Modality", 
                     breaks = c("f2f", "non_school", "online"), 
                     labels = c("Face-To-Face", "Non-School Day", 'Online'),
                     values = c("f2f" = "#00AFBB", "non_school" = "brown", "online" = "#E7B800"))

print(gg_usage_229425)

# Session vis
gg_sessions_229425 <- ggplot(df_vis_229425_daily, aes(x = date_local_original, y = avg_session_length)) +
  geom_point(aes(color = modality)) +  
  geom_line(color = "#BBBBBB") +
  labs(x = "", y = "Average Session Length (Mins)", title = "Case-Study 1: Average Length of Session (9AM-3PM)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "30 days", date_labels = "%Y-%m-%d") +
  scale_color_manual(name = "Modality", 
                     breaks = c("f2f", "non_school", "online"), 
                     labels = c("Face-To-Face", "Non-School Day", 'Online'),
                     values = c("f2f" = "#00AFBB", "non_school" = "brown", "online" = "#E7B800"))

# Switches vis 
gg_switches_229425 <- ggplot(df_vis_229425_daily, aes(x = date_local_original, y = avg_switches_velocity)) +
  geom_point(aes(color = modality)) +  
  geom_line(color = "#BBBBBB") +
  labs(x = "", y = "Average Time Between App-Switches (Mins)", title = "Case-Study 1: Average Time Between App-Switches (9AM-3PM)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "30 days", date_labels = "%Y-%m-%d") +
  scale_color_manual(name = "Modality", 
                     breaks = c("f2f", "non_school", "online"), 
                     labels = c("Face-To-Face", "Non-School Day", 'Online'),
                     values = c("f2f" = "#00AFBB", "non_school" = "brown", "online" = "#E7B800"))

print(gg_switches_229425)

# Save the plot as a PNG file 
ggsave(filename = "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/case-studies/plots/229425/usage_229425.png", 
       plot = gg_usage_229425, 
       width = 10, height = 3.5, dpi = 1000)

ggsave(filename = "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/case-studies/plots/229425/sessions_229425.png", 
       plot = gg_sessions_229425, 
       width = 10, height = 3.5, dpi = 1000)

ggsave(filename = "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/case-studies/plots/229425/switches_229425.png", 
       plot = gg_switches_229425, 
       width = 10, height = 3.5, dpi = 1000)
