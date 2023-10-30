# README ------------------------------------------------------------------------------------------------------
# Program takes in instructional_condition files [online, f2f, blended, non-school]
# Generated descriptive table for each condition based on relevent metrics [usage, switches, sessions] for each ID


# IMPORT LIBRARIES --------------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)


# IMPORT DATA -------------------------------------------------------------------------------------------------
df_f2f <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_f2f_participants.csv")
df_online <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_online_participants.csv")
df_non_school <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_non_school_participants.csv")
df_blended <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_blended_participants.csv")


# PREPROCESSING ------------------------------------------------------------------------------------------------
# Update inf values for sessions 
df_f2f$session_count_impute[df_f2f$session_count_impute == 0] <- 1
df_online$session_count_impute[df_online$session_count_impute == 0] <- 1
df_non_school$session_count_impute[df_non_school$session_count_impute == 0] <- 1
df_blended$session_count_impute[df_blended$session_count_impute == 0] <- 1

# Update inf values for switches 
df_f2f$app_switches_impute[df_f2f$app_switches_impute == 0] <- 1
df_online$app_switches_impute[df_online$app_switches_impute == 0] <- 1
df_non_school$app_switches_impute[df_non_school$app_switches_impute == 0] <- 1
df_blended$app_switches_impute[df_blended$app_switches_impute == 0] <- 1

# Summarize data by ID for f2f
df_f2f_participant_metrics <- df_f2f %>%
  select(ID, use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            sessions_daily = sum(session_count_impute),
            switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute)) %>%
  group_by(ID) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE),
            total_observations = length(date_local_original))


# Summarize data by ID for online
df_online_participant_metrics <- df_online %>%
  select(ID, use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            sessions_daily = sum(session_count_impute),
            switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute)) %>%
  group_by(ID) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE),
            total_observations = length(date_local_original))


# Summarize data by ID for non_school
df_non_school_participant_metrics <- df_non_school %>%
  select(ID, use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            sessions_daily = sum(session_count_impute),
            switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute)) %>%
  group_by(ID) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE),
            total_observations = length(date_local_original))

# Summarize data by ID for blended
df_blended_participant_metrics <- df_blended %>%
  select(ID, use_duration_minutes_impute, session_count_impute, app_switches_impute, date_local_original) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            sessions_daily = sum(session_count_impute),
            switches_daily = sum(app_switches_impute),
            session_length_daily = sum(use_duration_minutes_impute) / sum(session_count_impute), 
            switches_velocity_daily = sum(use_duration_minutes_impute) / sum(app_switches_impute)) %>%
  group_by(ID) %>% 
  summarize(use_duration_avg_daily = mean(use_duration_daily, na.rm = TRUE),
            sessions_avg_daily = mean(sessions_daily, na.rm = TRUE), 
            switches_avg_daily = mean(switches_daily, na.rm = TRUE),
            avg_session_length = mean(session_length_daily, na.rm = TRUE),
            avg_switch_velocity = mean(switches_velocity_daily, na.rm = TRUE),
            total_observations = length(date_local_original))


# Find IDs that appear in all data frames
# common_ids <- intersect(intersect(intersect(df_f2f_participant_metrics$ID, df_online_participant_metrics$ID), 
#                                   df_non_school_participant_metrics$ID), df_blended_participant_metrics$ID)

common_ids <- intersect(intersect(df_online_participant_metrics$ID, df_blended_participant_metrics$ID), 
                                  df_non_school_participant_metrics$ID)

common_ids <- setdiff(df_f2f_participant_metrics$ID, df_non_school_participant_metrics$ID)

# Display common IDs
print(common_ids)

# SUMMARIZE NON-SCHOOL ----------------------------------------------------------------------------------------
# Usage data ----
df_non_school_usage <- df_non_school_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(use_duration_avg_daily, na.rm = TRUE),
            sd = sd(use_duration_avg_daily, na.rm = TRUE),
            median = median(use_duration_avg_daily, na.rm = TRUE),
            min = min(use_duration_avg_daily, na.rm = TRUE),
            max = max(use_duration_avg_daily, na.rm = TRUE),
            
            conditon = 'non-school',
            metric = 'usage')


# Switches data ----
df_non_school_switches <- df_non_school_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_switch_velocity, na.rm = TRUE),
            sd = sd(avg_switch_velocity, na.rm = TRUE),
            median = median(avg_switch_velocity, na.rm = TRUE),
            min = min(avg_switch_velocity, na.rm = TRUE),
            max = max(avg_switch_velocity, na.rm = TRUE),
            
            conditon = 'non-school',
            metric = 'switch-velocity')


# Sessions data ----
df_non_school_sessions <- df_non_school_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_session_length, na.rm = TRUE),
            sd = sd(avg_session_length, na.rm = TRUE),
            median = median(avg_session_length, na.rm = TRUE),
            min = min(avg_session_length, na.rm = TRUE),
            max = max(avg_session_length, na.rm = TRUE),
            
            conditon = 'non-school',
            metric = 'session-length')


# Merge non-school metrics data 
df_non_school_stats_merged <- merge(df_non_school_usage, df_non_school_switches, all = TRUE) %>%
  merge(df_non_school_sessions, all = TRUE)


# SUMMARIZE ONLINE ----------------------------------------------------------------------------------------
# Usage data ----
df_online_usage <- df_online_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(use_duration_avg_daily, na.rm = TRUE),
            sd = sd(use_duration_avg_daily, na.rm = TRUE),
            median = median(use_duration_avg_daily, na.rm = TRUE),
            min = min(use_duration_avg_daily, na.rm = TRUE),
            max = max(use_duration_avg_daily, na.rm = TRUE),
            
            conditon = 'online',
            metric = 'usage')


# Switches data ----
df_online_switches <- df_online_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_switch_velocity, na.rm = TRUE),
            sd = sd(avg_switch_velocity, na.rm = TRUE),
            median = median(avg_switch_velocity, na.rm = TRUE),
            min = min(avg_switch_velocity, na.rm = TRUE),
            max = max(avg_switch_velocity, na.rm = TRUE),
            
            conditon = 'online',
            metric = 'switch-velocity')


# Sessions data ----
df_online_sessions <- df_online_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_session_length, na.rm = TRUE),
            sd = sd(avg_session_length, na.rm = TRUE),
            median = median(avg_session_length, na.rm = TRUE),
            min = min(avg_session_length, na.rm = TRUE),
            max = max(avg_session_length, na.rm = TRUE),
            
            conditon = 'online',
            metric = 'session-length')


# Merge online metrics data 
df_online_stats_merged <- merge(df_online_usage, df_online_switches, all = TRUE) %>%
  merge(df_online_sessions, all = TRUE)


# SUMMARIZE F2F --------------------------------------------------------------------------------------------
# Usage data ----
df_f2f_usage <- df_f2f_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(use_duration_avg_daily, na.rm = TRUE),
            sd = sd(use_duration_avg_daily, na.rm = TRUE),
            median = median(use_duration_avg_daily, na.rm = TRUE),
            min = min(use_duration_avg_daily, na.rm = TRUE),
            max = max(use_duration_avg_daily, na.rm = TRUE),
            
            conditon = 'f2f',
            metric = 'usage')


# Switches data ----
df_f2f_switches <- df_f2f_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_switch_velocity, na.rm = TRUE),
            sd = sd(avg_switch_velocity, na.rm = TRUE),
            median = median(avg_switch_velocity, na.rm = TRUE),
            min = min(avg_switch_velocity, na.rm = TRUE),
            max = max(avg_switch_velocity, na.rm = TRUE),
            
            conditon = 'f2f',
            metric = 'switch-velocity')


# Sessions data ----
df_f2f_sessions <- df_f2f_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_session_length, na.rm = TRUE),
            sd = sd(avg_session_length, na.rm = TRUE),
            median = median(avg_session_length, na.rm = TRUE),
            min = min(avg_session_length, na.rm = TRUE),
            max = max(avg_session_length, na.rm = TRUE),
            
            conditon = 'f2f',
            metric = 'session-length')


# Merge f2f metrics data 
df_f2f_stats_merged <- merge(df_f2f_usage, df_f2f_switches, all = TRUE) %>%
  merge(df_f2f_sessions, all = TRUE)


# SUMMARIZE BLENDED ----------------------------------------------------------------------------------------
# Usage data ----
df_blended_usage <- df_blended_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(use_duration_avg_daily, na.rm = TRUE),
            sd = sd(use_duration_avg_daily, na.rm = TRUE),
            median = median(use_duration_avg_daily, na.rm = TRUE),
            min = min(use_duration_avg_daily, na.rm = TRUE),
            max = max(use_duration_avg_daily, na.rm = TRUE),
            
            conditon = 'blended',
            metric = 'usage')


# Switches data ----
df_blended_switches <- df_blended_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_switch_velocity, na.rm = TRUE),
            sd = sd(avg_switch_velocity, na.rm = TRUE),
            median = median(avg_switch_velocity, na.rm = TRUE),
            min = min(avg_switch_velocity, na.rm = TRUE),
            max = max(avg_switch_velocity, na.rm = TRUE),
            
            conditon = 'blended',
            metric = 'switch-velocity')


# Sessions data ----
df_blended_sessions <- df_blended_participant_metrics %>%
  summarise(total_participants = n_distinct(ID),
            total_observations = sum(total_observations),
            
            mean = mean(avg_session_length, na.rm = TRUE),
            sd = sd(avg_session_length, na.rm = TRUE),
            median = median(avg_session_length, na.rm = TRUE),
            min = min(avg_session_length, na.rm = TRUE),
            max = max(avg_session_length, na.rm = TRUE),
            
            conditon = 'blended',
            metric = 'session-length')


# Merge f2f metrics data 
df_blended_stats_merged <- merge(df_blended_usage, df_blended_switches, all = TRUE) %>%
  merge(df_blended_sessions, all = TRUE)


# MERGE CONDITION DATA --------------------------------------------------------------------------------------
df_conditions_merged <- merge(df_f2f_stats_merged, df_online_stats_merged, all = TRUE) %>%
  merge(df_non_school_stats_merged, all = TRUE) %>% 
  merge(df_blended_stats_merged, all = TRUE)


# EXPORT DESCRIPTIVES ----------------------------------------------------------------------------------------
write.csv(df_conditions_merged, "/Users/baselhussein/Projects/ttf/school-hours-analysis/data/school-hours_descriptives.csv", row.names = FALSE)

