# README ------------------------------------------------------------------------------------------------------
# Program takes in instructional_condition files [online, f2f, blended, non-school]
# Fits a multilevel regression model on conditions


# IMPORT LIBRARIES --------------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)


# IMPORT DATA -------------------------------------------------------------------------------------------------
df_f2f <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_f2f_participants.csv")
df_online <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_online_participants.csv")
df_non_school <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_non_school_participants.csv")
df_blended <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_blended_participants.csv")
df_demographics <- read_csv("/Users/baselhussein/Projects/ttf/data/Teen_Demographics_n144.csv")


# PREPROCESSING ------------------------------------------------------------------------------------------------
# Update instructional_condition column 
df_non_school$instruction_condition <- 'nonschool'
df_f2f$instruction_condition <- 'face'

# Merge by date 
df_non_school_grouped <- df_non_school %>%
  select(ID, date_local_original, use_duration_minutes_impute, instruction_condition) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            instruction_condition = 'nonschool')

df_blended_grouped <- df_blended %>%
  select(ID, date_local_original, use_duration_minutes_impute, instruction_condition) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            instruction_condition = 'blended')

df_f2f_grouped <- df_f2f %>%
  select(ID, date_local_original, use_duration_minutes_impute, instruction_condition) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            instruction_condition = 'face')

df_online_grouped <- df_online %>%
  select(ID, date_local_original, use_duration_minutes_impute, instruction_condition) %>%
  group_by(ID, date_local_original) %>% 
  summarize(use_duration_daily = sum(use_duration_minutes_impute), 
            instruction_condition = 'online')
  
# Combine dfs
combined_conditions <- df_f2f_grouped %>%
  bind_rows(df_online_grouped) %>%
  bind_rows(df_non_school_grouped) %>%
  bind_rows(df_blended_grouped)

# Merge demographics 
names(df_demographics)[names(df_demographics) == 'participant_id'] <- 'ID'
combined_conditions_demographics <- merge(combined_conditions, df_demographics, by = "ID", all = TRUE)

# Add season column 
combined_conditions_seasons <- combined_conditions_demographics %>%
  mutate(month = as.numeric(format(date_local_original, "%m")), # Extract the month as a numeric value
         season = case_when(
           month %in% 9:11 ~ "fall",                           # September to December
           month %in% 1:2 | month == 12 ~ "winter",            # December to March
           month %in% 3:5 ~ "spring",                          # March to June
           month %in% 6:8 ~ "summer"                           # June to September
         )) %>%
  select(-month)

# Add weekday/weekend column for summer 
combined_conditions_seasons <- combined_conditions_seasons[combined_conditions_seasons$season == 'summer', ]
combined_conditions_seasons$day <- ifelse(weekdays(combined_conditions_seasons$date_local_original) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
combined_conditions_seasons$weekday <- ifelse(combined_conditions_seasons$day == 'Weekday', 1, 0)
combined_conditions_seasons$weekend <- 0

# Clean df
# combined_conditions_cleaned <- combined_conditions %>%
#   select(ID, use_duration_daily, instruction_condition) 

# Clear NaN participants 
combined_conditions_seasons_drop <- combined_conditions_seasons[complete.cases(combined_conditions_seasons$use_duration_daily), ]

# Export demographics for spot-check



# DUMMY VARIABLES -------------------------------------------------------------------------------------
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-dummy-coding/


# INSTRUCTION 
dummy_matrix_instruction <- model.matrix(~ instruction_condition - 1, data = combined_conditions_seasons_drop)

# Convert the matrix to a data frame
dummies_instruction  <- as.data.frame(dummy_matrix_instruction)

# Rename the columns if needed (optional)
colnames(dummies_instruction) <- gsub("Category", "Category_", colnames(dummies_instruction))

# Combine data and instruction condition dummy values 
combined_conditions_instruction <- bind_cols(combined_conditions_seasons_drop, dummies_instruction)


# # SEASON
# dummy_matrix_seasons <- model.matrix(~ season - 1, data = combined_conditions_instruction)
# 
# # Convert the matrix to a data frame
# dummies_seasons  <- as.data.frame(dummy_matrix_seasons)
# 
# # Rename the columns if needed (optional)
# colnames(dummies_seasons) <- gsub("Category", "Category_", colnames(dummies_seasons))
# 
# # Combine data and instruction condition dummy values 
# combined_conditions_instruction_seasons <- bind_cols(combined_conditions_instruction, dummies_seasons)


# TIMEZONE 
dummy_matrix_timezone <- model.matrix(~ tz - 1, data = combined_conditions_instruction_seasons)

# Convert the matrix to a data frame
dummies_timezone  <- as.data.frame(dummy_matrix_timezone)

# Rename the columns if needed (optional)
colnames(dummies_timezone) <- gsub("Category", "Category_", colnames(dummies_timezone))

# Combine data and instruction condition dummy values 
combined_conditions_instruction_seasons_tz <- bind_cols(combined_conditions_instruction_seasons, dummies_timezone)
  

# REGION  
# NaN values in region, fill for dummies
combined_conditions_instruction_seasons_tz$state_region <- replace(combined_conditions_instruction_seasons_tz$state_region, 
                                                                   is.na(combined_conditions_instruction_seasons_tz$state_region), 
                                                                   "None")

dummy_matrix_region <- model.matrix(~ state_region - 1, data = combined_conditions_instruction_seasons_tz)

# Convert the matrix to a data frame
dummies_region  <- as.data.frame(dummy_matrix_region)

# Rename the columns if needed (optional)
colnames(dummies_region) <- gsub("Category", "Category_", colnames(dummies_region))

# Combine data and instruction condition dummy values 
combined_conditions_instruction_seasons_tz_region <- bind_cols(combined_conditions_instruction_seasons_tz, dummies_region)


# CLEAN NAMES ------------------------------------------------------------------------------------------------
daily_usage_dummies <- combined_conditions_instruction_seasons_tz_region
daily_usage_dummies_drop <- subset(daily_usage_dummies, 
                                   select = -c(date_local_original, instruction_condition, state_region, tz, season))


names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'cgenderfirst'] <- 'gender'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'cageyrs01'] <- 'age'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'white_or_no'] <- 'race'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'cpeducmax'] <- 'parentEdu'

# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'instruction_conditionblended'] <- 'blended'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'instruction_conditionnonschool'] <- 'nonSchool'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'instruction_conditionface'] <- 'faceToFace'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'instruction_conditiononline'] <- 'online'

# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'seasonfall'] <- 'sznFall'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'seasonspring'] <- 'sznSpring'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'seasonsummer'] <- 'sznSummer'
# names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'seasonwinter'] <- 'sznWinter'

names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'state_regionNone'] <- 'regNan'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'state_regionNorth Central'] <- 'regNorthCentral'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'state_regionNortheast'] <- 'regNorthEast'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'state_regionSouth'] <- 'regSouth'
names(daily_usage_dummies_drop)[names(daily_usage_dummies_drop) == 'state_regionWest'] <- 'regWest'

# Rename 
df_daily_usage <- daily_usage_dummies_drop

# Print column names 
column_names <- colnames(daily_usage_dummies_drop)
print(column_names)

# MODEL ------------------------------------------------------------------------------------------------
# Response variable: use_duration_minutes_impute
# Fixed effect: instruction_condition (the variable that has a systematic influence on usage)
# Random effect: ID (student variability, some students in multiple conditions...)

# Fit the multilevel regression model
model <- lmer(use_duration_daily ~ # Response variable 
                
                ### Fixed effects 
                # Instruction 
                weekend + # Reference, all 0s
                weekday +
                
                # Season
                # sznFall + 
                # sznSpring + 
                # sznSummer + 
                # sznWinter + 
                # 
                # timezone
                tzCentral + 
                tzEastern + 
                tzMountain +
                tzPacific + 
                
                # region 
                regNan + # TODO: check 
                regNorthCentral + 
                regNorthEast + 
                regSouth + 
                regWest + 
                
                # gender
                gender + 
                
                # age
                age +
                
                # race
                race +
                
                # parent education 
                parentEdu +
              
              
                ### Random effects 
                (1 | ID), 
              
                data = df_daily_usage
              )

# Print column names 
# column_names <- colnames(combined_conditions_instruction_seasons)
# print(column_names)

# Review model output 

summary(model)
coef(summary(as(model,"merModLmerTest")))


# EXPORT ------------------
write.csv(df_daily_usage, "/Users/baselhussein/Downloads/df_daily_usage_multilevel.csv", row.names = FALSE)
