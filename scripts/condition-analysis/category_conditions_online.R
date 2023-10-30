# README -------------------------------------------------------------------------------------------------------
# Program takes in hourly data from each condition [blended, online, f2f, non-school] 
# Calculates total category-usage across all apps
# Exports 4 files, one for each condition 


# Import packages -------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)


# Import data -------------------------------------------------------------------------------------------------------
# Import condition data
df_online <- read_csv("/Users/baselhussein/Projects/ttf/school-hours-analysis/data/condition/df_online_participants.csv")

# Import app categories and packages
df_app_categories <- read_csv("/Users/baselhussein/Projects/ttf/data/app-categories/df_categories_tagged.csv")
df_app_categories$package <- paste(df_app_categories$package, "_mins_impute", sep = "")

# Preprocessing -------------------------------------------------------------------------------------------------------
# Drop str columns 
df_online_drop_str <- df_online %>%
  select(-days_since_start, -date_local_hour_night_adj, -missing_time_minutes, -date_local_night_adj, -hour_original, -hour_sorting, 
         -face_to_face_instruction, -online_instruction, -instruction_condition, -day_type, -holiday, -crit1)

# Calculate daily means of each participant
df_online_daily_sum <- df_online_drop_str %>%
  group_by(ID, date_local_original) %>%
  summarise(across(everything(), sum))

# Fill NaN values w 0s
df_online_daily_sum_fill_nan <- df_online_daily_sum %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

# Calculate mean across days for each participant 
df_online_participant_mean <- df_online_daily_sum_fill_nan %>%
  select(-date_local_original) %>%
  group_by(ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) 

# Select package names w/ _mins_impute tag
df_online_app_usage <- df_online_participant_mean %>%
  select(contains("_mins_impute"))

# Generate list of column names for mapping 
col_mapping <- setNames(df_app_categories$category_tagged, df_app_categories$package)

# Select relevant columns from categories file 
df_app_categories_select <- df_app_categories %>%
  select(package, category_tagged)

# Remove spaces from the "category_tagged" column
df_app_categories_select$category_tagged <- gsub(" ", "", df_app_categories_select$category_tagged)

# Replace "&" in "category_tagged" column
df_app_categories_select$category_tagged <- gsub("&", "And", df_app_categories_select$category_tagged)

# Tag packages w/ relevant app-category
colnames(df_online_app_usage) <- ifelse(
  colnames(df_online_app_usage) %in% df_app_categories_select$package,
  df_app_categories_select$category_tagged[match(colnames(df_online_app_usage), df_app_categories_select$package)],
  colnames(df_online_app_usage)
)

# If NaN, tag with no-category-info
colnames(df_online_app_usage)[is.na(colnames(df_online_app_usage))] <- "noCategoryInfo"

# Transpose df for combining matching categories 
transposed_df <- t(df_online_app_usage) %>%
  as.data.frame() %>%
  mutate(row_index = row_number())

transposed_df_index <- transposed_df %>%
  rownames_to_column("category") 

transposed_df_index$category <- gsub("\\..*", "", transposed_df_index$category)

# Sum duplicate categories 
transposed_df_index_grouped <- transposed_df_index %>% 
  group_by(category) %>%
  summarise(across(everything(), sum)) 

transposed_df_index_grouped_drop <- transposed_df_index_grouped %>%
  select(-row_index)

# Transpose to generate summary stats 
transpose_summary_stats <- t(transposed_df_index_grouped_drop)
transposed_df_categories <- as.data.frame(transpose_summary_stats)

col_names_categories <- as.character(transposed_df_categories[1, ])

transposed_df_categories <- transposed_df_categories[-1, ]

# Set the new column names
colnames(transposed_df_categories) <- col_names_categories

# Reset index 
rownames(transposed_df_categories) <- NULL

# Convert all entries to numeric
for (col in names(transposed_df_categories)) {
  transposed_df_categories[[col]] <- as.numeric(transposed_df_categories[[col]])
}

#str(transposed_df_categories)

# Rename columns for visualization 
names(transposed_df_categories)[names(transposed_df_categories) == 'VideoPlayersAndEditors'] <- 'Video Players'
names(transposed_df_categories)[names(transposed_df_categories) == 'MusicAndAudio'] <- 'Music & Audio'
names(transposed_df_categories)[names(transposed_df_categories) == 'HealthAndFitness'] <- 'Health & Fitness'

# Descriptives for categories --------------------------------------------------------------------------------
column_means <- colMeans(transposed_df_categories)
mean_df <- data.frame(Mean = column_means)
mean_df$Mean <- format(mean_df$Mean, scientific = FALSE)


# Boxplots  --------------------------------------------------------------------------------------------------
# Generate boxplots for top10 categories

# Top 10: Social, communication, video, gaming, entertainment, music, photography, productivity, educational, travel

# Top 10: Update based on results 
column_names <- c("Video Players", "Social", "Communication", "Gaming", "Entertainment",
                  "Productivity", "Educational", "Music & Audio", "Photography", "Health & Fitness")

column_names <- rev(column_names)

# Create a list to store the data for each column for boxplot 
data_list <- lapply(column_names, function(col) {
  transposed_df_categories[[col]]  # Reversing the data
})

# Create a data frame from the reversed data
data_boxplot <- data.frame(value = unlist(data_list),
                   column = rep(column_names, each = length(data_list[[1]])))

# Reorder the levels of the "column" variable based on the order of column_names
data_boxplot$column <- factor(data_boxplot$column, levels = column_names)

mean_value <- mean(data_boxplot$value)

# Create the horizontal box plot using ggplot2
# ggplot_bloxplot_online <- ggplot(data_boxplot, aes(y = column, x = value)) +
#   geom_boxplot(horizontal = TRUE) +
#   labs(title = "Top10 Categories (online)",
#        x = "Minutes per Day", y = NULL) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(angle = 0, hjust = 1),
#     panel.background = element_rect(fill = "white")  # Set the background color to white
#   )

# Create the horizontal box plot with mean annotations
ggplot_bloxplot_online <- ggplot(data_boxplot, aes(y = column, x = value)) +
  geom_boxplot(
    horizontal = TRUE,
    fill = "#4C72B0",
    color = "#2F4F4F",
    outlier.color = "Red"
  ) +
  labs(
    title = "Online",
    x = "Minutes Per Day",
    y = NULL
  ) +
  scale_x_continuous(limits = c(0, 360)) +
  #theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.line = element_line(size = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "#DDDDDD"),
    panel.grid.major.y = element_line(color = "#DDDDDD"), # Add gray grid lines
    panel.grid.minor = element_blank()  # Hide minor grid lines
  )


ggplot_bloxplot_online

# Export the boxplot
ggsave(
  filename = "/Users/baselhussein/Projects/ttf/school-hours-analysis/vis/ggplot_boxplot_online.png", 
  plot = ggplot_bloxplot_online, 
  width = 6, height = 3, dpi = 1000
)