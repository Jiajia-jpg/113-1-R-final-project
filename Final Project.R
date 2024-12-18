# Load the googlesheets4 package
library(googlesheets4)

# Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1R3PHHHA3f40X0oLpcTQ_Md6TwNNgLfugRY0vpdTRYWQ/edit?gid=0#gid=0"

# Read the first sheet (default) or specify the sheet by name or index
df <- read_sheet(sheet_url)

# View the data
print(df)
# Glimpse the structure of the data frame
glimpse(df)
library(tidyverse)

# Read the Google Sheet (assuming df is already loaded)
df <- df %>%
  # Clean up column names by making them concise and using snake_case
  rename(
    year_roc       = 民國年,
    school_type    = 學制,
    total_schools  = `校數/總計【統計數值】`,
    national_schools = `校數/國立【統計數值】`,
    municipal_schools = `校數/市立【統計數值】`,
    private_schools   = `校數/私立【統計數值】`
  ) %>%
  # Convert ROC year to Western year by adding 1911
  mutate(year_western = year_roc + 1911) %>%
  # Pivot district-specific columns to long format
  pivot_longer(
    cols = starts_with("校數/"),
    names_to = "district",
    values_to = "school_count",
    names_pattern = "校數/(.*)"
  ) %>%
  # Convert district names to readable format
  mutate(district = str_remove(district, "【統計數值】"))

# View the cleaned and transformed data
print(df)
library(tidyverse)

# Read and clean the data
df_clean <- df %>%
  # Rename ROC year column and simplify other column names
  rename(
    year_roc       = 民國年,
    school_type    = 學制,
    total_schools  = `校數/總計【統計數值】`,
    national_schools = `校數/國立【統計數值】`,
    municipal_schools = `校數/市立【統計數值】`,
    private_schools   = `校數/私立【統計數值】`
  ) %>%
  # Convert ROC year to Western year
  mutate(year_western = year_roc + 1911)

# Summarize the school counts by school type
school_summary <- df_clean %>%
  select(school_type, total_schools, national_schools, municipal_schools, private_schools) %>%
  group_by(school_type) %>%
  summarise(
    total = sum(total_schools, na.rm = TRUE),
    national = sum(national_schools, na.rm = TRUE),
    municipal = sum(municipal_schools, na.rm = TRUE),
    private = sum(private_schools, na.rm = TRUE)
  )

# View the summarized data
print(school_summary)

library(tidyverse)

# Read and clean the data
df_clean <- df %>%
  # Rename ROC year column and simplify other column names
  rename(
    year_roc          = `民國年`,
    school_type       = `學制`,
    total_schools     = `校數/總計【統計數值】`,
    national_schools  = `校數/國立【統計數值】`,
    municipal_schools = `校數/市立【統計數值】`,
    private_schools   = `校數/私立【統計數值】`
  ) %>%
  # Convert ROC year to Western year
  mutate(year_western = year_roc + 1911)

# Summarize the school counts by school type
school_summary <- df_clean %>%
  select(school_type, total_schools, national_schools, municipal_schools, private_schools) %>%
  group_by(school_type) %>%
  summarise(
    total = sum(total_schools, na.rm = TRUE),
    national = sum(national_schools, na.rm = TRUE),
    municipal = sum(municipal_schools, na.rm = TRUE),
    private = sum(private_schools, na.rm = TRUE)
  )

# View the summarized data
print(school_summary)
glimpse(string_df)

library(tidyverse)

# Extract city and district from taiwan_address
df <- df %>%
  mutate(
    city = str_extract(taiwan_address, "^[^市]+市"),
    district = str_extract(taiwan_address, "(?<=市)[^區]+區")
  )

# View the result
print(df)

# Filter rows where skill includes both "Python" and "R"
df_python_r <- df %>%
  filter(str_detect(skill, "Python") & str_detect(skill, "R"))

print(df_python_r)

# Count rows based on matches_pattern and from_taichung_city
summary <- df %>%
  summarise(
    matches_pattern_count = sum(matches_pattern, na.rm = TRUE),
    from_taichung_count = sum(from_taichung_city, na.rm = TRUE)
  )

print(summary)

glimpse(df)

df %>%
  group_by(district) %>%
  summarise(
    total_schools = sum(school_count, na.rm = TRUE)
  )
df %>%
  group_by(year_western, school_type) %>%
  summarise(
    total = sum(total_schools, na.rm = TRUE),
    national = sum(national_schools, na.rm = TRUE),
    municipal = sum(municipal_schools, na.rm = TRUE),
    private = sum(private_schools, na.rm = TRUE)
  )

df %>%
  group_by(district, school_type) %>%
  summarise(
    total_schools = sum(school_count, na.rm = TRUE)
  )

df %>%
  group_by(district, school_type) %>%
  summarise(total_schools = sum(school_count, na.rm = TRUE)) %>%
  pivot_wider(names_from = school_type, values_from = total_schools, values_fill = 0)

df %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

