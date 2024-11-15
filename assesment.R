library(tidyverse)
library(here)
library(janitor) 
library(gt) 

daylight = read.csv("data/daylight.csv", header = TRUE, sep = ",")

files <- list.files(here("data","consecutive_data"), pattern = "csv")

library(dplyr)
library(purrr)
library(readr)

# Function to handle files with missing DMDCode
read_and_standardize <- function(file) {
  data <- read_csv(here("data", "consecutive_data", file)) %>%
    # Ensure DMDCode exists and is a character
    mutate(DMDCode = ifelse("DMDCode" %in% colnames(.), as.character(DMDCode), NA_character_))
  return(data)
}

# Read and combine all files
consecutive_data <- files %>%
  map_dfr(~ read_and_standardize(.x))

vitamin_d_data <- consecutive_data %>%
  filter(str_detect(BNFItemDescription, regex("d3|d2|calciferol|cholecalciferol|ergocalciferol", ignore_case = TRUE)))

month_mapping <- c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, 
                   "jun" = 6, "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, 
                   "nov" = 11, "dec" = 12)

# Reshape data and use custom month mapping
long_daylight_data <- daylight %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "daylight") %>%
  mutate(
    # Convert month abbreviations to numbers using the custom mapping
    month = month_mapping[month],
    # Create a new column in the format YYYYMM
    Month = sprintf("%04d%02d", year, month)
  ) %>%
  select(Month, daylight)  # Keep only Month and Daylight columns

filtered_daylight_data <- long_daylight_data %>%
  filter(Month >= 202108)

monthly_vitamin_d <- vitamin_d_data %>%
  group_by(PaidDateMonth) %>%
  summarise(total_paid_quantity = sum(PaidQuantity, na.rm = TRUE))

monthly_vitamin_d = monthly_vitamin_d %>% 
  mutate(PaidDateMonth = as.character(PaidDateMonth))

filtered_daylight_data = filtered_daylight_data %>% 
  mutate(Month = as.character(Month))

combined_data <- filtered_daylight_data %>%
  left_join(monthly_vitamin_d, by = c("Month" = "PaidDateMonth"))

# Remove rows with NA values
cleaned_data <- combined_data %>%
  drop_na()

library(ggplot2)

# Bar chart for cleaned data
ggplot(data = cleaned_data, aes(x = Month)) +
  geom_bar(aes(y = daylight, fill = "Daylight (hours)"), stat = "identity", position = "dodge", color = "blue") +
  geom_bar(aes(y = total_paid_quantity / 1e6, fill = "Prescriptions (millions)"), stat = "identity", position = "dodge", color = "red") +
  labs(title = "Daylight and Total Prescriptions by Month",
       x = "Month",
       y = "Values (Daylight in hours, Prescriptions in millions)",
       fill = "Legend") +
  scale_fill_manual(values = c("Daylight (hours)" = "blue", "Prescriptions (millions)" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 