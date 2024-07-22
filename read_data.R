library(tidyverse)
data <- readxl::read_excel("./data/daten_metaanalyse.xlsx")

colnames <- c(
  "pub", "pub_id", "authors", "year", "type", "language", "peer_review", "hypothesis",
  "n", "n_male", "n_female", "mean_age", "sd_age", "country", "class", "school_type", 
  "selective", "selective_type", "mean_int", "sd_int", 
  "interest_type", "interest_instrument", "interest_items", "interest_reliability",
  "interest_domain_specific", "interest_subject",
  "achievement_type", "achievement_instrument", "achievement_items", "achievement_reliability",
  "achievement_domain_specific", "achievement_subject",
  "n_measurements", "distance", "analysis_type",
  "result",
  "r",
  "database",
  "comment"
)
colnames(data) <- colnames

# Function to calculate average for given string
calc_average <- function(x) {
  # Replace hyphens with commas and split the string
  nums <- unlist(strsplit(gsub("-", ",", x), split = ","))
  # Convert to numeric and calculate the average
  mean(as.numeric(nums))
}

clean_data <- data %>% 
  slice(4:n()) %>%
  mutate(n = ifelse(is.na(n), n_male, n)) %>% 
  mutate(n = ifelse(is.na(n), n_female, n)) %>% 
  mutate(
    class = ifelse(class == "Oberstufe", 12, class)
  ) %>% 
  mutate(
    class = ifelse(class == "N/A", NA, class)
  ) %>% 
  mutate(across(c(r, n), parse_number)) %>% 
  # Correct correlation for pub_id 1339, hier wurden Noten falsch rum kodiert
  mutate(
    r = ifelse(pub_id == 1339 & achievement_type == 1, abs(r), r)
  ) 

clean_data$class = sapply(clean_data$class, calc_average)

removed_data <- clean_data %>% 
  filter(is.na(class) | is.na(achievement_type) | is.na(r) | is.na(n) | is.na(pub)) %>% 
  select(pub, interest_subject)

clean_data <- clean_data %>% 
  filter(!is.na(class), !is.na(achievement_type)) %>%  
  filter(!is.na(r), !is.na(n), !is.na(pub))

write.csv(clean_data, "data/clean_data.csv")
