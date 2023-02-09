library(tidyverse)
library(rvest)
library(styler)
library(janitor)

setwd("~/DATA II/final-project-meredith-haley-joshua")

general_directory <- "C:\\Users\\joshu\\OneDrive\\Documents\\DATA II\\final-project-meredith-haley-joshua"

# set your path to a "data" folder

path_out <- "C:\\Users\\joshu\\OneDrive\\Documents\\DATA II\\final-project-meredith-haley-joshua\\data"


url_vector <- c(
  "https://www.eia.gov/electricity/data/eia860/xls/eia8602021.zip",
  "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_XLSX.zip"
)

retrieve_data <- function(url) {
  if (file.exists("electr.zip") |
    file.exists("ata.or.zip")) {
    stop("The file or files have already been downloaded. Check your files and please remove the specific link from your url vector")
  }

  for (each_url in url) {
    name <- paste(substring(each_url, 21, 26), ".zip", sep = "")

    files <- download.file(each_url, file.path(path_out, name))

    file_list <- c(name)

    # set the working directory as your data folder  otherwise the unzip operation will not work
    setwd(path_out)

    unzip(file_list)
  }
}
retrieve_data(url_vector)

# AQI data

aqi_url <- c("https://aqs.epa.gov/aqsweb/airdata/annual_aqi_by_county_")

aqi_year <- 1980:2022

retrieve_aqi <- function(years, url) {
  if (file.exists("aqi_summarydata.csv")) {
    stop("Hey silly, you already downloaded these files!")
  }

  for (each_year in years) {
    year <- paste(url, each_year, ".zip", sep = "")

    link_name <- substring(url, 43)

    name <- paste(link_name, each_year, ".zip", sep = "")

    files <- download.file(year, file.path(path_out, name))

    file_list <- c(name)

    setwd(path_out)

    unzip(file_list)
  }
}

retrieve_aqi(aqi_year, aqi_url)

aqi_data <- list.files(path = path_out, pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

write.csv(aqi_data, file.path(path_out, "aqi_summarydata.csv"), row.names = FALSE)

# load the data

coal_illness <- readxl::read_xlsx(file.path(
  path_out,
  "IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.xlsx"
),
sheet = 6, skip = 1
)

asthma <- readxl::read_xlsx(file.path(
  path_out,
  "IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.xlsx"
),
sheet = 8, skip = 1
)

# need this variable for data cleaning to distinguish type of illness

coal_illness$illness <- "pneumoconiosis"

asthma$illness <- "asthma"

# data sets share all observations and variables so just bind rows to only clean one data set

resp_cases <- bind_rows(coal_illness, asthma)

# remove footers

resp_cases <- resp_cases %>% filter(!is.na(FIPS))

resp_clean <- separate(resp_cases, col = "Location", sep = ",", remove = TRUE, into = c("County", "State"))

resp_clean$County <- gsub("County", "", resp_clean$County)

# drop the state averages, so filter where county is NA

resp_clean <- resp_clean %>%
  filter(State != "NA")

resp_clean <- clean_names(resp_clean)

# we need to reshape mortality into a "long" format

resp_clean <-
  resp_clean %>%
  pivot_longer(
    cols = 4:12,
    names_to = "year", values_to = "mortality_rate"
  )

resp_clean <- resp_clean %>%
  filter(year != "percent_change_in_mortality_rate_1980_2014")

# still need to extract the year from that character string

resp_clean$year <- gsub("mortality_rate_", "", resp_clean$year)

# the mortality column looks like it includes confidence intervals
# let's turn those into an upper and lower bound column

resp_clean[c("mortality_rate", "upper_bound", "lower_bound")] <- str_split_fixed(resp_clean$mortality_rate, " ", 3)

# need to use brackets because parentheses are special characters

resp_clean$upper_bound <- gsub("[(]", "", resp_clean$upper_bound)

resp_clean$upper_bound <- gsub(",", "", resp_clean$upper_bound)

resp_clean$lower_bound <- gsub("[)]", "", resp_clean$lower_bound)

write.csv(resp_clean, file.path(path_out, "resp_illness_tidy.csv"), row.names = FALSE)

# plant data

# note that this data includes all "operable" units

plant_data <- readxl::read_xlsx(file.path(path_out, "3_1_Generator_Y2021.xlsx"), skip = 1)

# to drop that last row of entirely NA's in the dataset (looks like a blank footer)

plant_data <- plant_data %>% filter(!is.na(State))

plants_reduced <- plant_data %>%
  select(
    "Utility ID",
    "Utility Name",
    "Plant Name",
    "State",
    "County",
    "Technology",
    "Ownership",
    "Operating Year",
    "Planned Retirement Year",
    "Energy Source 1",
    "Carbon Capture Technology?"
  )

# source: https://stackoverflow.com/questions/62247385/fifty-five-states-abbreviations-to-full-names-in-r

states <- inner_join(
  data.frame(
    State = state.name,
    long = state.center$x,
    lat = state.center$y,
    stringsAsFactors = FALSE
  ),
  data.frame(State = state.name, abbrev = state.abb)
)

plants_reduced <- plants_reduced %>%
  rename("abbrev" = "State")

plants_reduced <- plants_reduced %>%
  full_join(states, by = "abbrev")

technology <- clean_names(plants_reduced)

technology_tidy <- technology %>%
  group_by(state, county, abbrev, technology, operating_year) %>%
  count() %>%
  pivot_wider(names_from = "technology", values_from = "n", values_fill = 0) %>%
  arrange(operating_year)

# need intervals that match the health data

technology_tidy <- technology_tidy %>%
  filter(operating_year <= 2015) %>%
  mutate(interval_year = case_when(
    operating_year < 1985 ~ 1980,
    operating_year %in% 1985:1989 ~ 1985,
    operating_year %in% 1990:1994 ~ 1990,
    operating_year %in% 1995:1999 ~ 1995,
    operating_year %in% 2000:2004 ~ 2000,
    operating_year %in% 2005:2009 ~ 2005,
    operating_year %in% 2010:2013 ~ 2010,
    operating_year %in% 2014 ~ 2014, TRUE ~ operating_year
  ))

technology_tidy <- technology_tidy %>%
  filter(interval_year <= 2014)

technology_tidy <- technology_tidy %>%
  pivot_longer(cols = 5:31, names_to = "technology", values_to = "n") %>%
  group_by(state, county, interval_year, technology) %>%
  summarise(total_plants = sum(n), .groups = "keep")

technology_tidy <- technology_tidy %>%
  group_by(state, county, technology) %>%
  arrange(interval_year) %>%
  mutate(cumulative_plants = cumsum(total_plants))

technology_tidy <- technology_tidy %>%
  select(-c(total_plants))

technology_tidy <- technology_tidy %>%
  group_by(state, county, interval_year) %>%
  pivot_wider(names_from = technology, values_from = cumulative_plants)

write.csv(technology_tidy, file.path(path_out, "technology_tidy.csv"), row.names = FALSE)

# join plant data with illness data

resp_clean <- resp_clean %>% rename(interval_year = "year")

technology_tidy$interval_year <- as.character(technology_tidy$interval_year)

resp_clean$interval_year <- as.character(resp_clean$interval_year)

resp_clean <- resp_clean %>%
  mutate_if(is.character, str_trim)

technology_tidy <- technology_tidy %>%
  mutate_if(is.character, str_trim)

tech_health_merged <- resp_clean %>%
  left_join(technology_tidy, by = c("state", "county", "interval_year"))

# now fill in the plant technology values

tech_vector <- tech_health_merged %>% colnames(10:37)

tech_health_merged <- tech_health_merged %>%
  ungroup() %>%
  group_by(state, county) %>%
  fill(tech_vector, .direction = "downup")

# any remaining presence of an NA in the column is essentially a county who never had a power generator built

tech_health_merged <- tech_health_merged %>%
  mutate_at(c(9:35), ~ replace(., is.na(.), 0))

write.csv(tech_health_merged, file.path(path_out, "tech_health_merged.csv"), row.names = FALSE)

# county and tech data

county_air_data <- read_csv(file.path(path_out, "aqi_summarydata.csv"))

county_air_data <- clean_names(county_air_data)

# use anti-join to check where we don't have matches

nonmatch <- county_air_data %>%
  anti_join(tech_health_merged, by = "state", "county") %>%
  group_by(state) %>%
  count() %>%
  # will create a vector of non-matches to use later
  pull(state)

print(nonmatch)

county_air_data <- county_air_data %>% filter(state != nonmatch)

# we need to average the air quality across five year intervals

energy_data <- county_air_data %>%
  filter(year <= 2014) %>%
  mutate(interval_year = case_when(
    year %in% 1980:1984 ~ 1980,
    year %in% 1985:1989 ~ 1985,
    year %in% 1990:1994 ~ 1990,
    year %in% 1995:1999 ~ 1995,
    year %in% 2000:2004 ~ 2000,
    year %in% 2005:2009 ~ 2005,
    year %in% 2010:2013 ~ 2010,
    year %in% 2014 ~ 2014, TRUE ~ year
  ))

# we can see that air quality measurements are missing for random years in some counties

energy_data <- energy_data %>%
  group_by(state, county, interval_year) %>%
  summarise(across(everything(), list(mean)))

energy_data <- energy_data %>% select(-year_1)

energy_data$interval_year <- as.character(energy_data$interval_year)

# join the data

energy_data_tidy <- tech_health_merged %>%
  left_join(energy_data, by = c(
    "state",
    "county",
    "interval_year"
  ))

energy_data_tidy <- clean_names(energy_data_tidy)

old_names <- colnames(energy_data_tidy[36:50])

print(old_names)

new_names <- c(
  "days_with_aqi_avg",
  "good_days_avg",
  "moderate_days_avg",
  "unhealthy_for_sensitive_groups_days_avg",
  "unhealthy_days_avg",
  "very_unhealthy_days_avg",
  "hazardous_days_avg",
  "max_aqi_avg",
  "x90th_percentile_aqi_avg",
  "median_aqi_avg",
  "days_co_avg",
  "days_no2_avg",
  "days_ozone_avg",
  "days_pm2_5_avg",
  "days_pm10_avg"
)

# source: https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names

energy_data_tidy <- energy_data_tidy %>%
  rename_at(vars(old_names), ~new_names)

# do not fill in missing values because it is genuine missing data

write.csv(energy_data_tidy, file.path(path_out, "energy_data_tidy.csv"), row.names = FALSE)

