################################################################################
# TODO To submit results to UltraSignup, these fields are required.
## Follow instructions at: https://ultrasignup.com/events/results_request.aspx
################################################################################

# Load required packages
library(here)
library(tidyverse)

# Create subdirectories in case they're missing
if (!file.exists(here("data-export"))) dir.create(here("data-export"))

# Generate a list of filenames
dir_raw <- here("data-raw")
dir_export <- here("data-export")

csv_filenames <- list.files(dir_raw, pattern = ".csv$")

# TODO For fixed-distance races
# columns_required <- c("place", "time", "first", "last", "age", "gender")

# TODO For fixed-time events
columns_required <- c("place", "distance", "first", "last", "age", "gender")

# Optional fields
columns_optional <- c("city", "state", "dob", "bib", "status")
# TODO Add statuses to the raw data if you want to submit status to UltraSignup
# status_values <- c(1 = "Finished", 2 = "DNF", 3 = "DNS", 
#                    4 = "Unofficial Finish", 
#                    5 = "Disqualified", 6 = "Less than 10 Finishers")

for (filename in csv_filenames) {
  filename_UltraSignup <- paste0("UltraSignup-", filename)
  
  data <- read_csv(here(dir_raw, filename))
  
  data %>% 
    rename(Place = "Overall Place") %>%
    # TODO Comment `Time` out for fixed-time events
    # rename(Time = `Finish Time`) %>%
    arrange(Place) %>%
    mutate(Gender = case_when(str_starts(Gender, "M") ~ "Male",
                              str_starts(Gender, "F") ~ "Female")) %>%
    select(any_of(str_to_title(c(columns_required, columns_optional)))) %>%
    rename_with(str_to_lower, everything()) %>%
    write_csv(here("data-export", filename_UltraSignup))
  
}
