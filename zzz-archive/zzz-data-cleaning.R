library(here)
library(tidyverse)

source(here("R", "00_data-load.R"))
source(here("R", "01_generate-UltraSignup-results.R"))


### TODO Compare names across both
names_google <- googlesheets4::read_sheet("1SUOno-fveirOd39vehp_sCOCzNgP-hOoE88y0z_AodE", 
                          range = "A:B") %>%
  select(Last = `Last Name`, First = `First Name`)

names_new <- mileage_totals %>% select(Last, First)

google_names <- paste(names_google$First, names_google$Last)
new_names <- paste(names_new$First, names_new$Last)
google_only <- setdiff(google_names, new_names)
new_only <- setdiff(new_names, google_names)

# TODO These tables should be empty
frame_new_only <- data.frame(new_names = new_only) %>% arrange(new_names)
frame_google_only <- data.frame(new_names = google_only) %>% arrange(new_names)


### TODO Compare mileages
miles_google <- googlesheets4::read_sheet("1SUOno-fveirOd39vehp_sCOCzNgP-hOoE88y0z_AodE", 
                                          range = "A:F") %>%
  select(Last = `Last Name`, First = `First Name`, miles_google = `Total Distance (Miles)`)

# TODO This table should be empty
mileage_and_time_totals %>%
  select(Last, First, miles_new = `Total Miles`) %>%
  full_join(miles_google, by = c("Last", "First")) %>%
  filter(miles_new != miles_google) %>%
  View

### TODO Compare full spreadsheets
compare_mileage_google <- googlesheets4::read_sheet("1SUOno-fveirOd39vehp_sCOCzNgP-hOoE88y0z_AodE") %>%
  rename(Last = `Last Name`, First = `First Name`) %>%
  select(-City) %>%
  arrange(Last, First) %>%
  mutate(Gender = str_extract(Gender, "[MF]"))
  # arrange(Last, First, `Total Distance (Miles)`, `Total Distance (Miles)`)

compare_mileage_and_time_totals <- mileage_and_time_totals %>%
  select(names(compare_mileage_google)) %>%
  arrange(Last, First)

df1 <- compare_mileage_google
df2 <- compare_mileage_and_time_totals
identical(compare_mileage_google, compare_mileage_and_time_totals)
# TODO Check below if identical() returns FALSE
# Both below should be empty
mismatches_1 <- df1[df1$Gender != df2$Gender,]
mismatches_2 <- df2[df2$Gender != df1$Gender,]

## Checking for data quality - missing values?
merged_data %>%
  lapply(class)

merged_data_summary <- merged_data %>% 
  mutate_all(as.character) %>% 
  summarise(across(everything(),
                   .fns = list(NA_count = ~ sum(is.na(.), na.rm = TRUE),
                        all_caps_count = ~ sum(toupper(.) == . & is.na(as.numeric(.))),
                        question_mark_count = ~ sum(. == "?"),
                        zero_count = ~ sum(. == "0")),
                   .names = "{col}_summary_{fn}")) %>%
  pivot_longer(everything(),
               names_to = c("col_name", "summary_stat"),
               names_sep = "_summary_") %>% 
  arrange(col_name) %>%
  filter(value != 0 & !is.na(value))

# Any cities to correct?
# TODO This table should be empty
merged_data %>% filter(City != str_to_title(City) & City != "DeKalb") %>% 
  mutate(fixed = str_to_title(City), .before = everything()) %>% View

# Finally, deal with all the NAs
colSums(is.na(merged_data)) %>% .[which(. != 0)]

## NA Age
merged_data %>%
  filter(is.na(Age)) %>%
  arrange(-Year, Event, Place = `Overall Place`, Last, First, Age) %>%
  select(Year, Event, Last, First, Age) %>%
  View


## NA State
merged_data %>%
  filter(is.na(State)) %>%
  arrange(-Year, Event, Place = `Overall Place`, Last, First, Age) %>%
  select(Year, Event, Last, First, Age) %>%
  View

## NA City
merged_data %>%
  filter(is.na(City)) %>%
  arrange(-Year, Event, Place = `Overall Place`, Last, First, Age) %>%
  select(Year, Event, Last, First, Age) %>%
  View

