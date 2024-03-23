# Load required packages
library(here)
library(tidyverse)


# Create subdirectories in case they're missing
if (!file.exists(here("data"))) dir.create(here("data"))
if (!file.exists(here("data-export"))) dir.create(here("data-export"))
if (!file.exists(here("data-raw"))) dir.create(here("data-raw"))


# Generate a list of filenames
dir_raw <- here("data-raw")
dir_data <- here("data")
dir_export <- here("data-export")

csv_filenames <- list.files(dir_raw, pattern = ".csv$")
variable_names <- c()

for (filename in csv_filenames) {
  variable_name <- filename %>% 
    str_replace(".csv", "") %>% 
    str_replace_all("-", "_") %>%
    paste0("results_", .)
  
  variable_names <- c(variable_names, variable_name)
  
  data <- read_csv(
    here(dir_raw, filename),
    col_types = list(`Year` = col_integer(),
                     `Overall Place` = col_integer(),
                     `Age` = col_integer()
    )
  )
  
  data %>%
    select(-any_of(c("Note", "UltraSignup"))) %>%
    arrange(`Overall Place`, Last, First) %>%
    group_by(Gender) %>%
    mutate(`Division Place` = dense_rank(`Overall Place`), .after = `Overall Place`) %>%
    ungroup() %>%
    mutate(State = str_to_upper(State)) %>%
    assign(variable_name, ., envir = parent.frame())
}

merged_data <- lapply(variable_names, get) %>%
  bind_rows()

################################################################################
# TODO Create UUID for existing participants, then assign in registry
# TODO Update this function so that it will allow future years to assign UUID 
## in raw data, so that it could be used to update based on either having it already
## OR getting it assigned in the manner the first year was assigned.
## Test this by doing 2022 manually and 2015-2021 to initialize it
## TEST in a sandbox
## COULD also use Year -> (Last, First, Distance) tuples to assign UUID? 
### The idea being the same person couldn't do multiple distances each year
## COULD also use Year < 2022 to assign alphabetically, then Year >= 2022 to 
### assign based on the uuid_registry in combination with some other method.
### Would need to keep track of the initial end year (e.g.: year_initial = 2021)
################################################################################
# TODO Use this after initial assignments have been made?
# uuid_registry <- read_csv(here("data", "uuid_registry.csv"))

# Helper function to assign UUIDs
assign_uuids <- function(merged_data, uuid_registry) {
  # Adding an original_order column to restore the order at the end
  merged_data <- merged_data %>%
    mutate(original_order = row_number()) %>%
    arrange(Year, Last, First, Event)
  
  max_uuid <- if (nrow(uuid_registry) == 0 || !("UUID" %in% names(uuid_registry))) {
    0
  } else {
    max(uuid_registry$UUID, na.rm = TRUE)
  }
  
  for (i in 1:nrow(merged_data)) {
    current_row <- merged_data[i, ]
    existing_entry <- uuid_registry %>%
      filter(Last == current_row$Last, First == current_row$First)
    
    if (nrow(existing_entry) > 0) {
      merged_data[i, "UUID"] <- existing_entry$UUID[1]
    } else {
      new_uuid <- max_uuid + 1
      max_uuid <- new_uuid
      merged_data[i, "UUID"] <- new_uuid
      uuid_registry <- uuid_registry %>%
        add_row(UUID = new_uuid,
                Last = current_row$Last,
                First = current_row$First,
                Year_Min = current_row$Year,
                Event = current_row$Distance)
    }
  }
  
  # Restoring the original order and dropping the original_order column
  merged_data <- merged_data %>%
    arrange(original_order) %>%
    select(-original_order)
  
  list(merged_data = merged_data, uuid_registry = uuid_registry)
}

# Initialize UUID registry
if (file.exists(here("data", "uuid_registry.csv"))) {
  uuid_registry <- read_csv(here("data", "uuid_registry.csv"))
} else {
  uuid_registry <- tibble(UUID = integer(), 
                          Last = character(), 
                          First = character(), 
                          Year_Min = integer(), 
                          Event = numeric())
}

result <- assign_uuids(merged_data, uuid_registry)
merged_data <- result$merged_data
uuid_registry <- result$uuid_registry

# TODO pull this to the end once satisfied with UUID registry
# Saving the uuid_registry
write_csv(uuid_registry, here("data", "uuid_registry.csv"))


################################################################################

# Course Records
course_records <- merged_data %>%
  mutate(Gender = case_when(Gender == "M" ~ "Male", 
                            Gender == "F" ~ "Female", 
                            TRUE ~ "")) %>%
  group_by(Event, Gender) %>%
  arrange(-Distance) %>%
  slice_max(Distance, n = 3) %>%
  mutate(Rank = dense_rank(-Distance)) %>%
  ungroup() %>%
  arrange(-Time, desc(Gender), -Distance) %>%
  select(Event, Gender, Rank, First, Last, Age, Distance, Year)

# Mileage and Time Totals
### TODO Use the mileage totals to spot any data quality issues
### TODO Use the time totals to spot any data quality issues
mileage_and_time_totals <- merged_data %>%
  select(UUID, First, Last, Gender, City, State, Year, Distance, Time) %>% 
  arrange(Year, UUID) %>% 
  group_by(UUID) %>%
  # Fill down first to assume no change until we are informed of a change
  fill(First, Last, Gender, City, State, .direction = "down") %>%
  # Fill up after to handle if first is NA (Unnecessary?)
  fill(First, Last, Gender, City, State, .direction = "up") %>%
  # Overwrite First, Last, Gender, City, State with values from most recent race result
  arrange(-Year, UUID) %>% 
  mutate(across(c(First, Last, Gender, City, State), ~ .[which.max(Year)])) %>%
  mutate(`Finishes` = n(),
         `Total Distance (Miles)` = sum(Distance),
         `Total Time (Hours)` = sum(Time), .after = State) %>%
  pivot_longer(cols = c("Distance", "Time"), names_to = "Attribute", values_to = "Value") %>%
  mutate(Year = paste0(Year, " (", if_else(Attribute == "Distance", "D", "T"), ")")) %>%
  select(-Attribute) %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  ungroup() %>%
  mutate(`Rank (Miles)` = dense_rank(-`Total Distance (Miles)`), 
         `Rank (Time)` = dense_rank(-`Total Time (Hours)`), 
         .before = everything()) %>%
  arrange(`Rank (Miles)`, Last, First) %>%
  select(-UUID)

# All Results
# TODO finish this stuff
all_results <- merged_data %>%
  # TODO determine what I'm NOT selecting below and select it.
  ## OTHERWISE use rename() appropriately, or both select() and rename()
  select(Year, Event, Place = `Overall Place`, First, Last, City, State, 
         Age, Gender, DP = `Division Place`, Distance, Time) %>% 
  arrange(-Year, -Time, Place) %>%
  select(-Time)
 
# Export Data
write_csv(course_records, here("data", "course_records.csv"))
write_csv(mileage_and_time_totals, here("data", "mileage_and_time_totals.csv"))
write_csv(all_results, here("data", "all_results.csv"))
# TODO Export the UUID registry too
