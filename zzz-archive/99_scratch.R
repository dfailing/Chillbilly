# TODO: Create a RunPhD plotting package.

# TODO: Plot winning time year over year against CHANGED course records
### Add a STAR with matching color when a new CR is set
### TODO: Use R plots gallery to find a good scatter plot

# TODO: Make this a feature of the all_results instead? A column that means "new CR"?
# TODO: Get a which.min of year in which each best time occurred
### Add as a column like Best_*_Year so you can annotate each point or do something like
### annotate the last time it changed with the year it changed?
# TODO: Note that there was no race in 2020. How to account for in plot?
# TODO: Use `Changed` column to talk about stale course records?
# TODO: Look at difference in mens vs womens record over time? 

# TODO: Facet by Event, color by Gender?
# TODO: Facet by Gender, color by Event?
course_records_over_time <- all_results %>%
  filter(Event %in% c("100 Mile", "100K", "50K", "Half Marathon")) %>%
  arrange(Event, Year, Time) %>%  # Sort by Year and Time
  group_by(Event, Year, Gender) %>%
  summarise(Best = min(as.numeric(Time), na.rm = TRUE)) %>%  # Convert hms to numeric before getting the minimum
  group_by(Event, Gender) %>%
  mutate(Best = cummin(Best)) %>% 
  ungroup() %>%
  pivot_wider(names_from = Gender, names_prefix = "Best_", values_from = Best) %>%
  arrange(Event, Year) %>%
  group_by(Event) %>%
  fill(Best_F) %>% # TODO: No Female 100K finishers in 2019
  ungroup() %>%
  rowwise() %>%
  # mutate(Best_All = min(Best_M, Best_F)) %>%
  mutate(across(starts_with("Best_"), hms::as_hms)) %>% # Convert numeric back to hms
  pivot_longer(cols = starts_with("Best_"), 
               names_to = "Gender", 
               names_prefix = "Best_",
               values_to = "Time") %>%
  arrange(Event, Gender, Year) %>%
  group_by(Event, Gender) %>%
  mutate(Changed = (Time != lag(Time)) | is.na(lag(Time)))

course_records_over_time %>%
  # filter(Changed) %>%
  # select(-Changed) %>%
  filter(Event == "100 Mile") %>%
  # filter(Event == "100K") %>%
  # filter(Event == "50K") %>% # Weird formatting on y axis Time. Why?
  # filter(Event == "Half Marathon") %>%
  # View()
  ggplot(aes(x = Year, y = Time, color = Gender)) +
    geom_point()
