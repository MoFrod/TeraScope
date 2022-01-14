# Create a data set of application checkpoints for initial investigation grouped by hostname and arranged by timestamp
AC1 <- as_tibble(application.checkpoints) %>%
  group_by(hostname, eventName) %>%
  arrange(timestamp, .by_group = TRUE)
  
# Remove the non-numeric characters from the timestamp column in AC1
AC1$timestamp <- gsub("2018-11-08T", "", AC1$timestamp) %>% 
  str_replace("Z", "") # Removed year, date and alphabetic characters as they're all the same across all 660400 rows.

# Create separate columns for start and stop
#AC2 <- filter(AC1$eventType, "START")

# AC2 <- pivot_wider(AC1, )

# Convert timestamp values to date/time
# as_datetime(AC1$timestamp)


# Create a new column in AC1 called "runtime"

# Make data sets comparable
# Join between enrolments and archetype_survey_response on learner_id
# More info on joins: https://dplyr.tidyverse.org/reference/mutate-joins.html 
#joint_full <- left_join(FL1, full_archetypes, by = "learner_id", archetype)

#Filter by total render

# calculate duration of render

# Look at performance of gpus? does this need to happen on a task basis?
