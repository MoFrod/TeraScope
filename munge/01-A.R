# Create a data set of application checkpoints for initial investigation grouped by hostname then eventName and arranged by timestamp
AC1 <- as_tibble(application.checkpoints) %>%
  group_by(hostname) %>%
  arrange(timestamp, .by_group = TRUE)
  
# Remove the non-numeric characters from the timestamp column in AC1
AC1$timestamp <- gsub("T", "", AC1$timestamp) %>% 
  str_replace("Z", "") # Removed year, date and alphabetic characters as they're all the same across all 660400 rows.

# Make timestamp column date/time
AC1$timestamp <- as_datetime(AC1$timestamp, tz = "Europe/London")

# Move START and STOP to be columns per row to create a 'duration' variable showing times
AC2 <- AC1 %>%
  pivot_wider(names_from = eventType, # These are what we would like to have as the column names in the wider-but-shorter dataframe
  values_from = timestamp) # Fill those columsn with the timestamp value

# Create a data set of gpu for initial investigation grouped by hostname and arranged by timestamp
GPU1 <- as_tibble(gpu) %>%
  group_by(hostname) %>%
  arrange(timestamp, .by_group = TRUE)

# Remove the non-numeric characters from the timestamp column in GPU1
GPU1$timestamp <- gsub("T", "", GPU1$timestamp) %>% 
  str_replace("Z", "") # Removed year, date and alphabetic characters as they're all the same across all 660400 rows

# Make timestamp column date/time
GPU1$timestamp <- as_datetime(GPU1$timestamp, tz = "Europe/London")



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
