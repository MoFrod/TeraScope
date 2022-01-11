# Create a data set of application checkpoints for initial investigation
AC1 <- as_tibble(application.checkpoints)

# Remove the non-numeric characters from the timestamp column in AC1
AC1$timestamp <- gsub("T", "", AC1$timestamp) %>%
  str_replace("Z", "")

# Convert timestamp values to date/time
as_datetime(AC1$timestamp)


# Create a new column in AC1 called "runtime"

# Make data sets comparable
# Join between enrolments and archetype_survey_response on learner_id
# More info on joins: https://dplyr.tidyverse.org/reference/mutate-joins.html 
#joint_full <- left_join(FL1, full_archetypes, by = "learner_id", archetype)

#Filter by total render

# calculate duration of render

# Look at performance of gpus? does this need to happen on a task basis?
