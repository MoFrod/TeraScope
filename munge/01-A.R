# Create a data set of application checkpoints for initial investigation grouped by hostname then eventName and arranged by timestamp
AC1 <- as_tibble(application.checkpoints) %>%
  group_by(hostname) %>%
  arrange(timestamp, .by_group = TRUE)
  
# Remove the non-numeric characters from the timestamp column in AC1
AC1$timestamp <- gsub("T", "", AC1$timestamp) %>% 
  str_replace("Z", "") # Removed alphabetic characters as they're all the same across all 660400 rows

# Cache AC1
cache("AC1")

# Move START and STOP to be columns per row to create a 'duration' variable showing times
AC2 <- AC1 %>%
  pivot_wider(names_from = eventType, # These are what we would like to have as the column names in the wider-but-shorter dataframe
  values_from = timestamp) # Fill those columns with the timestamp value

# Make START/STOP columns date/time
AC2 <- AC2 %>%
  mutate(START = parse_date_time(START, orders = c("ymd HMS")))
AC2 <- AC2 %>%
  mutate(STOP = parse_date_time(STOP, orders = c("ymd HMS"))) 

# Create a duration column for AC2 to calculate run time
AC2 <- AC2 %>%
  mutate(duration = STOP - START)

# Round duration in AC2
AC2$duration <- AC2$duration %>%
  as.numeric() %>%
  round(., digits = 2)

# Cache AC2
cache("AC2")

# Create a data set of gpu for initial investigation grouped by hostname and arranged by timestamp
GPU1 <- as_tibble(gpu) %>%
  group_by(hostname) %>%
  arrange(timestamp, .by_group = TRUE)

# Remove the non-numeric characters from the timestamp column in GPU1
GPU1$timestamp <- gsub("T", "", GPU1$timestamp) %>% 
  str_replace("Z", "") # Removed alphabetic characters as they're all the same across all 660400 rows

# Convert timestamp column to date/time in local time zone
GPU1$timestamp <- as_datetime(GPU1$timestamp, tz = "Europe/London")

# Cache GPU1
cache("GPU1")

# Create a data set of tasks.x.y. for intial investigation
XY1 <- as_tibble(task.x.y) %>%
  group_by(jobId)

# Cache XY1
cache("XY1")
