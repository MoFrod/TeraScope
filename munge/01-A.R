# Create a data set of application checkpoints for initial investigation
AC1 <- as_tibble(application.checkpoints)

# Remove the non-numeric characters from the timestamp column in AC1
AC1$timestamp <- gsub("T", "", AC1$timestamp) %>%
  str_replace("Z", "")

# Convert timestamp values to date/time
as_datetime(AC1$timestamp)


# Create a new column in AC1 called "runtime"

