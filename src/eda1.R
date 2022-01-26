# Which event types dominate task run times? 
# The data mining success criteria is the execution time (duration) of event types to determine which event types have the longest execution time. 

# Plot duration of eventName activities
ggplot(AC2, aes(x = eventName, y = duration)) + geom_boxplot() # Total Render has longest execution time because it accounts for the whole rendering process.

# Remove Total Render from results
AC3 <- AC2 %>%
  filter(eventName != "TotalRender")

# LABEL AND SAVE # Plot duration of eventName activities without total render
ggplot(AC3, aes(x = eventName, y = duration)) + geom_boxplot() # Render dominates run time with the longest execution times

# Remove Render from results
AC4 <- AC3 %>%
  filter(eventName != "Render")

# Plot duration of eventName activities without render to see other execution times better
ggplot(AC4, aes(x = eventName, y = duration)) + geom_boxplot() # Uploading has a significantly larger execution time than tiling and saving config.

# Remove Uploading from results
AC5 <- AC4 %>%
  filter(eventName != "Uploading")

# LABEL AND SAVE # Plot duration of eventName activities without uploading to see saving config and tiling execution times better
ggplot(AC5, aes(x = eventName, y = duration)) + geom_boxplot() 

# Which hosts have the longest Render time?
AC6 <- AC3 %>%
  filter(eventName == "Render") %>%
  arrange(desc(duration)) # Arrange so longest duration is at the top

R_long <- head(AC6, 50) %>%
  group_by(hostname) # Isolate the 50 longest durations

R_short <- tail(AC6, 50) %>%
  group_by(hostname) # Isolate the 50 shortest durations

# Which hosts have the longest TotalRender time?
AC7 <- AC2 %>%
  filter(eventName == "TotalRender") %>%
  arrange(desc(duration)) # Arrange so longest duration is at the top

TR <- AC7 %>%
  group_by(hostname) %>% 
  summarise(av_duration = mean(duration), sd_duration = sd(duration)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV = (sd_duration/av_duration)*100) # New column with coefficient of variation

# Exploration into the longest and shortest durations
TR_long <- head(AC7, 50) %>%
  group_by(hostname) # Isolate the 50 longest durations

TR_short <- tail(AC7, 50) %>%
  group_by(hostname) # Isolate the 50 shortest durations

# Are these the same hosts across Render and Total Render?
all(R_long$hostname %in% TR_long$hostname) # No, they're not

all(R_short$hostname %in% TR_short$hostname) # Yes, they are.

R_short1 <- tail(AC6, 500) %>%
  group_by(hostname) # There are 247 rendering events with no time. Why?

RT_short1 <- tail(AC7, 500) %>%
  group_by(hostname) # There are also 247 events with no total time. Why?

# Which are the hosts that have no duration for Total Render?
RT_NA <- tail(AC7, 247) %>%
  group_by(hostname) # Isolate the 247 events with no total time.

TR_NA %>%
  count() %>%
  print() # Identify how many times each host has no TR. There are 4 hosts.

# Do these hosts work at all?
all(TR_NA %in% TR_long) # These hosts do not appear in the list of those with the longest duration

TR_long1 <- head(AC7, 65546) %>%
  group_by(hostname) # Remove the 247 events from the overall list

all(TR_NA %in% TR_long1) # They do not seem to work at all. 

#AC7 <- AC2 %>%
  #filter(eventName == "TotalRender") %>%
  #pivot_wider(names_from = hostname, values_from = duration) %>%
  #pivot_wider(names_from = n, values from)


#rt_hosts <- AC2 %>%
  #summarise(hostname, count = mean(duration, na.rm = TRUE))

#AC6 <- AC2 %>%
  #filter(eventName == "TotalRender") %>%
  #pivot_wider(names_from = hostname, values_from = duration) 

