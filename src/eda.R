# Initial data exploration to develop understanding of data sets

# Count how many unique host names there are
hosts <- AC1 %>%
  group_by(hostname) %>% # Group by hostname
  summarise (n_distinct(hostname)) %>% # Summarise number of distinct hostnames
  count() %>%
  print() # There are 1024

# Are there more GPU serials then hosts?
ghosts <- GPU1 %>%
  group_by(hostname) %>% # Group by hostname
  summarise (n_distinct(hostname)) %>% # Summarise number of distinct hostnames
  count() %>%
  print() # There are 1024 hosts in the GPU data set

gserials <- GPU1 %>%
  group_by(gpuSerial) %>% # Group by gpuSerial
  summarise(n_distinct(gpuSerial)) %>% # Summarise number of distinct gpuSerials
  count() %>%
  print() # Each virtual machine has one GPU card, there are 1024 gpuserials

# Check whether the timestamps across AC1 and GPU1 are the same
all(AC1$timestamp %in% GPU1$timestamp) #They are not in their current format.

# Check whether the hostnames across AC1 and GPU1 are they same
all(AC1$hostname %in% GPU1$hostname) # They are.

# How many eventNames are there?
eT <- AC1 %>%
  group_by(eventName) %>% # Group by eventName
  summarise(n_distinct(eventName)) %>% # Summarise number of distinct eventNames
  count() %>%
  print()

# How many times does rendering happen per host?
TRh <- AC1 %>%
  filter(eventName == "TotalRender") %>% # Filter for TotalRender in the eventName
  count() %>%
  as_tibble() %>% # Create as a tibble
  arrange(desc(n), .by_group = TRUE) # Arrange by descending order, but maintain groups. Some hosts render more than others. Is this because of performance or tasking?

Rh <- AC1 %>%
  filter(eventName == "Render") %>% # Filter for Render in the eventName 
  count() # This appears to be the same as TotalRender

# How many taskIds are there?
ID <- AC1 %>%
  group_by(taskId) %>% # Group by taskIds
  summarise(n_distinct(taskId)) %>% # Summarise number of distinct taskIds
  count() %>% 
  print() # There are 65793 unique taskIds

# How many tasks does each host run?
cT <- AC1 %>%
  summarise(n_distinct(taskId)) %>% # Summarise number of distinct taskIds
  print(., n = 3, width = Inf) # 8b6a0eebc87b4cb2b0539e81075191b900000D runs 71 tasks (most), dcc19f48bb3445a28338db3a8f002e9c00000S runs 58 (least). Thi appears to match the number of times TotalRender happens per host. 

# How many jobIds are there?
j <- AC1 %>%
  group_by(jobId) %>% # Group by jobIds
  summarise(n_distinct(jobId)) %>% # Summarise by number of distinct jobIds
  count() %>%
  print() # 3

jXY <- XY1 %>% # Is this the same in the other data set?
  summarise(n_distinct(jobId)) %>% #Summarise number of distinct taskIDs
  count() %>%
  print() # Yes, 3

# How many tasks does each jobId have?
cTXY <- XY1 %>%
  summarise(n_distinct(taskId)) %>% # Summarise number of distinct taskIds
  print()

cTj <- AC1 %>%
  group_by(jobId) %>% # Group by jobId
  summarise(n_distinct(taskId)) %>% # Summarise number of distinct taskIds
  print() # Same as above

# How many tasks in XY1?
cT1 <- XY1 %>%
  group_by(taskId) %>% # Group by taskID
  summarise(n_distinct(taskId)) %>% # Summarise number of distinct taskID
  count() %>%
  print() # Same as AC2

# Are the task IDs the same across AC1 and XY1?
all(AC1$taskId %in% XY1$taskId) #Yes

# How many gpuUUIDs are there? Is this the same as number of taskIds?
UU <- GPU1 %>%
  group_by(gpuUUID) %>% # Group by gpuUUID
  summarise(n_distinct(gpuUUID)) %>% # Summarise number of distinct gpuUUID
  count() %>% # How many are there?
  print() # 1024 - there is one per host. 

# Smaller subset of data for Matt support
sub-set <- AC1 %>% head (1000) %>%
write.csv(sub-set, "subset.csv") # Copy for Matt support

# Plot same set of GPU temperature, memory and core utilisation as a line graph.
GPU1 %>% head(50) %>%
  ggplot(aes(x = timestamp)) + geom_line(aes(y = gpuTempC), colour = "#b2df8a", size = 1) + geom_line(aes(y = gpuUtilPerc), colour = "#1f78b4", size = 1) + geom_line(aes(y = gpuMemUtilPerc), colour = "#a6cee3", size = 1) + scale_colour_brewer(palette = "Paired") 

# Plot duration of events as a line graph
AC2 %>% head(50) %>%
  ggplot(aes(x = START)) + geom_line(aes(y = duration), colour = "#33a02c", size = 1)

# Plot duration of eventName activities as boxplot
ggplot(AC2, aes(x = eventName, y = duration)) + geom_boxplot() #Further investigation in EDA1


