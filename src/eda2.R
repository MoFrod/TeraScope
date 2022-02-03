# What is the relationship between execution time and GPU power draw? 

# What is the mean, standard deviation and coefficient of variation for Total Render duration?
AC7 <- AC2 %>%
  filter(eventName == "TotalRender") # Filter to create a new data set only with TotalRender

TR1 <- AC7 %>%
  group_by(hostname) %>% # Group by the virtual machine host name
  summarise(av_duration = mean(duration), sd_duration = sd(duration)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV = (sd_duration/av_duration)*100) # New column with coefficient of variation

# What is the mean, standard deviation and coefficient of variation for GPU power draw?
Power <- GPU1 %>%
  group_by(hostname) %>% 
  summarise(av_power = mean(powerDrawWatt), sd_power = sd(powerDrawWatt)) %>% # Calculate mean and standard deviation of power for each host.
  mutate(CoV_p = (sd_power/av_power)*100) # New column with coefficient of variation

# Create single data set to compare execution time and power draw. 
Power_Duration <- left_join(TR1, Power, by = "hostname")

# SAVE AND LABEL # Quick plot execution time by power draw 
Power_Duration %>%
  ggplot(aes(x = av_duration, y = av_power)) + geom_point(position = "jitter") + geom_smooth()

# Quick plot average duration by coefficient of variation for duration
Power_Duration %>%
  ggplot(aes(x = av_duration, y = CoV)) + geom_point(position = "jitter") + geom_smooth()

# Quick plot average power draw by coefficienct of variation for power
Power_Duration %>%
  ggplot(aes(x = av_power, y = CoV_p)) + geom_point(position = "jitter") + geom_smooth()

# What is the mean, standard deviation and coefficient of variation for Total Render duration?
AC10 <- AC2 %>%
  filter(eventName == "TotalRender") # Filter to create a new data set only with TotalRender

R <- AC10 %>%
  group_by(hostname) %>% # Group by the virtual machine host name
  summarise(av_duration = mean(duration), sd_duration = sd(duration)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV = (sd_duration/av_duration)*100) # New column with coefficient of variation

# Create single data set to compare execution time and power draw. 
Power_Duration3 <- left_join(R, Power, by = "hostname")

# Quick plot render execution time by power draw 
Power_Duration3 %>%
  ggplot(aes(x = av_duration, y = av_power)) + geom_point(position = "jitter") # Looks very similar to Total Render

# Quick plot render average duration by coefficient of variation for duration
Power_Duration3 %>%
  ggplot(aes(x = av_duration, y = CoV)) + geom_point(position = "jitter") + geom_smooth() # Looks very similar to Total Render

# Quick plot render average power draw by coefficienct of variation for power
Power_Duration %>%
  ggplot(aes(x = av_power, y = CoV_p)) + geom_point(position = "jitter") + geom_smooth() # Looks very similar to Total Render

# Quickplot execution time by power draw
Power_Duration3 %>%
  ggplot(aes(x = av_power)) + geom_line(aes(y = av_duration), colour = "#1f78b4", size = 1)

# Quickplot execution time by power draw with the 100 longest average durations
Power_Duration %>% 
  arrange(desc(av_duration)) %>%
  head(100) %>%
  ggplot(aes(x = av_power)) + geom_line(aes(y = av_duration), colour = "#1f78b4", size = 1)

# Quickplot execution time by power draw with the 100 lowest average durations
Power_Duration %>% 
  arrange(desc(av_duration)) %>%
  tail(100) %>%
  ggplot(aes(x = av_power)) + geom_line(aes(y = av_duration), colour = "#1f78b4", size = 1)

# Quickplot execution time by power draw for the 100 highest average gpu power draws
Power_Duration %>%
  arrange(desc(av_power)) %>%
  head(100) %>%
  ggplot(aes(x = av_power)) + geom_line(aes(y = av_duration), colour = "#1f78b4", size = 1)

#Quickplot execution time by power draw for the 100 lowest average gpu power draws
Power_Duration %>%
  arrange(desc(av_power)) %>%
  tail(100) %>%
  ggplot(aes(x = av_power)) + geom_line(aes(y = av_duration), colour = "#1f78b4", size = 1)

# Boxplot execution time by power draw
ggplot(Power_Duration, aes(x = av_power, y = av_duration)) + geom_boxplot()

# Create a single data set with power usage and duration for total render
BEGIN <- as_datetime("2018-11-0807:41:27.242", tz = "Europe/London")
END <- as_datetime("2018-11-0807:42:27.242", tz = "Europe/London")

AC8 <- AC2 %>%
  filter(START >= BEGIN, START <= END) %>% # Select start times between BEGIN and END
  filter (eventName == "TotalRender") %>% # Filter TotalRender
  mutate(timestamp = round_date(START, unit = "2 seconds")) # Create new timestamp column where start time is rounded to the nearest 15 seconds

GPU2 <- GPU1 %>%
  filter(timestamp >= BEGIN, timestamp <= END) %>% # Select timestamp that is between BEGIN and END
  mutate(timestamp = round_date(timestamp, unit = "2 seconds")) # Alter the timestamp column so that it is rounded to the nearest 15 seconds

Power_Duration1 <- full_join(AC8, GPU2, by = c("timestamp", "hostname")) %>% # Join AC8 and GPU2
  na.omit() %>% # Omit rows with NA
  group_by(hostname) %>% # Group by hostname
  arrange(timestamp, .by_group = TRUE) # Arrange by timestamp

# SAVE AND LABEL # Plot hostname by power draw and duration of total render
Power_Duration1 %>%
  ggplot(aes(x = duration, y = powerDrawWatt)) + geom_point(position = "jitter")

# Exploration into the longest and shortest total render durations
TR_long1 <- Power_Duration1 %>%
  arrange(desc(duration)) %>%
  head(., 50) %>%
  group_by(hostname) # Isolate the 50 longest durations

TR_short1 <- Power_Duration1 %>%
  arrange(desc(duration)) %>%
  tail(., 50) %>%
  group_by(hostname) # Isolate the 50 shortest durations

# Quick plot duration and power for longest total render durations
TR_long1 %>%
  ggplot(aes(x = duration, y = powerDrawWatt)) + geom_point(position = "jitter")

# Quick plot duration and power for shortest total render durations
TR_short1 %>%
  ggplot(aes(x = duration, y = powerDrawWatt)) + geom_point(position = "jitter")

# Do all the taskIds in Power_Duration1 occur in XY1?
all(Power_Duration1$taskId %in% XY1$taskId)

# Join together Power Duration 1 and XY1
Total <-  left_join(Power_Duration1, XY1, by = "taskId")

# SAVE AND LABEL # Plot hostname by power draw and duration of total render with difference shapes for taskId
Total %>%
  ggplot(aes(x = duration, y = powerDrawWatt, color = jobId.x)) + geom_point(position = "jitter")

# Create a single data set with power usage and duration for every event other than total render
AC9 <- AC2 %>%
  filter(START >= BEGIN, START <= END) %>% # Select start times between BEGIN and END
  filter (eventName != "TotalRender") %>% # Filter out TotalRender
  mutate(timestamp = round_date(START, unit = "2 seconds")) # Create new timestamp column where start time is rounded to the nearest 15 seconds

Power_Duration2 <- full_join(AC9, GPU2, by = c("timestamp", "hostname")) %>% # Join AC8 and GPU2
  na.omit() %>% # Omit rows with NA
  group_by(hostname) %>% # Group by hostname
  arrange(timestamp, .by_group = TRUE) # Arrange by timestamp

# Create average, standard deviation and coefficient of variation for power draw and duration
Power_Duration2 <- Power_Duration2 %>%
  group_by(hostname) %>% 
  mutate(av_duration = mean(duration), sd_duration = sd(duration)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV = (sd_duration/av_duration)*100) %>% # New column with coefficient of variation
  mutate(av_power = mean(powerDrawWatt), sd_power = sd(powerDrawWatt)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV_p = (sd_power/av_power)*100) # New column with coefficient of variation

# Plot hostname by power draw and duration of total render
Power_Duration2 %>%
  ggplot(aes(x = av_duration, y = av_power, shape = eventName)) + geom_point(position = "jitter")

# Create a column for coefficient of variation in R_Power_Duration
R_Power_Duration <- R_Power_Duration %>%
  group_by(hostname) %>% #group by host
  mutate(av_duration = mean(duration), sd_duration = sd(duration))

summarise(R_Power_Duration)

# Quickplot timestamp by power draw for host that consumes the highest av_power
Power_Duration2 %>% 
  filter(hostname == "a77ef58b13ad4c01b769dac8409af3f800000D") %>%
  ggplot(aes(x = timestamp)) + geom_line(aes(y = powerDrawWatt), colour = "#1f78b4", size = 1)

# Quickplot execution time by power draw
Power_Duration2 %>% 
  ggplot(aes(x = powerDrawWatt)) + geom_line(aes(y = duration), colour = "#1f78b4", size = 1)

# Quickplot power draw by execution time
Power_Duration2 %>% 
  ggplot(aes(x = duration)) + geom_line(aes(y = powerDrawWatt), colour = "#1f78b4", size = 1)

# LABEL AND SAVE #Quickplot event name by power draw (SAVE)
ggplot(Power_Duration2, aes(x = eventName, y = powerDrawWatt)) + geom_boxplot()






# Create GPU2 sop timestamp isn't as datetime like AC1
#GPU2 <-  as_tibble(gpu) %>%
  #group_by(hostname) %>%
  #arrange(timestamp, .by_group = TRUE)

# Remove the non-numeric characters from the timestamp column in GPU
#GPU2$timestamp <- gsub("T", "", GPU2$timestamp) %>% 
  #str_replace("Z", "") # Removed alphabetic characters as they're all the same across all 660400 rows

# create time buckets to create comparable data across AC1 and GPU2
#DF1 <- head(AC1, 500) %>% arrange(timestamp, .by_group = TRUE) # Create a sample and make sure it's arranged by timestamp
#BEGIN <- as_datetime("2018-11-0807:41:27.242", tz = "Europe/London")
#END <- as_datetime("2018-11-0808:30:16.345", tz = "Europe/London")
#BUCKET <- 100000 #Needs to be rounded up in order to join the spreadsheets
#for (T in DF1$timestamp) { 
  #if(T < BEGIN + BUCKET) {
   # T = BEGIN  
 # } else{BEGIN <- BEGIN + BUCKET} 
 # if(BEGIN > END){
   # break
 # }
#} 

#DF2 <- head(GPU2, 500) %>% arrange(timestamp, .by_group = TRUE) # Create a sample and make sure it's arranged by timestamp
#for (T in DF2$timestamp) {
  #if(T < BEGIN + BUCKET) {
    #T = BEGIN  
  #} else{BEGIN <- BEGIN + BUCKET} 
  #if(BEGIN > END){
   # break
  #}
#}

# Join the data sets together with the common timestamps
#Total_Perf <- left_join(DF1, DF2, by = "timestamp") # Doesn't work yet because timestamps aren't right.
