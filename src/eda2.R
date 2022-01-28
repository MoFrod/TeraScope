# What is the relationship between execution time and GPU power draw? 


# What the mean, standard deviation and coefficient of variation is for GPU power draw?
Power <- GPU1 %>%
  group_by(hostname) %>% 
  summarise(av_power = mean(powerDrawWatt), sd_power = sd(powerDrawWatt)) %>% # Calculate mean and standard deviation of duration for each host.
  mutate(CoV_p = (sd_power/av_power)*100) # New column with coefficient of variation

# Create single data set to compare execution time and power draw. 
Power_Duration <- left_join(TR, Power, by = "hostname")

# Quickplot execution time by power draw
Power_Duration %>%
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

# Create a single data set with power usage and duration for all events except total render
BEGIN <- as_datetime("2018-11-0807:41:27.242", tz = "Europe/London")
END <- as_datetime("2018-11-0807:42:27.242", tz = "Europe/London")

AC8 <- AC2 %>%
  filter(START >= BEGIN, START <= END) %>% # Select start times between BEGIN and END
  filter (eventName != "TotalRender") %>% # Filter out TotalRender
  mutate(timestamp = round_date(START, unit = "2 seconds")) # Create new timestamp column where start time is rounded to the nearest 15 seconds

GPU2 <- GPU1 %>%
  filter(timestamp >= BEGIN, timestamp <= END) %>% # Select timestamp that is between BEGIN and END
  mutate(timestamp = round_date(timestamp, unit = "2 seconds")) # Alter the timestamp column so that it is rounded to the nearest 15 seconds

Power_Duration1 <- full_join(AC8, GPU2, by = c("timestamp", "hostname")) %>% # Join AC8 and GPU2
  na.omit() %>% # Omit rows with NA
  group_by(hostname) %>% # Group by hostname
  arrange(timestamp, .by_group = TRUE) # Arrange by timestamp

# Quickplot timestamp by power draw for host that consumes the highest av_power
Power_Duration1 %>% 
  filter(hostname == "a77ef58b13ad4c01b769dac8409af3f800000D") %>%
  ggplot(aes(x = timestamp)) + geom_line(aes(y = powerDrawWatt), colour = "#1f78b4", size = 1)

# Quickplot execution time by power draw
Power_Duration1 %>% 
  ggplot(aes(x = powerDrawWatt)) + geom_line(aes(y = duration), colour = "#1f78b4", size = 1)

# Quickplot power draw by execution time
Power_Duration1 %>% 
  ggplot(aes(x = duration)) + geom_line(aes(y = powerDrawWatt), colour = "#1f78b4", size = 1)

# Quickplot event name by power draw (SAVE)
ggplot(Power_Duration1, aes(x = eventName, y = powerDrawWatt)) + geom_boxplot()

# Quickplot power draw by duration as boxplot
ggplot(Power_Duration1, aes(x = powerDrawWatt, y = duration)) + geom_boxplot()

# Quickplot 
ggplot(Power_Duration1, aes(x = powerDrawWatt)) + geom_histogram()
ggplot(Power_Duration1, aes(x = duration)) + geom_histogram()

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
