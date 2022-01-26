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

# 

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
