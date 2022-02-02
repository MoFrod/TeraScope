# What is the relationship between GPU power draw and performance?

# What is the mean, standard deviation and coefficient of variation for all GPU metrics
Performance <- GPU1 %>%
  group_by(hostname) %>% 
  summarise(av_power = mean(powerDrawWatt), sd_power = sd(powerDrawWatt), av_temp = mean(gpuTempC), sd_temp = sd(gpuTempC), av_util = mean(gpuUtilPerc), sd_util = sd(gpuUtilPerc), av_mem = mean(gpuMemUtilPerc), sd_mem = sd(gpuMemUtilPerc)) %>% # Calculate mean and standard deviation of power for each host.
  mutate(CoV_p = (sd_power/av_power)*100, CoV_t = (sd_temp/av_temp)*100, CoV_u = (sd_util/av_util)*100, CoV_m = (sd_mem/av_mem)*100) # New column with coefficient of variation

# Quickplot power by temperature
Performance %>%
  ggplot(aes(x = av_temp, y = av_power)) + geom_point(position = "jitter") + geom_smooth() # Temperature seems to increase as you use more power

# Quickplot power by util
Performance %>%
  ggplot(aes(x = av_util, y = av_power)) + geom_point(position = "jitter") + geom_smooth() # Power usage generally increases as more of the gpu core is used - start to see divide into two groups

# Quickplot power by mem
Performance %>%
  ggplot(aes(x = av_mem, y = av_power)) + geom_point(position = "jitter") + geom_smooth() # Power use generally increases as more gpu memory is used - you see a clear divide into two groups here.

# Quickplot mem by temp
Performance %>%
  ggplot(aes(x = av_mem, y = av_temp)) + geom_point(position = "jitter") + geom_smooth() # Not entirely clear what this is telling me - two groups are clear

# Quickplot mem by util
Performance %>%
  ggplot(aes(x = av_mem, y = av_util)) + geom_point(position = "jitter") + geom_smooth() # Very clear trajectory of more memory corresponds to utilising  more gpu core

# Quickplot util by temp
Performance %>%
  ggplot(aes(x = av_util, y = av_temp)) + geom_point(position = "jitter") + geom_smooth() # Temperature seems broadly consisten across gpu core utilisation
