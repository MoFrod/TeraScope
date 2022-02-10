#Can we identify particular GPU cards (based on their serial numbers) whose performance differs to other cards? 

# Filter out gpu cards that have the longest duration
Power_Duration3 <- Power_Duration2 %>%
  filter(eventName == "TotalRender") %>%
  group_by(gpuSerial) %>%
  mutate(med_gpu_t = median(duration), av_gpu_t = mean(duration)) # Median and average look very close 

S_gpu <- Power_Duration3 %>% # Create two groups to match the split between hostnames
  filter(med_gpu_t >= 34) %>%
  arrange(desc(med_gpu_t)) %>%
  head(10)

# Plot S-gpu as barplot
S_gpu %>%
  ggplot(aes(x = av_gpu_t, y =powerDrawWatt, fill = gpuSerial)) + geom_boxplot()

# Writing data for Matt support
write.csv(TR1, "TR1.csv") # Copy for Matt support

write.csv(Performance, "Performance.csv") # Copy for Matt support

write.csv(GPU1, "GPU1.csv") # Copy for Matt support

write.csv(Power_Duration2, "Power_Duration2.csv") # Copy for Matt support
