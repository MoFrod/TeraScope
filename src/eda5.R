#Can we identify particular GPU cards (based on their serial numbers) whose performance differs to other cards? 

# Filter out gpu cards that have the longest duration
Power_Duration3 <- Power_Duration2 %>%
  group_by(gpuSerial) %>%
  mutate(av_gpu_t = mean(duration))

S_gpu <- Power_Duration3 %>%
  filter(av_gpu_t >= 34) %>%
  arrange(desc(av_gpu_t)) %>%
  head(10)

# Plot S-gpu as barplot
S_gpu %>%
  ggplot(aes(x = av_gpu_t, y =powerDrawWatt, fill = gpuSerial)) + geom_boxplot()
  