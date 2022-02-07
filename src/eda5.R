#Can we identify particular GPU cards (based on their serial numbers) whose performance differs to other cards? 

# Join TR1 with Performance
TR_P <- left_join(TR1, Performance, by = "hostname")

# Join TR_P with GPU1 by hostname
s_GPU <- left_join(TR_P, GPU1, by = "hostname")

# How many unique hostnames are there?
hosts1 <- s_GPU %>%
  group_by(hostname) %>% # Group by hostname
  summarise (n_distinct(hostname)) %>% # Summarise number of distinct hostnames
  count() %>%
  print()

# Group s_GPU by hostname
s_GPU <- s_GPU %>%
  group_by(hostname)

# Have one row per unique hostname
s_GPU1 <- distinct(s_GPU, hostname, .keep_all = TRUE)
  
  
# Plot 
s_GPU %>%
  ggplot(aes(x = av_mem, y = time)) + geom_point(position = "jitter") + geom_smooth()
