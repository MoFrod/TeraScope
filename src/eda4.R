#Can we identify particular GPU cards (based on their serial numbers) whose performance differs to other cards? 

# Join TR1 with Performance
TR_P <- left_join(TR1, Performance, by = "hostname")

# Select to x of TR_P and match it to the hosts in GPU1
s_GPU <- TR_P %>%
  arrange(desc(av_duration)) %>%
  head(50)


s_GPU1 <- left_join(s_GPU, GPU1, by = "hostname")
