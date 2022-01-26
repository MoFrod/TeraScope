# What is the relationship between GPU power draw and performance?

# Quickplot power draw to utilisation of GPU core
GPU1 %>% head(100) %>%
  ggplot(aes(x = timestamp)) + geom_line(aes(y = powerDrawWatt), colour = "#b2df8a", size = 1) + geom_line(aes(y = gpuUtilPerc), colour = "#1f78b4", size = 1)

