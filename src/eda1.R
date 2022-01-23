# Which event types dominate task run times? 
# The data mining success criteria is the execution time (duration) of event types to determine which event types have the longest execution time. 

# Plot duration of eventName activities
ggplot(AC2, aes(x = eventName, y = duration)) + geom_boxplot() # Total Render has longest execution time because it accounts for the whole rendering process.

# Remove Total Render from results
AC3 <- AC2 %>%
  filter(eventName != "TotalRender")

# LABEL AND SAVE # Plot duration of eventName activities without total render
ggplot(AC3, aes(x = eventName, y = duration)) + geom_boxplot() # Render dominates run time with the longest execution times

# Remove Render from results
AC4 <- AC3 %>%
  filter(eventName != "Render")

# Plot duration of eventName activities without render to see other execution times better
ggplot(AC4, aes(x = eventName, y = duration)) + geom_boxplot() # Uploading has a significantly larger execution time than tiling and saving config.

# Remove Uploading from results
AC5 <- AC4 %>%
  filter(eventName != "Uploading")

# LABEL AND SAVE # Plot duration of eventName activities without uploading to see saving config and tiling execution times better
ggplot(AC5, aes(x = eventName, y = duration)) + geom_boxplot() 

# Which hosts have the longest TotalRender time?
#AC6 <- AC2 %>%
  #filter(eventName == "TotalRender") %>%
  #pivot_wider(names_from = hostname, values_from = duration) %>%
  #pivot_wider(names_from = n, values from)


#rt_hosts <- AC2 %>%
  #summarise(hostname, count = mean(duration, na.rm = TRUE))

#AC6 <- AC2 %>%
  #filter(eventName == "TotalRender") %>%
  #pivot_wider(names_from = hostname, values_from = duration) 

