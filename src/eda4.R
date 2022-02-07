# Create a heatmap co-ordinates from XY1 with colour as Total Render time
T1 <- AC7[, c("taskId", "duration")] # Create new tibble with taskId and duration time

TR2 <- left_join(T1, XY1, by = "taskId") # Join together T1 and XY1

TR2 %>%
  ggplot(aes(x = x, y = y, fill = duration)) + geom_tile() # Surfaces with intense texture take more time to render, for example stadium seats and roofs with different surfaces such as satelites. 

# Join together XY1 and Power_Duration1 by taskId
Total1 <- left_join(XY1, Power_Duration2, by = "taskId")
