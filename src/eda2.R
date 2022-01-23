# Create GPU2 sop timestamp isn't as datetime like AC1
GPU2 <-  as_tibble(gpu) %>%
  group_by(hostname) %>%
  arrange(timestamp, .by_group = TRUE)

# Remove the non-numeric characters from the timestamp column in GPU
GPU2$timestamp <- gsub("T", "", GPU2$timestamp) %>% 
  str_replace("Z", "") # Removed alphabetic characters as they're all the same across all 660400 rows

# create time buckets to create comparable data across AC1 and GPU2
DF1 <- head(AC1, 100) %>% arrange(timestamp, .by_group = TRUE) #
BEGIN <- as_datetime("2018-11-0807:41:27.242", tz = "Europe/London")
END <- as_datetime("2018-11-0808:30:16.345", tz = "Europe/London")
BUCKET <- 100
for (T in DF1$timestamp) {
  if(T < BEGIN + BUCKET) {
    T = BEGIN  
  } else{BEGIN <- BEGIN + BUCKET} 
  if(BEGIN > END){
    break
  }
} 
DF2 <- head(GPU2, 100) %>% arrange(timestamp, .by_group = TRUE) #
for (T in DF2$timestamp) {
  if(T < BEGIN + BUCKET) {
    T = BEGIN  
  } else{BEGIN <- BEGIN + BUCKET} 
  if(BEGIN > END){
    break
  }
}
