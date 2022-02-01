# Initial data exploration to develop understanding of data sets

# Count how many unique hosts there are
hosts <- AC1 %>%
  group_by(hostname) %>%
  summarise (n_distinct(hostname)) %>%
  count() %>%
  print() # There are 1024

# Are there more GPU serials then hosts?
ghosts <- GPU1 %>%
  group_by(hostname) %>%
  summarise (n_distinct(hostname)) %>%
  count() %>%
  print() # There are 1024 hosts in the GPU data set

gserials <- GPU1 %>%
  group_by(gpuSerial) %>%
  summarise(n_distinct(gpuSerial)) %>%
  count() %>%
  print() # Each virtual machine has one GPU card, there are 1024 gpuserials

# Check whether the timestamps across AC1 and GPU1 are the same
all(AC1$timestamp %in% GPU1$timestamp) #They are not in their current format.

# Check whether the hostnames across AC1 and GPU1 are they same
all(AC1$hostname %in% GPU1$hostname) # They are.

# How many eventNames are there?
eT <- AC1 %>%
  group_by(eventName) %>%
  summarise(n_distinct(eventName)) %>%
  count() %>%
  print()

# How many times does TotalRender happen?
R <- application.checkpoints %>%
  filter(eventName == "TotalRender") %>%
  count() %>%
  print() # Rendering happens 132080 times 

# How many times does rendering happen per host?
TRh <- AC1 %>%
  filter(eventName == "TotalRender") %>%
  count() %>%
  as_tibble() %>%
  arrange(desc(n), .by_group = TRUE) # Some hosts render more than others. Is this because of performance or tasking?

Rh <- AC1 %>%
  filter(eventName == "Render") %>%
  count() # This appears to be the same as TotalRender

# How many taskIds are there?
ID <- AC1 %>%
  group_by(taskId) %>%
  summarise(n_distinct(taskId)) %>% 
  count() %>% 
  print() # There are 65793 unique taskIds

# How many tasks does each host run?
cT <- AC1 %>%
  summarise(n_distinct(taskId)) %>%
  print(., n = 3, width = Inf) # 8b6a0eebc87b4cb2b0539e81075191b900000D runs 71 tasks (most), dcc19f48bb3445a28338db3a8f002e9c00000S runs 58 (least). Thi appears to match the number of times TotalRender happens per host. 

# How many jobIds are there?
j <- AC1 %>%
  group_by(jobId) %>%
  summarise(n_distinct(jobId)) %>%
  count() %>%
  print() # 3

jXY <- XY1 %>% # Is this the same in the other data set?
  summarise(n_distinct(jobId)) %>%
  count() %>%
  print() # Yes, 3

# How many tasks does each jobId have?
cTXY <- XY1 %>%
  summarise(n_distinct(taskId)) %>%
  print()

cTj <- AC1 %>%
  group_by(jobId) %>%
  summarise(n_distinct(taskId)) %>%
  print() # Same as above

# How many tasks in XY1?
cT1 <- XY1 %>%
  group_by(taskId) %>%
  summarise(n_distinct(taskId)) %>%
  count() %>%
  print() # Same as AC2

# Are the task IDs the same across AC1 and XY1?
all(AC1$taskId %in% XY1$taskId) #Yes

# How many gpuUUIDs are there? Is this the same as number of taskIds?
UU <- GPU1 %>%
  group_by(gpuUUID) %>%
  summarise(n_distinct(gpuUUID)) %>%
  count() %>%
  print() # 1024 - there is one per host. 

# Smaller subset of data for Matt support
sub-set <- AC1 %>% head (1000) %>%
write.csv(sub-set, "subset.csv") # Copy for Matt support

# Plot same set of GPU temperature, memory and core utilisation.
GPU1 %>% head(50) %>%
  ggplot(aes(x = timestamp)) + geom_line(aes(y = gpuTempC), colour = "#b2df8a", size = 1) + geom_line(aes(y = gpuUtilPerc), colour = "#1f78b4", size = 1) + geom_line(aes(y = gpuMemUtilPerc), colour = "#a6cee3", size = 1) + scale_colour_brewer(palette = "Paired") 

# Plot duration of events
AC2 %>% head(50) %>%
  ggplot(aes(x = START)) + geom_line(aes(y = duration), colour = "#33a02c", size = 1)

# Plot duration of eventName activities
ggplot(AC2, aes(x = eventName, y = duration)) + geom_boxplot() #Further investigation in EDA1


# DO MORE INITIAL INVESTIGATION ON GPU AND TASK DATA



  #%>%
# AC1 %>%
  mutate(number = row_number()) %>%
  ungroup() %>%
  pivot_wider(id_cols = taskId, names_from = number, values_from = c(eventName, eventType, timestamp, hostname, jobId, taskId), names_glue = "{.value}_{number}")
       

       
       
       #  ggplot(aes(x = num_true, y = n, fill = highest_education_level)) + geom_bar(stat="identity", position="dodge") + scale_fill_brewer(palette = "PuOr") + labs(title = "Number of Demographic Fields by Number of Learners per Education", x = "Number of Demographic Data Fields Provided by Learner", y = "Number of Learners", fill = "Education Level"

# Combine enrolments data for cs course into cyber_security_full_enrolments
       full_enrolments <- rbind(cyber.security.1_enrolments, cyber.security.2_enrolments, cyber.security.3_enrolments, cyber.security.4_enrolments, cyber.security.5_enrolments, cyber.security.6_enrolments, cyber.security.7_enrolments)
       
       # Show empty cells as NA
       full_enrolments <- full_enrolments %>% mutate_all(na_if,"")
       
       # Filter how many students completed and did not complete the course
       complete <- filter(full_enrolments, !is.na(full_enrolments$fully_participated_at))#Filter students who have completed the course
       incomplete <- filter(full_enrolments, is.na(full_enrolments$fully_participated_at)) #Filter students who have not completed the course
       
       # Create data set of students and demographic data for initial investigation
       FL1 <- as_tibble(full_enrolments) %>% #Create tibble
         mutate(completed = !is.na(fully_participated_at), #Is there a non-NA value in fully_participated_at?
                gender_declared = gender != "Unknown", #Is gender something other than unknown?
                country_declared = country != "Unknown", #Is country something other than unknown?
                age_declared = age_range != "Unknown", #Is age something other than unknown?
                education_declared = highest_education_level != "Unknown", #Is education level something other than unknown?
                employment_status_declared = employment_status != "Unknown", #Is employment status something other than unknown?
                employment_area_declared = employment_area != "Unknown") #Is employment area something other than unknown?
       
       # Find the learner IDs of columns which include "declared" in the name.
       # These are the columns we would like to count TRUEs for.
       ids_of_declared_cols <- grep("declared", names(FL1))
       
       # Create a new column called "count" in FL1 containing the value (count of columns) where there is a a vale != FALSE.
       true_counts <- FL1 %>% 
         mutate(num_true = rowSums(.[ids_of_declared_cols] != FALSE)) %>% # Count up how many of the columns have a value != FALSE.
         print(ids_of_declared_cols, num_true, completed, n = 20, width = Inf) # View the selected columns (from line 3) and also the count, to confirm we have the intended result.
       
       # Combine archetype data for cs course into cyber_security_full_archetypes
       full_archetypes <- rbind(cyber.security.1_archetype.survey.responses, cyber.security.2_archetype.survey.responses, cyber.security.3_archetype.survey.responses, cyber.security.4_archetype.survey.responses, cyber.security.5_archetype.survey.responses, cyber.security.6_archetype.survey.responses, cyber.security.7_archetype.survey.responses)
       
       # Join between enrolments and archetype_survey_response on learner_id
       # More info on joins: https://dplyr.tidyverse.org/reference/mutate-joins.html 
       joint_full <- left_join(FL1, full_archetypes, by = "learner_id", archetype)
       
       # Create data set of students, demographic data and archetypes for further investigation
       FL2 <- as_tibble(joint_full) #Create tibble
