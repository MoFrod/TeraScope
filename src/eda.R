# Count how many unique hosts there are
hosts <- AC1 %>%
  group_by(hostname) %>%
  summarise (n_distinct(hostname)) %>%
  count() %>%
  print() # Hosts must be the GPU node, as we know there are 1024


       
       
AC1 %>%
  group_by(hostname, eventName) %>%
  distinct(timestamp)%>%
  arrange(timestamp) %>%
  mutate(runtime = c(NA, diff(timestamp))) %>%
         print()
       
       
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
