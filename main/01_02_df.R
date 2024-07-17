
Lframe <- Lframe %>%
  mutate(sex = as.factor(sex),
         free_case = as.factor(free_case),
         corigin = as.factor(corigin),
         religious_affiliation = as.factor(religious_affiliation),
         german_speaking = as.factor(german_speaking),
         german_writing = as.factor(german_writing),
         german_reading = as.factor(german_reading),
         school_degree_low = as.factor(school_degree_low),
         school_degree_med = as.factor(school_degree_med),
         school_degree_high = as.factor(school_degree_high),
         vocational_training = as.factor(vocational_training))
Rframe <- Rframe %>%
  mutate(sex = as.factor(sex),
         free_case = as.factor(free_case),
         corigin = as.factor(corigin),
         religious_affiliation = as.factor(religious_affiliation),
         german_speaking = as.factor(german_speaking),
         german_writing = as.factor(german_writing),
         german_reading = as.factor(german_reading),
         school_degree_low = as.factor(school_degree_low),
         school_degree_med = as.factor(school_degree_med),
         school_degree_high = as.factor(school_degree_high),
         vocational_training = as.factor(vocational_training))
