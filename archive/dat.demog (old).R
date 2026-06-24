dat.demog <- function(x) {
   rec.educ <- function(x) {
      x = case_match(x, 
                     "Less than high school" ~ 1,
                     "High school graduate" ~ 1,
                     "2 year degree" ~ 2,
                     "4 year degree" ~ 3,
                     "Professional degree" ~ 4,
                     "Doctorate" ~ 4,
                     .default = NA)
      }   
   dat.demog <- x %>% 
      dplyr::select("ResponseId", "StartDate",
                    "Q26.1", "Q26.2", 
                    "Q26.3", "Q26.4", "Q26.5",
                    "Q26.6", "Q26.7", "Q26.8", 
                    "Duration (in seconds)", 
                    "Q_RecaptchaScore") %>% 
      unnest(c("Duration (in seconds)", "Q_RecaptchaScore", "StartDate")) %>% 
      mutate(year = format(StartDate, "%m %y")) %>% 
      mutate(year = factor(year)) %>% 
      rename(id = ResponseId, 
             duration = `Duration (in seconds)`,
             recapscore = Q_RecaptchaScore) %>% 
      mutate(Gender = case_match(Q26.1, 
                                 "Man" ~ 1, 
                                 "Woman" ~ 2, 
                                 "Non-binary" ~ 3,
                                 "Other" ~ 3, 
                                 .default = 3)) %>% 
      mutate(Gender = factor(Gender, labels = c("Man", "Woman", 
                             "Non-bin./Other/Ref."))) %>% 
      mutate(Age = case_match(Q26.2, 
                          "18 - 19" ~ 1,
                          "20 - 24" ~ 2,
                          "25 -29"  ~ 3,
                          "30 - 34" ~ 4,
                          "35 - 39" ~ 5,
                          "40 - 44" ~ 6,
                          "45 - 49" ~ 7,
                          "50 - 54" ~ 8,
                          "55 - 59" ~ 9,
                          "60- 64" ~ 10,
                          "65 - 69" ~ 11,
                          "70 - 74" ~ 12,
                          "75 - 79" ~ 12,
                          "80 - 85" ~ 12,
                          .default = 4)) %>% 
      mutate(across(c("Q26.3", "Q26.4", "Q26.5"), rec.educ)) %>% 
      rowwise() %>% 
         mutate(Par.Educ = mean(c_across(Q26.4:Q26.5), na.rm = TRUE)) %>% 
      ungroup() %>% 
      rename(Educ = Q26.3, Ma.Educ = Q26.4, Fa.Educ = Q26.5) %>% 
      mutate(Inc = case_match(Q26.6, 
                          "Less than $10,000" ~ 1,
                           "$10,000 - $19,999" ~ 2,
                           "$20,000 - $29,999" ~ 3,
                           "$30,000 - $39,999" ~ 4,
                           "$40,000 - $49,999" ~ 5,
                           "$50,000 - $59,999" ~ 6,
                           "$60,000 - $69,999" ~ 7,
                           "$70,000 - $79,999" ~ 8,
                           "$80,000 - $89,999" ~ 9,
                           "$100,000 - $149,999" ~ 10,
                           "More than $150,000" ~ 11,
                          .default = 4)) %>% 
      mutate(Inc2 = Inc^2, Age2 = Age^2) %>% 
      mutate(Race = case_match(Q26.7,
                               "White" ~ 1,
                               "Black or African American" ~ 2,
                               "Asian" ~ 3,
                               "Hispanic" ~ 4,
                               "Multiracial" ~ 5,
                               "American Indian or Alaska Native" ~ 6,
                               "Native Hawaiian or Pacific Islander" ~ 6,
                               "Other" ~ 6,
                               .default = 6)) %>% 
      mutate(Race = factor(Race, labels = c("White", "Black", "Asian", "Hisp.", 
                                            "Mult.", "Native/Other"))) %>% 
      mutate(polviews = case_match(Q26.8, 
                              "Extremely liberal" ~ 1,
                              "Liberal" ~ 2,
                              "Slightly liberal" ~ 3, 
                              "Moderate" ~ 4,
                              "Slightly conservative" ~ 5,
                              "Conservative" ~ 6,
                              "Extremely Conservative" ~ 7, 
                              .default = 4))  %>% 
      dplyr::select(-starts_with("Q"), -StartDate)  
      dat.demog$Gender <- relevel(dat.demog$Gender, ref = "Man")
      dat.demog$Race <- relevel(dat.demog$Race, ref = "White")
return(dat.demog)
}