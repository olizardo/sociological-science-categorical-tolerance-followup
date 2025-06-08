dat.demog <- function(x) {
   w <- x %>% 
      select("ResponseId", "StartDate",
                    "Q26.1", "Q26.2", 
                    "Q26.3", "Q26.4", "Q26.5",
                    "Q26.6", "Q26.7", "Q26.8", 
                    "Duration (in seconds)", 
                    "Q_RecaptchaScore") %>% 
      unnest(c("Duration (in seconds)", "Q_RecaptchaScore")) %>% 
      rename(id = ResponseId, 
             Duration = `Duration (in seconds)`,
             Captcha = Q_RecaptchaScore,
             Gender = Q26.1,
             Age = Q26.2,
             Educ = Q26.3, Ma.Educ = Q26.4, Fa.Educ = Q26.5,
             Inc = Q26.6,
             Race = Q26.7,
             PolViews = Q26.8
             ) %>% 
      mutate(year = format(StartDate, "%y")) %>% 
      mutate(year = factor(year)) %>% 
      mutate(Black = if_else(Race == "Black or African American", 1, 0)) %>% 
      mutate(Hisp = if_else(Race == "Hispanic", 1, 0)) %>% 
      mutate(Asian = if_else(Race == "Asian", 1, 0)) %>% 
      mutate(White = if_else(Race == "White", 1, 0)) %>%
      mutate(Mult = if_else(Race == "Multiracial", 1, 0)) %>% 
      mutate(Other = if_else(Race == "Other", 1, 0)) %>% 
      mutate(Native = if_else(Race == "American Indian or Alaska Native", 1, 0)) %>% 
      mutate(Woman = if_else(Gender == "Woman", 1, 0)) %>% 
      mutate(Age.n = as.numeric(Age)) %>% 
      mutate(Age2 = Age.n^2) %>% 
      mutate(Educ.n = as.numeric(Educ)) %>% 
      mutate(Educ2 = Educ.n^2) %>% 
      mutate(low.Educ = if_else(Educ.n == 1, 1, 0)) %>% 
      mutate(polviews.n = as.numeric(PolViews)) %>% 
      mutate(Inc.n = as.numeric(Inc))
      return(w)
      }