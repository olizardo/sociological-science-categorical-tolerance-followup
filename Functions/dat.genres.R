dat.genres <- function(x) {
  rec.taste <- function(z) {
    z = case_match(z, 
                      "Dislike a great deal" ~ 1, 
                      "Dislike somewhat" ~ 2, 
                      "Neither like nor dislike" ~ 3,
                      "Like somewhat" ~ 4,
                      "Like a great deal" ~ 5,
                      NA ~ NA)
      }
  genres.1 <- c("Classical", "Country", 
                "Disco", "Jazz", "Musicals", 
                "Pop", "R_and_B", "Rap", 
                "Reggae", "Rock", "EDM", "Metal")
  dat.grs <- x %>% 
    dplyr::select("ResponseId", starts_with("Q3.")) %>%
    rename(id = ResponseId) %>% 
    mutate(across(Q3.1_1:Q3.1_12, rec.taste)) %>% 
    na.omit()
  dat.g <- dplyr::select(dat.grs, c(1:13))
  dat.g.ord <- dplyr::select(dat.grs, c(1, 14:25)) %>% 
    mutate(across(2:13 , as.numeric)) 
  names(dat.g)[2:13] <- genres.1
  names(dat.g.ord)[2:13] <- genres.1
  dat.g.long <- dat.g %>% 
    pivot_longer(
      cols = c(2:13),
      names_to = "genre",
      values_to = "eval.g"
    ) 
  dat.g.ord.long <- dat.g.ord %>% 
    pivot_longer(
      cols = c(2:13),
      names_to = "genre",
      values_to = "ord.g"
    )
  dat.g.long <- left_join(dat.g.long, dat.g.ord.long)
  return(list(dat.g = dat.g, dat.g.ord = dat.g.ord, dat.g.long = dat.g.long))
}