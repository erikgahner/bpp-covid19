library("tidyverse")
library("haven")
library("labelled")

gallup_raw <- read_sav("S64653.sav") 

gallup <- gallup_raw %>% 
  mutate(treat_handwash = ifelse(is.na(RUC1A), 1, 0),
         treat_closecontact = ifelse(is.na(RUC3A), 1, 0),
         outcome_handwash = RUC2,
         outcome_closecontact = RUC4,
         outcome_handwash_10 = ifelse(RUC2 >= 10, 1, 0),
         outcome_closecontact_10 = ifelse(RUC4 >= 10, 1, 0),
         male = ifelse(KOEN == 2, 0, 1),
         age = ALDER,
         parti = case_when(
           PARTI_G == 1 ~ "Enhedslisten",
           PARTI_G == 2 ~ "SF",
           PARTI_G == 3 ~ "Socialdemokratiet",
           PARTI_G == 4 ~ "Radikale",
           PARTI_G == 5 ~ "Alternativet",
           PARTI_G == 6 ~ "Konservative",
           PARTI_G == 7 ~ "Venstre",
           PARTI_G == 8 ~ "Dansk Folkeparti",
           PARTI_G == 9 ~ "Liberal Alliance",
           PARTI_G == 10 ~ "Nye Borgerlige"
         ),
         gov = case_when(
           PARTI_G %in% c(1, 2, 3, 4, 5) ~ 1,
           PARTI_G %in% c(6, 7, 8, 9, 10) ~ 0,
           TRUE ~ NA_real_
         )) %>% 
  select(treat_handwash, treat_closecontact, outcome_handwash, outcome_closecontact,
         outcome_handwash_10, outcome_closecontact_10, male, age, parti, gov)

gallup %>% write_csv("bpp_covid19.csv")

