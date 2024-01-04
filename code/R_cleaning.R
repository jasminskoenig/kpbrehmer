library(tidyverse)
library(readxl) 

bundestag_ergebnisse <- read_excel("data/bundestag_ergebnisse.xlsx")

bundestag_ergebnisse |> 
  mutate(across(3:GDP, ~ as.numeric(.x))) |> 
  pivot_longer(cols = -c(Wahltag, Wahlbeteiligung),
               names_to = "party",
               values_to = "result") |> 
  mutate(party = trimws(str_remove(party, "\\d"))) ->
  bundestag_long

bundestag_long |> 
  filter(result > 5 | party == "Linke⁠"|party == "DP"|party=="Z" ) |> 
  filter(!is.na(result)) |> 
  mutate(party = str_remove(party, "\\d"),
         color = case_when(
           party == "CDU/CSU" ~ "#4C5B5C",
           party == "SPD" ~ "#DB5461",
           party == "FDP" ~ "#FDE74C",
           party == "Linke⁠" ~ "#7b2cbf",
           party == "Z" ~ "#3A4547",
           party == "AfD" | party == "DP" ~ "#6D4833",
           party == "Grüne⁠" ~"#26A96C" ,
           TRUE ~ NA
           ),
         alpha = case_when(
           party == "CDU/CSU" ~ 0.5,
           party == "SPD" ~ 0.9,
           party == "FDP" ~ 0.9,
           party == "Linke⁠"~ 0.9,
           party == "AfD" ~ 0.9,
           party == "Grüne⁠" ~ 0.9,
           TRUE ~ NA
         ),
         angle = case_when(
           party == "CDU/CSU" ~ 45,
           party == "SPD" ~ 165,
           party == "FDP" ~ 0,
           party == "Linke⁠"~109,
           party == "AfD" ~ 145,
           party == "Grüne⁠" ~79,
           TRUE ~ NA
         )) ->
  bundestag_clean

write_rds(bundestag_clean, "data/bundestag_clean.rds")
write.csv(bundestag_clean, "data/bundestag_clean.csv")

