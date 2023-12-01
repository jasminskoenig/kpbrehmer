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
  mutate(party = str_remove(party, "\\d"),
         color = case_when(
           party == "CDU/CSU" ~ "#312E2D",
           party == "SPD" ~ "#E0BBB3",
           party == "FDP" ~ "#F9D549",
           party == "Linke⁠" ~ "#D23627",
           party == "AfD" ~ "#6D4833",
           party == "Grüne⁠" ~ "#408960",
           TRUE ~ NA
           ),
         alpha = case_when(
           party == "CDU/CSU" ~ 0.5,
           party == "SPD" ~ 0.7,
           party == "FDP" ~ 0.9,
           party == "Linke⁠"~ 0.9,
           party == "AfD" ~ 0.9,
           party == "Grüne⁠" ~ "#408960",
           TRUE ~ NA
         )) ->
  bundestag_clean

write_rds(bundestag_clean, "data/bundestag_clean.rds")
write.csv(bundestag_clean, "data/bundestag_clean.csv")

