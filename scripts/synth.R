# Script written by Daniel Anderson for Akhila Nekkanti on Feb 18, 2019

library(tidyverse)

# read in raw data files
files <- fs::dir_ls(here::here("raw-data"), glob = "*.sav")

feb17 <- rio::import(files[1], setclass = "tbl_df") %>% 
  rio::characterize()

jan23 <- rio::import(files[2], setclass = "tbl_df") %>% 
  rio::characterize()

# Check out how many levels are in each response
feb17_resp_counts <- map(names(feb17), ~count(feb17, !!rlang::sym(.x)))
feb17_resp_counts[map_dbl(feb17_resp_counts, nrow) > 10]

# limit to just responses to simulate
feb17_responses <- feb17 %>% 
  select(-V1:-Q1, # We'll basically add these back in later
         -Q6, -Q167, -FB4, -FB5) # these are free response and would be difficult to simulate

# Do the same thing for jan23 file
jan23_resp_counts <- map(names(jan23), ~count(jan23, !!rlang::sym(.x)))
jan23_resp_counts[map_dbl(jan23_resp_counts, nrow) > 10] # Seems like these responses are the same...

jan23_responses <- jan23 %>% 
  select(-V1:-Q1, 
         -Q6, -Q167, -FB4, -FB5) # same selection

# Conduct simuilation
feb17_syn <- synthpop::syn(feb17_responses)
jan23_syn <- synthpop::syn(jan23_responses)


# recode id's, but make them consisten
all_ids <- unique(c(feb17$V1, jan23$V1))
id_df <- tibble(id = seq_along(all_ids),
                V1 = all_ids)

# Add in ID variable and keep the time variable and the school
feb17_d <- bind_rows(
    select(feb17, V1:Q1),
    feb17_syn$syn
  ) %>% 
  left_join(id_df) %>% 
  select(id, everything(), -V1)

jan23_d <- bind_rows(
    select(jan23, V1:Q1),
    jan23_syn$syn
  ) %>% 
  left_join(id_df) %>% 
  select(id, everything(), -V1)

# Create new folder to save synthetic data
fs::dir_create("data")

# Write out data
write_csv(feb17_d, here::here("data", "feb17.csv"))
write_csv(jan23_d, here::here("data", "jan23.csv"))
