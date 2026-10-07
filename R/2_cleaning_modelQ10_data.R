# Cleaning model Q10s
# Josh H
# 25/08/26


# Packages ----
library(tidyverse)



# Read in the data ----
dat <- read_csv("Data/ModelQ10s_dat.csv") %>% 
  select(primRef, model, taxa, rate, Q10, modelType) %>% # select variables
  mutate(Q10 = case_when(model == "BLING" ~ NA, # Change BLING Q10 to NA because it was "-" prior
                         .default = Q10)) %>% # keep remaining Q10 vals as is
  mutate(Q10 = as.numeric(Q10), # make Q10 numeric
         name = "modelQ10") %>% # make a generic identifying name for the values for when we combine plots
  mutate_if(is.character, factor) %>%  # make all chr vars a factor
  filter_out(modelType == "Ecosystem") # exclude ecosystem models and only include BGCs of ESMs
glimpse(dat)


# Subset the data and generate simple 
# Get n_obs
n_obs <- dat %>%
  group_by(rate) %>% 
  drop_na(Q10) %>% 
  summarise(n = n())

Z <- 1.96  # critical value for 95% CI

# Get meanQ10 and 95% CIs and bind the number of obs ----
sumDat <- dat %>% 
  drop_na(Q10) %>% 
  group_by(rate) %>% # group by rate
  summarise(mean_Q10 = mean(Q10), # compute mean
            sd_Q10 = sd(Q10)) %>% # compute standard deviation
  left_join(n_obs, by = "rate") %>% # left join the number of observations by rate
  mutate(se = sd_Q10 / sqrt(n), # compute standard error based on the sd
         CI_lwr = mean_Q10 - Z * se, # generate CI based on SE
         CI_upr = mean_Q10 + Z * se) %>% # generate CI based on SE
  mutate(name = "modelQ10")
sumDat

# Save as RDS
saveRDS(dat, "Data/ModelQ10_dat.rds") # save the cleaned model-specific Q10 data
saveRDS(sumDat, "Data/ModelQ10summary_dat.rds") # save the summary data
