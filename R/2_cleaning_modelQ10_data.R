# Model Q10s
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

# ---- removed below because we no longer include NEMUR ---- #
# datClean <- dat %>%
#   filter(rate != "All rates") %>% # Create a new dataframe for model Q10s...
#   bind_rows(dat %>% # bind the initial data frame
#               filter(rate == "All rates") %>% # but only include the NUMERO model values...
#               slice(rep(1, 4)) %>% # slice the values for the only row and replicate them 4 times..,
#               mutate(rate = c("Grazing", "Growth", "Respiration", "Excretion"))) # and give them new rate values instead of All rates


# Subset the data and generate simple 
# Get n_obs
n_obs <- dat %>%
  group_by(rate) %>% 
  drop_na(Q10) %>% 
  summarise(n = n())

Z <- 1.96  # critical value for 95% CI

# Get meanQ10 and 95% CIs and bind the number of obs 
sumDat <- dat %>% 
  drop_na(Q10) %>% 
  group_by(rate) %>%
  summarise(mean_Q10 = mean(Q10),
            sd_Q10 = sd(Q10)) %>%
  left_join(n_obs, by = "rate") %>%
  mutate(se = sd_Q10 / sqrt(n),
         CI_lwr = mean_Q10 - Z * se,
         CI_upr = mean_Q10 + Z * se) %>% 
  mutate(name = "modelQ10")

ggplot() +
  geom_point(data = dat,
             aes(x = name, y = Q10),
             colour = "grey60",
             alpha = 0.5,
             position = position_jitter(width = 0.1)) +
  geom_errorbar(data = sumDat,
                aes(x = name, ymin = CI_lwr, ymax = CI_upr, colour = rate),
                width = 0.1) +
  geom_point(data = sumDat,
             aes(x = name, y = mean_Q10, colour = rate),
             size = 3) +
  theme_bw() +
  labs(x = expression(bold("Temp dependence in MEMs")), y = expression(bold("Q"[10]*""))) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~rate, scales = "fixed", ncol = 2)
   
# Great, looks good. 

# Save as RDS
saveRDS(dat, "Data/ModelQ10_dat.rds")
saveRDS(sumDat, "Data/ModelQ10summary_dat.rds")





