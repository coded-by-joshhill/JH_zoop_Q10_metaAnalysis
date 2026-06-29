# Table 1 - Historic Q10 values
# Josh Hill
# 18/11/2025



  # Here I read in the historic Q10 data
  # Separate the data into appropriate groupings
  # Synthesise the current state of Q10 values into one concise table
    # Firstly, split the data wider by the types of Q10 (rates) making rates the columns
    # zoopGrp will be the rows



# Packages and helpers ----
library(tidyverse)



# Read in the data ----
dat <- readRDS("Data/historicQ10_dat.rds") %>% 
  filter(!phylum == "Rotifera") %>% 
  # mutate(zoopGrp = case_when( # Create custom groupings following Ikeda2014
  #   order == "Euphausiacea"   ~ "Euphausiacea",
  #   order == "Amphipoda"      ~ "Amphipoda",
  #   order == "Decapoda"       ~ "Decapoda",
  #   order == "Mysidacea"      ~ "Mysida",
  #   class == "Copepoda"       ~ "Copepoda",
  #   phylum == "Mollusca"      ~ "Mollusca (larvae)",
  #   phylum == "Chaetognatha"  ~ "Chaetognatha",
  #   phylum == "Cnidaria"      ~ "Cnidaria",
  #   phylum == "Ctenophora"    ~ "Ctenophora",
  #   class == "Thaliacea"      ~ "Thaliacea",
  #   class == "Appendicularia" ~ "Appendicularia",
  #   .default = "OTHER")) %>% 
  relocate(zoopGrp, .before = phylum)
glimpse(dat)


# Plot it up ---- 
  # to visualise the distribution of historic temp dependence estimates
# Custom grouping orders 
group_order <- c("Ctenophores",
                 "Cnidarians",
                 "Chaetognaths",
                 "Molluscs",
                 "Amphipods",
                 "Copepods",
                 "Decapods",
                 "Euphausiids",
                 "Mysids",
                 "Appendicularians",
                 "Thaliaceans")
                 
rate_order <- c("Grazing", "Growth", "Respiration", "Excretion")

# Create plotting dataframe
pdat <- dat %>% 
  filter(rate %in% c("Clearance", "Ingestion", "Growth", "Respiration", 
                     "HouseProduction", "ExcretionAmmonia", "ExcretionPhosphate")) %>% 
  select(rate, zoopGrp, Q10) %>% 
  mutate(rate = fct_recode(rate,
                           "Grazing" = "Clearance",
                           "Grazing" = "Ingestion",
                           "Growth"  = "HouseProduction",
                           "Excretion" = "ExcretionAmmonia",
                           "Excretion" = "ExcretionPhosphate"),
         rate = fct_relevel(rate, rate_order),
         zoopGrp = fct_relevel(zoopGrp, group_order))

# Get n_obs and define variables for summary
n_obs <- pdat %>%
  count(rate, zoopGrp)

Z <- 1.96  # critical value for 95% CI

summary_data <- pdat %>%
  group_by(zoopGrp, rate) %>%
  summarise(
    mean_Q10 = mean(Q10, na.rm = TRUE),
    sd_Q10   = sd(Q10, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  left_join(n_obs, by = c("rate", "zoopGrp")) %>%
  mutate(
    se     = sd_Q10 / sqrt(n),
    CI_lwr = mean_Q10 - Z * se,
    CI_upr = mean_Q10 + Z * se)


# Plot it up...
meanQ10s<- ggplot() +
  geom_jitter(data = pdat,
              aes(x = zoopGrp, y = Q10),
              color = "darkgrey",
              width = 0.15, size = 1.5, alpha = 0.5) +
  geom_errorbar(data = summary_data %>% filter(n >2),
                aes(x = zoopGrp, ymin = CI_lwr, ymax = CI_upr),
                width = 0.15,
                color = "black") +
  geom_point(data = summary_data,
             aes(x = zoopGrp, y = mean_Q10),
             color = "black", 
             size = 2) +
  facet_wrap(~rate, scales = "fixed", ncol = 2) +
  theme_bw() +
  labs(x = expression("Taxonomic group"),
       y = expression("Temperature senstivitiy (Q"[10] *")")) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        strip.text = element_text(size = 10, face = "bold"))
meanQ10s

# ggsave("Output/Figure_2_toEdit.pdf", plot = meanQ10s, width = 140, height = 160, units = "mm")
# ggsave("Output/Figure_2_toEdit.png", plot = meanQ10s, width = 140, height = 160, units = "mm")

  