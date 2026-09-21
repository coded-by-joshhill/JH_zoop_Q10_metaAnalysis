# Historic Q10 values
# Josh Hill
# 17/07/2026


# Packages and helpers ----
library(tidyverse)
library(ggtext) # For fixing subscripts on plot easy...



# Read in the zooplankton Q10 data ----
dat <- readRDS("Data/historicQ10_dat.rds") %>% 
  filter(!zoopGrp == "OTHER") %>% 
  relocate(zoopGrp, .before = phylum)
glimpse(dat)

  # Custom rate order 
  rate_order <- c("Grazing", "Growth", "Respiration", "Excretion")

# Read in the Model Q10 data
modDat <- readRDS("Data/ModelQ10_dat.rds") %>% 
  mutate(name = "Model Q10", # update name
         rate = fct_relevel(rate, rate_order)) %>% # relevel the rate order
  drop_na(Q10)

# Read in the Model Q10 Summary
modDatSum <- readRDS("Data/ModelQ10summary_dat.rds") %>% 
  mutate(name = "Model Q10", # update name
         rate = fct_relevel(rate, rate_order)) # relevel the rate order

# Custom grouping order
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
                 

# Create plotting dataframe ----
pdat <- dat %>% 
  filter(Q10 < 20) %>% # drop extreme outliers...
  # Filter for all initial rate types
  filter(rate %in% c("Clearance", "Ingestion", "Growth", "Respiration", 
                     "HouseProduction", "Excretion", "ExcretionAmmonia", "ExcretionPhosphate")) %>% 
  select(rate, zoopGrp, Q10) %>% 
  # Harmonise the rates to common terms
  mutate(rate = fct_recode(rate, 
                           # Aggregate clearance and ingestion into Grazing
                           "Grazing" = "Clearance",
                           "Grazing" = "Ingestion",
                           # Aggregate House production into Growth
                           "Growth"  = "HouseProduction",
                           # Aggregate Excretion types into Excretion
                           "Excretion" = "ExcretionAmmonia",
                           "Excretion" = "ExcretionPhosphate"),
         rate = fct_relevel(rate, rate_order), # reorder the rates with our custom order
         zoopGrp = fct_relevel(zoopGrp, group_order)) # as above but for zooplankton groups


# Get n_obs and define variables for summary
n_obs <- pdat %>%
  count(rate, zoopGrp)



# Create a summary of the Q10 data, including confidence intervals ----
Z <- 1.96  # critical value for 95% CI

# Summary of zooplankton Q10 data
summary_data <- pdat %>%
  group_by(zoopGrp, rate) %>%
  summarise(mean_Q10 = mean(Q10, na.rm = TRUE), # calculate mean Q10
            sd_Q10 = sd(Q10, na.rm = TRUE), # calculate standard deviation
            .groups = "drop") %>%
  left_join(n_obs, by = c("rate", "zoopGrp")) %>% # left join the number of observations by rate and zooplankton group
  mutate(se = sd_Q10 / sqrt(n), # calculate standard error so we can estimate 95% CI
         # Estimate CIs based on SE
         CI_lwr = mean_Q10 - Z * se,
         CI_upr = mean_Q10 + Z * se)

# add "Overall zooplankton Q10" based on our initial 
summary_data_wOverall <- summary_data %>%
  bind_rows(
    summary_data %>%
      group_by(rate) %>%
      summarise(zoopGrp  = "Overall zooplankton Q10",
                mean_Q10 = mean(mean_Q10, na.rm = TRUE),
                sd_Q10 = NA_real_,
                n = sum(!is.na(se)),
                se = sqrt(sum(se^2, na.rm = TRUE)) /n,
                .groups = "drop") %>%
      mutate(CI_lwr = mean_Q10 - Z * se,
             CI_upr = mean_Q10 + Z * se))

summary_data_wOverall %>% arrange(rate, mean_Q10) %>% view()


# Arrange the x-axis text order...
grp_order <- levels(pdat$zoopGrp)
axis_levels <- c(grp_order, "Overall zooplankton Q10", "Model Q10")



# Plot it up...
meanQ10s <- ggplot() +
  # Background panel for overall Z
  annotate("rect",
           xmin = length(grp_order) + 0.5, xmax = length(grp_order) + 1.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  # Background panel for Model Q10 vals
  annotate("rect",
           xmin = length(grp_order) + 1.5, xmax = length(grp_order) + 2.6, 
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.4) +
  # Text label for Overall zooplankton Q10
  geom_text(data = summary_data_wOverall %>% filter(zoopGrp == "Overall zooplankton Q10"),
            aes(x = zoopGrp, y = Inf, label = sprintf("%.2f", mean_Q10)),
            vjust = -0.5, size = 2.5) +
  # Text label for Model Q10
  geom_text(data = modDatSum,
            aes(x = name, y = Inf, label = sprintf("%.2f", mean_Q10)),
            vjust = -0.5, size = 2.5) +
  # Horizontal line showing mean Q10 used in models
  # geom_hline(data = modDatSum, 
  #            aes(yintercept = mean_Q10, colour =), # the value of meanQ10 in models...
  #            linetype = "dashed",
  #            linewidth = 0.5) +
  # Dashed vertical separator before the OverallZ column
  geom_vline(xintercept = length(grp_order) + 0.5,
             linetype = "dashed", 
             colour = "grey60", 
             linewidth = 0.5) +
  # Dashed vertical separator before the Q10 vals in models
  geom_vline(xintercept = length(grp_order) + 1.5,
             linetype = "solid", 
             colour = "black", 
             linewidth = 0.5) +
  # Raw Q10 data from the literature
  geom_point(data = pdat,
             aes(x = zoopGrp, y = Q10, colour = "raw"),
             size = 1.5, 
             alpha = 0.3,
             position = position_jitter(width = 0.2, height = 0)) +
  # CIs based on SE (zooplankton grps + overall Z - points w/out e)
  geom_errorbar(data = summary_data_wOverall %>% filter(n > 2),
                aes(x = zoopGrp, ymin = CI_lwr, ymax = CI_upr),
                width = 0.15, 
                colour = "black") +
  # mean Q10 points
  geom_point(data = summary_data_wOverall %>% filter(zoopGrp != "Overall zooplankton Q10"),
             aes(x = zoopGrp, y = mean_Q10, colour = "mean"),
             size = 2) +
  # OverallZ mean
  geom_point(data = summary_data_wOverall %>% filter(zoopGrp == "Overall zooplankton Q10"),
             aes(x = zoopGrp, y = mean_Q10),
             size = 2, colour = "black") +
  # CIs for Q10 in MEMs
  geom_errorbar(data = modDatSum,
                aes(x = name, ymin = CI_lwr, ymax = CI_upr),
                width = 0.15,
                colour = "black") +
  # Raw Q10 in MEMs points
  geom_point(data = modDat,
             aes(x = name, y = Q10, colour = "raw"),
             size = 1.5,
             alpha = 0.3,
             position = position_jitter(width = 0.15, height = 0)) +
  # Q10 in MEMs mean point
  geom_point(data = modDatSum,
             aes(x = name, y = mean_Q10, colour = "mean"),
             size = 2) +
  # # Q10 text taxonomic groups
  # geom_text(data = summary_data %>% filter(zoopGrp != "Overall zooplankton"),
  #           aes(x = zoopGrp, y = -.5, label = sprintf("%.2f", mean_Q10)),
  #           size = 3, colour = "black") +
  # # OverallZ Q10 text
  # geom_text(data = summary_data %>% filter(zoopGrp == "Overall zooplankton"),
  #           aes(x = zoopGrp, y = -.5, label = sprintf("%.2f", mean_Q10)),
  #           size = 3, colour = "black", fontface = "bold") +
  # Facet wrap by the rate processes
  facet_wrap(~rate, scales = "fixed", ncol = 2) +
  theme_bw() +
  labs(x = NULL,
       y = expression("Temperature sensitivity (Q"[10] *")")) +
  # Force the y-axis values to be between 0 and 8 to show raw data
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  # Use custom taxonomic order with Overall specified as the last "column"
  scale_x_discrete(limits = axis_levels,
                   labels = c(setNames(grp_order, grp_order),
                              "Overall zooplankton Q10" = "Overall zooplankton Q<sub>10</sub>",
                              "Model Q10" = "Model Q<sub>10</sub>"),
                   expand = expansion(add = c(0.4, 0.4))) +
  scale_colour_manual(name = NULL,
                      values = c("raw" = "darkgrey", "mean" = "black"),
                      labels = c("raw"  = expression("Raw Q"[10]),
                                 "mean" = expression("Mean Q"[10]))) +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_markdown(angle = 35, hjust = 1),
        panel.border = element_rect(colour = "grey30", linewidth = 0.3, fill = NA),
        panel.spacing.y = unit(0.5, "lines"),
        
        legend.position = "top",
        legend.margin = margin(t = -3, b = -7),
        strip.text = element_text(size = 10, face = "bold", margin = margin(b = 8)),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 26),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(fill = "NA", colour = "NA")) +
  coord_cartesian(clip = "off") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))
meanQ10s


# ggsave("Output/Q10Plot.pdf", plot = meanQ10s# ggsave("Output/Q10Plot.pdf", plot = meanQ10s# ggsave("Output/Q10Plot.pdf", plot = meanQ10s, width = 180, height = 160, units = "mm", dpi = 300)
ggsave("Output/Q10Plot.png", plot = meanQ10s, width = 180, height = 160, units = "mm", dpi = 300)

  