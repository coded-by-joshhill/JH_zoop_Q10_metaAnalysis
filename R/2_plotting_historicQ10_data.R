# Historic Q10 values
# Josh Hill
# 17/07/2026


# Packages and helpers ----
library(tidyverse)



# Read in the data ----
dat <- readRDS("Data/historicQ10_dat.rds") %>% 
  filter(!zoopGrp == "OTHER") %>% 
  relocate(zoopGrp, .before = phylum)
glimpse(dat)

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

# Create plotting dataframe ----
pdat <- dat %>% 
  filter(Q10 < 20) %>% # drop extreme outliers...
  filter(rate %in% c("Clearance", "Ingestion", "Growth", "Respiration", 
                     "HouseProduction", "Excretion", "ExcretionAmmonia", "ExcretionPhosphate")) %>% 
  select(rate, zoopGrp, Q10) %>% 
  mutate(rate = fct_recode(rate, # Tidy the rates 
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



# Create a summary of the Q10 data, including confidence intervals ----
Z <- 1.96  # critical value for 95% CI

summary_data <- pdat %>%
  group_by(zoopGrp, rate) %>%
  summarise(mean_Q10 = mean(Q10, na.rm = TRUE),
            sd_Q10 = sd(Q10, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(n_obs, by = c("rate", "zoopGrp")) %>%
  mutate(se = sd_Q10 / sqrt(n),
         CI_lwr = mean_Q10 - Z * se,
         CI_upr = mean_Q10 + Z * se)

# Calculate mean and CI for "Overall zooplankton" per rate and overwrite summary_data
summary_data <- summary_data %>%
  bind_rows(
    summary_data %>%
      group_by(rate) %>%
      summarise(zoopGrp  = "Overall zooplankton",
                mean_Q10 = mean(mean_Q10, na.rm = TRUE),
                sd_Q10   = NA_real_,
                n        = sum(!is.na(se)),
                se       = sqrt(sum(se^2, na.rm = TRUE)) /n,
                .groups  = "drop") %>%
      mutate(CI_lwr = mean_Q10 - Z * se,
             CI_upr = mean_Q10 + Z * se))


grp_order   <- levels(pdat$zoopGrp)
if (is.null(grp_order)) grp_order <- unique(as.character(pdat$zoopGrp))
axis_levels <- c(grp_order, "Overall zooplankton")

# Plot it up...
meanQ10s <- ggplot() +
  # Dashed vertical separator before the OverallZ column
  geom_vline(xintercept = length(grp_order) + 0.5,
             linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  # Raw Q10 data from the literature
  geom_jitter(data = pdat,
              aes(x = zoopGrp, y = Q10, colour = "raw"),
              width = 0.15, size = 1.5, alpha = 0.3) +
  # Error bars based on SE (taxa + overall; overall passes n > 2 via group count)
  geom_errorbar(data = summary_data %>% filter(n > 2),
                aes(x = zoopGrp, ymin = CI_lwr, ymax = CI_upr),
                width = 0.15, colour = "black") +
  # mean Q10
  geom_point(data = summary_data %>% filter(zoopGrp != "Overall zooplankton"),
             aes(x = zoopGrp, y = mean_Q10, colour = "mean"),
             size = 2) +
  # OverallZ mean
  geom_point(data = summary_data %>% filter(zoopGrp == "Overall zooplankton"),
             aes(x = zoopGrp, y = mean_Q10),
             size = 2, colour = "black") +
  # Q10 text taxonomic groups
  geom_text(data = summary_data %>% filter(zoopGrp != "Overall zooplankton"),
            aes(x = zoopGrp, y = -.5, label = sprintf("%.2f", mean_Q10)),
            size = 3, colour = "black") +
  # OverallZ Q10 text
  geom_text(data = summary_data %>% filter(zoopGrp == "Overall zooplankton"),
            aes(x = zoopGrp, y = -.5, label = sprintf("%.2f", mean_Q10)),
            size = 3, colour = "black", fontface = "bold") +
  facet_wrap(~rate, scales = "fixed", ncol = 2) +
  theme_bw() +
  labs(x = NULL,
       y = expression("Temperature sensitivity (Q"[10] *")")) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  # CHANGED: force taxonomic order with Overall pinned last
  scale_x_discrete(limits = axis_levels,
                   expand = expansion(add = c(0.6, 1.1))) +
  scale_colour_manual(name = NULL,
                      values = c("raw" = "darkgrey", "mean" = "black"),
                      labels = c("raw"  = expression("Raw Q"[10]),
                                 "mean" = expression("Mean Q"[10]))) +
  scale_linetype_manual(name = NULL,
                        values = c("assumed" = "dashed"),
                        labels = expression("Commonly assumed model Q"[10])) +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.border = element_rect(colour = "grey30", linewidth = 0.3, fill = NA),
        legend.position = "top",
        strip.text = element_text(size = 10, face = "bold"),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 25)) +
  coord_cartesian(clip = "off") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))
meanQ10s


# ggsave("Output/Q10Plot.pdf", plot = meanQ10s, width = 180, height = 160, units = "mm", dpi = 300)
ggsave("Output/Q10Plot.png", plot = meanQ10s, width = 180, height = 160, units = "mm", dpi = 300)

  