# Supplementary table of empirical zoopQ10 values
# Josh Hill
# 05/10/26


# Libraries ----
library(tidyverse)
library(writexl)



# Read in the zooplankton Q10 data ----
dat <- readRDS("Data/historicQ10_dat.rds") %>% 
  filter(!zoopGrp == "OTHER") %>% 
  mutate(tempRange_C = paste0(temp_min_C, "–", temp_max_C)) %>% 
  relocate(zoopGrp, .before = phylum) %>% 
  relocate(tempRange_C, .after = temp_max_C)
glimpse(dat)

# Custom grouping order based roughly on phylogeny
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

# Custom rate order 
rate_order <- c("Grazing", "Growth", "Respiration", "Excretion")


# Create a table for the supplementary materials
tableDat <- dat %>% 
  select(
    # Taxon details
    taxa, zoopGrp, 
    # Q10 details
    rate, Q10, tempRange_C, Q10Type,
    # Author details
    primRef, primRef_URL) %>% 
  mutate(rate = fct_recode(rate, 
                           # Aggregate clearance and ingestion into Grazing
                           "Grazing" = "Clearance",
                           "Grazing" = "Ingestion",
                           # Aggregate House production into Growth
                           "Growth"  = "HouseProduction",
                           # Aggregate Excretion types into Excretion
                           "Excretion" = "ExcretionAmmonia",
                           "Excretion" = "ExcretionPhosphate",
                           "Excretion" = "PhosphateExcretion",
                           # Fix others
                           "Moulting" = "Molting",
                           "Digestion" = "GutClearance"),
         rate = fct_relevel(rate, rate_order), # reorder the rates with our custom order
         zoopGrp = fct_relevel(zoopGrp, group_order)) %>%  # as above but for zooplankton groups
  #filter(rate == c("Grazing", "Growth", "Respiration", "Excretion")) %>% 
  arrange(zoopGrp, taxa, rate, primRef, Q10) %>% 
  mutate(
    primRef_URL = case_when(
      primRef == "Hill2026" ~ "In review",
      .default = primRef_URL)) %>% 
  rename("Originally reported taxon" = "taxa",
         "Taxonomic group" = "zoopGrp",
         "Biological rate" = "rate",
         "Q10 estimate" = "Q10",
         "Temperature range (°C)" = "tempRange_C",
         "Q10 type" = "Q10Type",
         "Primary reference" = "primRef",
         "Primary reference DOI/URL" = "primRef_URL")


  

# Check data
head(tableDat, n = 20) 
# looks good

# Save as excel sheet
write_xlsx(tableDat, "Output/suppTableZQ10.xlsx")

