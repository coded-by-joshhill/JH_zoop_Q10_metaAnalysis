# Cleaning historic Q10 data
# Josh Hill
# 29/06/26



# Packages ----
library(tidyverse)
library(janitor)
library(worrms)



# Read in the data ----
dat <- read_csv("Data/Historic_Q10_dat.csv") %>%
  mutate(ref_no = paste0("Hill_", row_number()), # create a unique identifier (e.g., Hill_row#)
         taxa = str_squish(taxa),
         rate = recode(rate,
                        "AmmoniaExcretion" = "ExcretionAmmonia",
                       .default = rate)) %>% 
  filter(taxa != "Zooplankton") %>% # remove these observations because we cannot derive Aphia ID and specific classifications...
  relocate(ref_no, .before = everything()) %>%  # move refno before all columns
  select(-temp_range_C) # remove temp_range, we can derive this from min or max anyway later
glimpse(dat)


# Look at all unique taxon
dat %>% 
  distinct(taxa) %>% 
  arrange(taxa) %>% 
  print(n = "Inf")
# All seems to be in order
 
 

# Subset taxa data to get AphiaIDs and classifications ----
taxaDat <- dat %>% 
  select(taxa) %>% 
  distinct(taxa) %>% 
  arrange(taxa)
  
  # Get AphiaIDs
  taxaDatID <- taxaDat %>% 
    mutate(AphiaID = map_int(
      taxa, # extract AphiaIDs for taxa using the worrms package
      possibly(~ { Sys.sleep(0.3); wm_name2id(.x)}, # use Sys.sleep to prevent overloading API
               otherwise = NA_integer_)))  # Set to NA if unable to get AphiaID

    taxaDatID %>% 
      summary() # ensure there are no NAs...
      # There's two...
      taxaDatID %>% filter(is.na(AphiaID)) 
      # Appendicularia and Ctenophora
    
      # Manually add Aphia ids for missing data from WoRMS database
      taxaDatID <- taxaDatID %>%
        mutate(AphiaID = if_else(taxa == "Appendicularia", 146421, AphiaID)) %>% 
        mutate(AphiaID = if_else(taxa == "Ctenophora", 1248, AphiaID))


  # Add classifications using AphiaIDs
  taxaDatClass <- taxaDatID %>%
    mutate(classification = map(AphiaID, ~ { # get taxonomic classifications for each AphiaID 
      wm_classification(.x)}))
  

  # Unnest classification info and update taxaDat
  taxaDat <- taxaDatClass %>%
    select(taxa, classification) %>%
    unnest(classification) %>% # Unnest classification information into separate columns
    select(taxa, rank, scientificname) %>%
    pivot_wider(names_from = rank,
                values_from = scientificname) %>% 
    select(taxa, Phylum, Class, Order, Family, Genus) %>% # only keep necessary columns
    clean_names(case = "snake")

  

# Join taxa info back into Q10 data ----
datClean <- dat %>% 
    left_join(taxaDat, by = "taxa") %>% 
    relocate(c(phylum, class, order, family, genus), .before = taxa) %>% 
    mutate(
      # Create custom size groupings following Grigoratou et al. 2025 Figure 1
      sizeGrp = case_when(
        # Mesoplankton: 0.2 um - 20 mm
        phylum == "Chaetognatha"  ~ "Mesoplankton",
        class == "Appendicularia" ~ "Mesoplankton", # grouped here because we only have Oikopleura dioica
        class == "Copepoda"       ~ "Mesoplankton",
        order == "Pteropoda"      ~ "Mesoplankton", # grouped here because they are juv pteropods and larval bivalves
        # Macroplankton: 20 mm - 200 mm
        phylum == "Annelida"      ~ "Macroplankton", # grouped here because Tomopteris carpenteri is a larger sp.
        phylum == "Cnidaria"      ~ "Macroplankton",
        phylum == "Ctenophora"    ~ "Macroplankton",
        class == "Hydrozoa"       ~ "Macroplankton",
        class == "Malacostraca"   ~ "Macroplankton",
        class == "Thaliacea"      ~ "Macroplankton",
        # Others - not classified and will be excluded from analyses
        order == "Oegopsida"      ~ "OTHER", # an order of Cephalopod (squid), excluded because likely too large and rare as zoops
        .default = "OTHER"),
      
      # Create custom functional groups based on feeding modes
      funcGrp = case_when(
        # Crustaceans
        class == "Copepoda"       ~ "Crustaceans",
        class == "Malacostraca"   ~ "Crustaceans",
        # Gelatinous filter-feeders
        class == "Appendicularia" ~ "GelFilter",
        class == "Thaliacea"      ~ "GelFilter",
        # Gelatinous predators
        phylum == "Chaetognatha"  ~ "GelPreds", # grouped here because they can be quite gelatinous and are highly predatory
        phylum == "Cnidaria"      ~ "GelPreds",
        phylum == "Ctenophora"    ~ "GelPreds",
        # Others - not classified and will be excluded from analyses
        phylum == "Annelida"      ~ "OTHER", # excluded because Tomopteris are quite gelatinious and are generally pretty rare
        phylum == "Mollusca"      ~ "OTHER", # excluded because most are pteropods and don't fit into feeding classification
        .default = "OTHER"),
      
      # Create custom groupings for general zoop groups following Ikeda 2014
      zoopGrp = case_when( 
        phylum == "Annelida"      ~ "Annelids",
        phylum == "Chaetognatha"  ~ "Chaetognaths",
        phylum == "Cnidaria"      ~ "Cnidarians",
        phylum == "Ctenophora"    ~ "Ctenophores",
        phylum == "Mollusca"      ~ "Molluscs",
        class == "Appendicularia" ~ "Appendicularians",
        class == "Copepoda"       ~ "Copepods",
        class == "Thaliacea"      ~ "Thaliaceans",
        order == "Euphausiacea"   ~ "Euphausiids",
        order == "Amphipoda"      ~ "Amphipods",
        order == "Decapoda"       ~ "Decapods",
        order == "Mysidacea"      ~ "Mysids",
        order == "Mysida"         ~ "Mysids",
        # Others - not classified and will be excluded from analyses
        phylum == "Rotifera"      ~ "OTHER", # Only 1 observation
        .default = "OTHER"),
    ) %>% 
    relocate(zoopGrp, .before = phylum) %>% 
    relocate(funcGrp, .before = zoopGrp) %>% 
    relocate(sizeGrp, .before = funcGrp)
  glimpse(datClean)


  # Count number of unique zoopGrps Q10s
  datClean %>% 
    group_by(zoopGrp) %>% 
    mutate(countZoopGrp = sum(zoopGrp > 1, na.rm = TRUE)) %>% 
    distinct(zoopGrp, countZoopGrp) %>% 
    arrange(countZoopGrp)
  # 1 Mysids                      2
  # 2 Chaetognaths                5
  # 3 OTHER                       6 ... these are 1 rotifera and 5 crustaceans..which cannot have a finer zoopGrp classification
  # 4 Ctenophores                 9
  # 5 Thaliaceans                11
  # 6 Appendicularians           13
  # 7 Molluscs                   15
  # 8 Cnidarians                 19
  # 9 Decapods                   30
  # 10 Euphausiids                34
  # 11 Amphipods                  74
  # 12 Copepods                  131
  
# End data cleaning ----

  
# Save the data ----
saveRDS(datClean, "Data/historicQ10_dat.rds")
