# Code from model q10s in figure

# Removed for now because the audience isn't specifically modellers
# modelDat <- read_csv("https://www.dropbox.com/scl/fi/eec1k6xmdb9815zuetnbb/ModelQ10s_dat.csv?rlkey=ydh1m3b5jwt4ab4k40fzck1ex&st=bwd7hqt8&dl=1") %>% 
#   filter(Q10 != "–") %>% # remove the observation where a model does not parameterise temperature dependence
#   mutate(Q10 = as.numeric(Q10))
# glimpse(modelDat)


# Create a plotting dataframe for the model-assumed Q10 data ----       # Removed the model-specific Q10 values
# modQ10_pdat <- modelDat %>% 
#   group_by(rate) %>% 
#   summarise(meanQ10 = mean(Q10, na.rm = TRUE)) %>% 
#   filter(rate != "All rates") %>%   # keep Grazing and Growth, but apply the "all rates" value of 2 to Respiration and Excretion
#   bind_rows(tibble(rate = c("Respiration", "Excretion"), meanQ10 = 2))
# modQ10_pdat

# # Dotted line to show Q10s that models often use
# geom_hline(data = modQ10_pdat,
#            aes(yintercept = meanQ10, linetype = "assumed"),
#            colour = "red", linewidth = 0.6) +
