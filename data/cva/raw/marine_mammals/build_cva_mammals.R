# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1_orig <- readxl::read_excel("data/cva/raw/marine_mammals/supplement/pone.0290643.s005_small_manual_edits.xlsx", skip=1)
data2_orig <- readxl::read_excel("data/cva/raw/marine_mammals/supplement/pone.0290643.s006.xlsx", skip=1)


# Species key
################################################################################

# Format data
data1 <- data1_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=species_common_name,
         species=scientific_name, 
         area=stock) %>% 
  # Format area
  mutate(area=recode(area,
                     "GOMx, Oceanic"="GOMx, oceanic",
                     "GOMx, Continental shelf"="GOMx, continental shelf")) %>% 
  # Build stock
  mutate(stock=paste0(comm_name, " (", area, ")"))

# Inspect
freeR::complete(data1)

# Species key
spp_key <- data1 %>% 
  count(functional_group, comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)
freeR::check_names(spp_key$species)

# Results
################################################################################

# Format data
data2 <- data2_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(stock=stock_name, 
         dist_change=distribution_response) %>% 
  # Format stock
  mutate(stock=gsub(")E, S, D", ")", stock),
         stock=gsub(")S", ")", stock),
         stock=gsub(")E", ")", stock)) %>% 
  # Add species info
  left_join(data1, by=c("stock")) %>% 
  # Format functional groups
  mutate(functional_group=paste0(functional_group, "s")) %>% 
  # Clean up common name
  mutate(comm_name=gsub("sp. group", "spp.", comm_name),
         comm_name=recode(comm_name,
                          "Sperm Whale"="Sperm whale")) %>% 
  # Rebuild stock name
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Convert to sentence case
  mutate_at(vars(exposure:vulnerability, dist_change:phenology_response), stringr::str_to_sentence) %>% 
  # Arrange
  select(functional_group, stock, comm_name, species, area, status,
         everything())

# Inspect
str(data2)
freeR::complete(data2)

# Export data
saveRDS(data2, file="data/cva/raw/marine_mammals/Lettrich_etal_2023_cva.Rds")
