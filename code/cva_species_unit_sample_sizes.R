
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data"
plotdir <- "figures"

# Read data
data_orig <- readRDS("data/cva/processed/cva_data.Rds")

data_orig %>% 
  group_by(reference, region) %>% 
  summarize(n=n(),
            nspp=n_distinct(species))
