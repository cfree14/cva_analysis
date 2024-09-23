# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/cva/raw/subscores"
outdir <- "data/cva/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "all-cva-data.xlsx"), skip=1)
