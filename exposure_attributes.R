
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
data_orig <- readxl::read_excel("data/cva/processed/cva_exposure_attributes.xlsx")


# Build data
################################################################################

# Check 
data_orig %>% 
  count(region, attribute) %>% 
  filter(n!=1)

data_orig %>% 
  count(region)

# Number of regions with each attribute
stats <- data_orig %>% 
  count(attribute) %>% 
  arrange(desc(n))

# Number of attributes per region
nstats <- data_orig %>% 
  count(region) %>% 
  arrange(desc(n))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_orig, aes(y=attribute %>% factor(., levels=stats$attribute), 
                           x=region %>% factor(., levels=nstats$region))) +
  geom_tile() +
  # Labels
  labs(y="Exposure attribute", x="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g

# Export
ggsave(g, filename=file.path(plotdir, "us_cva_exposure_attributes.png"),
       width=5.5, height=5.5, units="in", dpi=600)


