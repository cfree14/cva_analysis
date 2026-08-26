
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
data_orig <- readxl::read_excel("data/cva/processed/cva_sensitivity_attributes.xlsx")

# Format data
atts <- data_orig %>% 
  filter(study=="Loughran") %>% pull(attribute) %>% rev()

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
ggplot(data_orig, aes(y=factor(attribute, levels=atts), x=region)) +
  geom_tile() +
  # Labels
  labs(y="Sensitivity attribute", x="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



