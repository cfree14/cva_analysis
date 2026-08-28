
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
data_orig <- readxl::read_excel("data/cva/processed/cva_applications.xlsx")

# TO-DO
# You want to carefully examine raw data (actions assigned to quotes)

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  select(region, application) %>% 
  # Split and gather applicaitons
  separate(col="application", into=paste("application", 1:5), sep=", ") %>% 
  gather(key="application_num", value="application", 2:ncol(.)) %>% 
  # Get ride of missing values
  filter(!is.na(application)) %>% 
  # Simplify
  select(region, application) %>% 
  unique()

# Applications stats
napps <- data %>% 
  count(application) %>% 
  arrange(desc(n))
  
# Region stats
nregions <- data %>% 
  count(region) %>% 
  arrange(desc(n))

# Plot data
################################################################################

# Theme
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
g <- ggplot(data, aes(x=factor(region, nregions$region), 
                 y=factor(application, napps$application))) +
  geom_tile() +
  # Labels
  labs(x="Region", y="Potential application") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g  

# Export
ggsave(g, filename=file.path(plotdir, "FigX_cva_applications.png"),
       width=6.5, height=4.5, units="in", dpi=600)




