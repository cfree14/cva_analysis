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
plotdir <- "data/cva/figures"

# Read subscores
data_orig <- readxl::read_excel(file.path(indir, "all-cva-data_manually_formatted.xlsx"), na=c("null", "N/A"), col_types = "text")

# Read scores
scores <- readRDS(file.path(outdir, "cva_data.Rds"))

# Build species key
spp_key <- scores %>% 
  select(comm_name, species) %>% 
  unique()
freeR::which_duplicated(spp_key$comm_name)

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Remove empty
  filter(!is.na(comm_name1)) %>% 
  # Fill region
  fill(region, .direction="down") %>% 
  # Format region
  mutate(region=recode(region,
                       "Gulf"="Gulf of Mexico",
                       "NE"="Northeast habitat",
                       "NEVA"="Northeast",
                       "SAVA"="South Atlantic",
                       "WCVA Salmon"="Pacific salmon",
                       "Pacific"="Western Pacific",
                       "WCVA General"="Pacific",
                       "MMCVA"="Marine mammals")) %>% 
  # Format system
  mutate(system=gsub("'|`", "", system) %>% stringr::str_to_sentence(),
         system=recode(system,
                       "Cps"="Coastal pelagic species",
                       "Hms"="Highly migratory species",
                       "Forage"='Forage fish',
                       "Pink"="Pink salmon",
                       "Sockeye"="Sockeye salmon",
                       "Chum"="Chum salmon",
                       "Coho"="Coho salmon")) %>% 
  # Format metric
  mutate(metric=gsub("'|`", "", metric)) %>% 
  # Format attribute
  mutate(attribute=gsub("'|`", "", attribute),
         attribute=stringr::str_to_sentence(attribute),
         attribute=recode(attribute,
                          "Air temp"="Air temperature",
                          "Bottom temp"="Bottom temperature",
                          "River temp"="River temperature",
                          "Sea surface temp"="Sea surface temperature",
                          "Air temperature (proxy for nearshore ocean temp)"="Air temperature (proxy for nearshore ocean temperature)",
                          "Current ew"="Currents (E/W)",
                          "Current ns"="Currents (N/S)",
                          "Floods"="Flooding",
                          "Ocean ph (standard anomaly)"="Ocean pH (standard anomaly)",                             
                          "Ocean ph (variance ratio)"="Ocean pH (variance ratio)",
                          "Ph"="pH",
                          "Prey/diet specificity"="Prey specificity",
                          "Wind ew"="Winds (E/W)",                                                  
                          "Wind ns"="Winds (N/S)")) %>% 
  # Format score
  mutate(score_catg=gsub("'|`", "", score_catg)) %>% 
  # Format vulnerability
  mutate(vulnerability=gsub("'|`|]", "", vulnerability)) %>% 
  # Merge area
  mutate(area=paste(area1, area2, area3, sep="/"),
         area=gsub("/NA", "", area) %>% stringr::str_squish(),
         area=ifelse(area=="NA", NA, area)) %>%
  select(-c(area1, area2, area3)) %>% 
  # Merge common name
  mutate(comm_name2=ifelse(is.na(comm_name2), "", comm_name2),
         comm_name3=ifelse(is.na(comm_name3), "", comm_name3),
         comm_name=paste(comm_name1, comm_name2, comm_name3) %>% gsub("\\[|\\]|'|`", "", .) %>% stringr::str_squish() ) %>% 
  select(-c(comm_name1, comm_name2, comm_name3)) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Add scientific name
  left_join(spp_key, by=c("comm_name")) %>% 
  # Convert to numeric
  mutate_at(vars(data_quality, n_low:n_very_high, score_avg), as.numeric) %>% 
  # Arrange
  select(region, system, area, comm_name, species, everything())

# Species
spp_key_check <- data %>% 
  count(comm_name, species)
  
# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$region)
table(data$system)
table(data$metric)
sort(unique(data$attribute))

# Areas
table(data$area)

# Scores
table(data$score_catg)
table(data$vulnerability)

# Region key
region_key <- data %>% 
  count(region, system)



# Attribute key
att_key <- data %>% 
  count(metric, attribute)


# Export data
################################################################################

# Simplify
data_out <- data %>% 
  select(-c(score_catg, vulnerability))

# Export
saveRDS(data_out, file=file.path(outdir, "cva_subscores.Rds"))


# Visualize data
################################################################################

# Region to do
region_do <- "Pacific"

# Exposure
#####################################

# Prepare data
data1 <- data %>% 
  # Filter to region of interest
  filter(region==region_do & metric=="Exposure")

# Species stats
stats_spp1 <- data1 %>% 
  group_by(system, comm_name) %>% 
  summarize(mean=mean(mean)) %>% 
  ungroup() %>% 
  arrange(system, desc(mean))

# Attribute stats
stats_att1 <- data1 %>% 
  group_by(attribute) %>% 
  summarize(mean=mean(mean)) %>% 
  ungroup() %>% 
  arrange(desc(mean))

# Order data
data_ordered1 <- data1 %>% 
  mutate(comm_name=factor(comm_name, levels=stats_spp1$comm_name),
         attribute=factor(attribute, levels=stats_att1$attribute))

# Plot exposure
ggplot(data_ordered1,
       aes(x=attribute, y=comm_name, fill=mean)) +
  facet_grid(system~., space="free_y", scales="free_y", 
             labeller = label_wrap_gen(width=10, multi_line = T)) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag="A", title = "Exposure scores") +
  # Legend
  scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Sensitivity
#####################################

# Prepare data
data2 <- data %>% 
  # Filter to region of interest
  filter(region==region_do & metric=="Sensitivity")

# Species stats
table(data2$component_score)
stats_spp2 <- data2 %>% 
  group_by(system, comm_name) %>% 
  summarize(mean=mean(mean),
            score=unique(component_score)) %>% 
  ungroup() %>% 
  arrange(system, desc(mean)) %>% 
  mutate(score=factor(score, levels=c("Low", "Moderate", "High", "Very High")))

# Attribute stats
stats_att2 <- data2 %>% 
  group_by(attribute) %>% 
  summarize(mean=mean(mean)) %>% 
  ungroup() %>% 
  arrange(desc(mean))

# Order data
data_ordered2 <- data2 %>% 
  mutate(comm_name=factor(comm_name, levels=stats_spp2$comm_name),
         attribute=factor(attribute, levels=stats_att2$attribute))

# Plot exposure
ggplot(data_ordered2,
       aes(x=attribute, y=comm_name, fill=mean)) +
  facet_grid(system~., space="free_y", scales="free_y", 
             labeller = label_wrap_gen(width=10, multi_line = T)) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag="B", title = "Sensitivity scores") +
  # Legend
  scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))













