# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/cva/raw"
outdir <- "data/cva/processed"
plotdir <- "data/cva/figures"

# Read non-Pacific
data_ne <- readRDS(file.path(indir, "northeast/Hare_etal_2016_cva.Rds")) %>% 
  mutate(reference="Hare et al. 2016",
         region="Northeast")
data_gom <- readRDS(file.path(indir, "gulf_of_mexico/Quinlan_etal_2023_cva.Rds")) %>% 
  mutate(reference="Quinlan et al. 2023",
         region="Gulf of Mexico")
data_sa <- readRDS(file.path(indir, "south_atlantic/Burton_etal_2023_cva.Rds")) %>% 
  mutate(reference="Burton et al. 2023",
         region="South Atlantic")
data_np <- readRDS(file.path(indir, "north_pacific/Spencer_etal_2019_cva.Rds")) %>% 
  mutate(reference="Spencer et al. 2019",
         region="Bering Sea")
data_wp <- readRDS(file.path(indir, "western_pacific/Giddens_etal_2022_cva.Rds")) %>% 
  mutate(reference="Giddens et al. 2022",
         region="Western Pacific")
data_car <- readxl::read_excel(file.path(indir, "caribbean_rec/EDF_tables.xlsx")) %>% 
  mutate(reference="EDF 2022",
         region="Caribbean")
data_mm <- readRDS(file.path(indir, "marine_mammals/Lettrich_etal_2023_cva.Rds")) %>% 
  mutate(reference="Lettrich et al. 2023",
         region="Marine mammals")

# Read Pacific
data_pac <- readRDS(file.path(indir, "pacific/McClure_etal_2023_cva.Rds")) %>% 
  mutate(reference="McClure et al. 2023",
         region="Pacific")

# Read Pacific salmon
data_pac_salmon <- readxl::read_excel(file.path(indir, "pacific_salmon/Crozier_etal_2019_tables.xlsx")) %>% 
  mutate(reference="Crozier et al. 2019",
         region="Pacific salmon",
         functional_group=comm_name,
         comm_name=paste(stock, comm_name, sep="-"))

# Get direction/distribution data for:
# Pacific (Canary rockfish - Puget Sound)
# Western Pacific


# Build data
################################################################################

# Merge data
data <- bind_rows(data_ne, data_sa, data_gom, data_car,
                  data_np, data_wp, data_pac, data_pac_salmon, 
                  data_mm) %>% 
  # Arrange
  select(reference, region, functional_group, comm_name, species, area, 
         vulnerability, sensitivity, exposure, dir_effect, dist_change) %>% 
  # Format directional effect
  mutate(dir_effect=stringr::str_to_sentence(dir_effect)) %>% 
  # Format scores
  mutate(exposure=factor(exposure, levels=c("Low", "Moderate", "High", "Very high")),
         sensitivity=factor(sensitivity, levels=c("Low", "Moderate", "High", "Very high")),
         vulnerability=factor(vulnerability, levels=c("Low", "Moderate", "High", "Very high")),
         dist_change=factor(dist_change, levels=c("Low", "Moderate", "High", "Very high")),
         dir_effect=factor(dir_effect, levels=c("Negative", "Neutral", "Positive"))) %>% 
  # Format functional group
  mutate(functional_group=stringr::str_to_sentence(functional_group),
         functional_group=recode(functional_group,
                                 "Invertebrate"="Invertebrates",
                                 "Cephalopod"="Cephalopods",
                                 "Coastal"="Coastal pelagics",
                                 "Coastal pelagic"="Coastal pelagics",
                                 "Coastal pelagic fish"="Coastal pelagics",
                                 "Coastal pelagic species"="Coastal pelagics",
                                 "Coral reef jegs"="Coral reef JEGS",
                                 "Crab"="Crabs",
                                 "Deep slope"="Deep-slope fish",
                                 "Diadromous"="Diadromous fish",
                                 "Elasmobranch"="Elasmobranchs",
                                 "Forage"="Forage fish",
                                 "Forage fishes"="Forage fish",
                                 "Gadid"="Gadids",
                                 "Grenadier"="Grenadiers",
                                 "Groundish"="Groundfish",
                                 "Grouper"="Groupers",
                                 "Other anadromous"="Other anadromous fish",
                                 "Other coral reef"="Other coral reef fish",
                                 "Pelagic"="Pelagic fish",
                                 "Pelagics"="Pelagic fish",
                                 "Sallmon"="Salmons",
                                 "Sculpin"="Sculpins",
                                 "Shark"="Sharks",
                                 "Snapper"="Snappers" )) %>% 
  # Format species
  mutate(species=recode(species,
                        "Reinhardtius stomias"="Atheresthes stomias",
                        "Anchoa hepsetus & A. mitchilli"="Anchoa hepsetus / Anchoa mitchilli",
                        "Ammodytes americanus & Ammodytes dubius"="Ammodytes americanus / Ammodytes dubius",
                        "Acipenser oxyrhynchus"="Acipenser oxyrinchus",
                        "Penaeus setiferus"="Litopenaeus setiferus",
                        "Micropogonias undulates"="Micropogonias undulatus",
                        "Penaeus aztecus"="Farfantepenaeus aztecus",
                        "Farfantapenaeus aztecus"="Farfantepenaeus aztecus",
                        "Penaeus robustus"="Farfantepenaeus duorarum",
                        "Farfantapenaeus duoarum"="Farfantepenaeus duorarum",
                        "Tetrapturus audax"="Kajikia audax",
                        "Anguilla oceanica"="Conger oceanicus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Dasyatis Sabina"="Dasyatis sabina",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus nigritus"="Hyporthodus nigritus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Euthynneus alletteratus"="Euthynnus alletteratus",
                        "Haelichoeres bivittatum"="Halichoeres bivittatus",
                        "Haemulon plumieri"="Haemulon plumierii",
                        "Melanogrammus aegleinus"="Melanogrammus aeglefinus",
                        "Sebastes dalli"="Sebastes dallii"),
         species=ifelse(comm_name=="Gulf sturgeon", "Acipenser oxyrinchus desotoi", species)) %>% 
  # Format name
  mutate(comm_name=recode(comm_name, 
                          "Scamp"="Scamp grouper",
                          "Tilefish"="Golden tilefish",
                          "Dolphin"="Dolphinfish",
                          "Sand tiger"="Sand tiger shark",
                          "Eastern Bering Sea Pacific cod"="Pacific cod",
                          "Mahimahi"="Dolphinfish",
                          "Mullet"="Striped mullet",
                          "Scalloped hammerhead"="Scalloped hammerhead shark"),
         comm_name=case_when(species=="Anchoa hepsetus / Anchoa mitchilli" ~ "Broad-striped anchovy / Bay anchovy",
                             species=="Squalus suckleyi" ~ "Pacific spiny dogfish",
                             species=="Panulirus penicillatus" ~ "Pronghorn spiny lobster",
                             T ~ comm_name))

# Inspect
freeR::complete(data)
data %>% 
  group_by(region) %>% 
  summarize(dir=sum(is.na(dir_effect)),
            dist=sum(is.na(dist_change)))

# Region
table(data$region)

# Functional group
table(data$functional_group) # could improve

# Scores
table(data$vulnerability)
table(data$exposure)
table(data$sensitivity)
table(data$dist_change)
table(data$dir_effect)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species) # FINE: Oncorhynchus spp, Paralithodes camtschaticus, Sebastes paucispinis, Sebastes pinniger

freeR::check_names(spp_key$species) # Acipenser oxyrinchus desotoi, Nicholsina usta, Palola viridis are both correct


# Plot data
################################################################################

# Totals
stats <- data %>% 
  group_by(region, functional_group, vulnerability) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(region, functional_group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(region=factor(region,
                       levels=c("Northeast", "South Atlantic", "Gulf of Mexico", "Caribbean",  "Marine mammals",
                                "Pacific", "Pacific salmon", "Bering Sea", "Western Pacific")))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot
g <- ggplot(stats, aes(y=functional_group, x=prop, fill=vulnerability)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion of species", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Vulnerability",
                    values=RColorBrewer::brewer.pal(4, "Spectral") %>% rev()) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cva_scores_long.png"), 
       width=6.5, height=8, units="in", dpi=600)

# Export data
################################################################################

# Groups
groups <- c("Northeast", "South Atlantic", "Gulf of Mexico", "Caribbean",  "Marine mammals")

# Plot
g1 <- ggplot(stats %>% filter(region %in% groups), 
             aes(y=functional_group, x=prop, fill=vulnerability)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion of species", y="", title="East Coast CVAs") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Vulnerability",
                    values=RColorBrewer::brewer.pal(4, "Spectral") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot
g2 <- ggplot(stats %>% filter(!region %in% groups), 
             aes(y=functional_group, x=prop, fill=vulnerability)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion of species", y="", title="West Coast CVAs") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Vulnerability",
                    values=RColorBrewer::brewer.pal(4, "Spectral") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.45, 0.55))

# Export
ggsave(g, filename=file.path(plotdir, "cva_scores_wide.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "cva_data.Rds"))

