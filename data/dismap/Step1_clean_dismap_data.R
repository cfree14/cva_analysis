# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/dismap/raw"
outdir <- "data/dismap/processed"
plotdir <- "data/dismap/figures"

# https://apps-st.fisheries.noaa.gov/dismap/
# https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html


# Build data
################################################################################

# Files to read
files2read <- list.files(indir)

# Loop through files and read
x <- files2read[1]
data_orig <- purrr::map_df(files2read, function(x){
  
  # Read region
  region <- read.csv(file.path(indir, x), nrows=1, header=F)[1, "V1"] %>% 
    gsub("Dataset: ", "", .) %>% stringr::str_squish()
  
  # Read data
  fdata <- read.csv(file.path(indir, x), skip=2, na.strings="") %>% 
    mutate(region=region) %>% 
    select(region, everything())
  
})

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(species=species_scientific_name,
         comm_name=common_name,
         cog_lat1=avg_cog_lat_start,
         cog_lat2=avg_cog_lat_end,
         cog_lat_diff=diff_in_cog_lat,
         cog_lat_pdiff=x_change_in_cog_lat,
         cog_depth1=avg_cog_depth_start,
         cog_depth2=avg_cog_depth_end,
         cog_depth_diff=diff_in_cog_depth,
         cog_depth_pdiff=x_change_in_cog_depth) %>% 
  # Update scientific names
  # All of the commented out species names are correct in WORMS
  mutate(species=recode(species, 
                        # "Achelous gibbesii"                        
                        # "Achelous spinicarpus"                     
                        # "Actinauge verrillii"                      
                        # "Actinostola faeculenta"                   
                        # "Anchoa spp."                              
                        # "Ancylopsetta quadrocellata"               
                        # "Arctoraja parmifera"                      
                        "Atheresthes stomias and A. evermanni"= "Atheresthes stomias, Atheresthes evermanni",
                        # "Bathyraja spp."                           
                        # "Beringraja binoculata"                    
                        # "Beringraja inornata"                      
                        # "Beringraja rhina"                         
                        # "Buccinum polare"                          
                        # "Clupea pallasii"                          
                        # "Crossaster borealis"                      
                        "Doryteuthis sp"="Doryteuthis sp.",                           
                        # "Echidnocerus foraminatus"                 
                        # "Eques lanceolatus"                        
                        # "Eucinostomus spp."                        
                        # "Gadus chalcogrammus"                      
                        # "Gibbesia neglecta"                        
                        "Halieutichthys sp"="Halieutichthys sp.",                      
                        "Hippoglossoides elassodon and H. robustus"="Hippoglossoides elassodon, Hippoglossoides robustus",
                        # "Hypanus sabinus"                          
                        # "Hypanus say"                              
                        # "Lepidopsetta sp."                         
                        # "Leptasterias (Hexasterias) polaris"       
                        # "Leucoraja erinaceus"                      
                        # "Liponema brevicorne"                      
                        # "Microstomus bathybius"                    
                        # "Moreiradromia antillensis"                
                        # "Mycale (Mycale) loveni"                   
                        # "Myzopsetta ferruginea"                    
                        # "Myzopsetta proboscidea"                   
                        # "Nearchaster (Nearchaster) aciculosus"     
                        # "Neptunea borealis"                        
                        # "Nicholsina usta"                          
                        # "Ophiura sarsii"                           
                        # "Pandalus dispar"                          
                        # "Penaeus aztecus"                          
                        # "Penaeus duorarum"                         
                        # "Penaeus setiferus"                        
                        # "Plicifusus kroyeri"                       
                        # "Rostroraja eglanteria"                    
                        # "Rostroraja texana"                        
                        "Sebastes melanostictus and S. aleutianus"="Sebastes melanostictus, Sebastes aleutianus",
                        "Sebastes variabilis and S. ciliatus"="Sebastes variabilis, Sebastes ciliatus"
                        # "Stegophiura ponderosa"                    
                        # "Stephanolepis hispida"                    
                        # "Trachinocephalus myops"                   
                        # "Tritonia tetraquetra"
                        )) %>% 
  # Add missing common names
  mutate(comm_name=case_when(species=="Bathybembix bairdii" ~ "Baird's top shell",
                             species=="Chrysaora melanaster" ~ "Northern sea nettle",
                             species=="Myxoderma platyacanthum" ~ "Myxoderma sea star spp.",
                             species=="Neptunea borealis" ~ "Neptunea sea snail spp.",
                             species=="Pleurobranchaea californica" ~ "Deep water sea slug spp.",
                             species=="Stegophiura ponderosa" ~ "Stegophiura brittle star spp.",
                             species=="Thrissacanthias penicillatus" ~ "Thrissacanthias sea star spp.",
                             T ~ comm_name)) %>% 
  # Add council
  mutate(council=recode(region,
                        "Eastern and Northern Bering Sea"="North Pacific",
                        "Eastern Bering Sea"="North Pacific",
                        "Gulf of Alaska"="North Pacific",                  
                        "Gulf of Mexico"="Gulf of Mexico",                 
                        "Hawai'i Islands"="Western Pacific",                 
                        "Northeast US Fall"="New England/Mid-Atlantic",            
                        "Northeast US Spring"="New England/Mid-Atlantic",              
                        "Northern Bering Sea"="North Pacific",      
                        "Southeast US Fall"="South Atlantic",    
                        "Southeast US Spring"="South Atlantic",
                        "Southeast US Summer"="South Atlantic",
                        "West Coast Annual"="Pacific",
                        "West Coast Triennial"="Pacific")) %>% 
  # Arrange
  select(council, region, comm_name, species, everything())

# Inspect
str(data)
freeR::complete(data)

# Region
sort(unique(data$region))

# Species
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)
freeR::check_names(spp_key$species)

# Sample size
sample_n <- data %>% 
  count(council, region)

# Plot
ggplot(sample_n, aes(y=reorder(region, desc(n)), x=n, fill=council)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of species", y="") +
  # Theme
  theme_bw() +
  theme(legend.position = "none")


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "dismap_data.Rds"))


# Plot data
################################################################################

# Format data for plotting
data_plot <- data %>% 
  # Order councils
  mutate(council=recode_factor(council,
                               "New England/Mid-Atlantic"="New England/\nMid-Atlantic",
                               "South Atlantic"="South\nAtlantic",
                               "Gulf of Mexico"="Gulf of\nMexico",
                               "Pacific"="Pacific",
                               "North Pacific"="North\nPacific",
                               "Western Pacific"="Western\nPacific")) %>% 
  # Gather
  select(council, region, cog_lat_diff, cog_depth_diff) %>% 
  gather(key="metric", value="value", 3:ncol(.)) %>% 
  mutate(metric=recode_factor(metric,
                              "cog_lat_diff"="Latitude (°N)",
                              "cog_depth_diff"="Depth (m)"))

# Plot data
g <- ggplot(data_plot, aes(y=region, x=value)) +
  facet_grid(council~metric, scales="free", space="free_y") +
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept = 0, color="grey30", linetype="dashed") +
  # Labels
  labs(x="Change in metric", y="") +
  # Theme
  theme_bw()

# Export figure
ggsave(g, filename=file.path(plotdir, "dismap_changes_by_survey.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



