
# Plot subscores
# data <- subscores; region <- "Bering Sea"
plot_subscores <- function(data, region){
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=10),
                     axis.title=element_text(size=11),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=11),
                     strip.text=element_text(size=11),
                     plot.title=element_text(size=12),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Exposure
  ##############################
  
  # Prepare data
  region_do <- region
  data1 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Exposure")
  
  # Species stats
  stats_spp1 <- data1 %>% 
    group_by(system, comm_name) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(system, desc(score_avg))
  
  # Attribute stats
  stats_att1 <- data1 %>% 
    group_by(attribute) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(desc(score_avg))
  
  # Longest x-axis label
  nchar_max <- data %>% 
    filter(region==region_do) %>% 
    pull(attribute) %>% unique() %>% nchar() %>% max()
    
  # Order data
  data_ordered1 <- data1 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp1$comm_name),
           attribute=factor(attribute, levels=stats_att1$attribute)) %>% 
    mutate(attribute=stringr::str_pad(attribute, width=nchar_max, side="left", pad=" "))
  
  # Plot exposure
  g1 <- ggplot(data_ordered1,
         aes(x=attribute, y=comm_name, fill=score_avg)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="A", title = "Exposure subscores") +
    # Legend
    scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() + my_theme +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  # Sensitivity
  ##############################
  
  # Prepare data
  region_do <- region
  data2 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Sensitivity")
  
  # Species stats
  stats_spp2 <- data2 %>% 
    group_by(system, comm_name) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(system, desc(score_avg))
  
  # Attribute stats
  stats_att2 <- data2 %>% 
    group_by(attribute) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(desc(score_avg))
  
  # Order data
  data_ordered2 <- data2 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp2$comm_name),
           attribute=factor(attribute, levels=stats_att2$attribute)) %>% 
    mutate(attribute=stringr::str_pad(attribute, width=nchar_max, side="left", pad=" "))
  
  # Plot exposure
  g2 <- ggplot(data_ordered2,
         aes(x=attribute, y=comm_name, fill=score_avg)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="B", title = "Sensitivity subscores") +
    # Legend
    scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() + my_theme +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  # Merge
  ##############################
  
  g <- gridExtra::grid.arrange(g1, g2, nrow=1)
  
}

# Plot scores
# data <- scores; region <- "Bering Sea"
plot_scores <- function(data, region){
  
  # Build data
  region_do <- region
  rdata <- data %>% 
    filter(region==region_do)
  
  # Vulnerability data
  rdata1 <- rdata %>% 
    # Simplify
    select(region, functional_group, comm_name, exposure, sensitivity, vulnerability, dist_change) %>% 
    # Gather
    gather(key="metric", value="score", 4:ncol(.)) %>% 
    # Format
    mutate(metric=stringr::str_to_sentence(metric),
           metric=recode(metric, "Dist_change"="Propensity for\ndistribution shift"), 
           metric=factor(metric, levels=c("Exposure", "Sensitivity", "Vulnerability", "Propensity for\ndistribution shift")),
           score=factor(score, levels=c("Low", "Moderate", "High", "Very high")))
  
  # Order data
  stats <- rdata1 %>% 
    spread(key="metric", value="score") %>% 
    arrange(functional_group, desc(Vulnerability), desc(Sensitivity), desc(Exposure))
  rdata1_ordered <- rdata1 %>% 
    mutate(comm_name=factor(comm_name, levels=stats$comm_name))
  
  # Direction data
  rdata2 <- rdata %>% 
    # Simplify
    select(region, functional_group, comm_name, exposure, sensitivity, vulnerability, dir_effect) %>% 
    # Order
    mutate(comm_name=factor(comm_name, levels=stats$comm_name))
  
  # Plot data
  ggplot(rdata1_ordered, aes(y=comm_name, x=metric, fill=score)) +
    facet_grid(functional_group~., scales="free_y", space="free_y") +
    geom_raster() +
    # Plot direction
    geom_point(data=rdata2, mapping=aes(x=factor("Vulnerability", levels=levels(rdata1$metric)), 
                                        y=comm_name, 
                                        shape=dir_effect, 
                                        color=dir_effect), inherit.aes = F, size=2) +
    # Labels
    labs(x="", y="") +
    # Legend
    # scale_fill_ordinal(name="Score", drop=F) +    
    scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(4, "Spectral") %>% rev()) +
    scale_shape_manual(name="Directional effect", values=c("–", "0", "+"), drop=F) +
    scale_color_manual(name="Directional effect", values=c("darkred", "black", "navy"), drop=F) +
    # Theme
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
}


# Plot data quality
# data <- subscores; region <- "Bering Sea"
plot_subscore_quality <- function(data, region){
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=10),
                     axis.title=element_text(size=11),
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=11),
                     strip.text=element_text(size=11),
                     plot.title=element_text(size=12),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Exposure
  ##############################
  
  # Longest x-axis label
  nchar_max <- data %>% 
    filter(region==region_do) %>% 
    pull(attribute) %>% unique() %>% nchar() %>% max()
  
  # Prepare data
  region_do <- region
  data1 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Exposure")
  
  # Species stats
  stats_spp1 <- data1 %>% 
    group_by(system, comm_name) %>% 
    summarize(data_quality_avg=mean(data_quality, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(system, desc(data_quality_avg))
  
  # Attribute stats
  stats_att1 <- data1 %>% 
    group_by(attribute) %>% 
    summarize(data_quality_avg=mean(data_quality, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(desc(data_quality_avg))
  
  # Order data
  data_ordered1 <- data1 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp1$comm_name),
           attribute=factor(attribute, levels=stats_att1$attribute)) %>% 
    mutate(attribute=stringr::str_pad(attribute, width=nchar_max, side="left", pad=" "))
  
  # Plot exposure
  g1 <- ggplot(data_ordered1,
               aes(x=attribute, y=comm_name, fill=data_quality)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="A", title = "Exposure subscore data quality") +
    # Legend
    scale_fill_gradientn(name="Data quality", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() + my_theme +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  g1
  
  # Sensitivity
  ##############################
  
  # Prepare data
  region_do <- region
  data2 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Sensitivity")
  
  # Species stats
  stats_spp2 <- data2 %>% 
    group_by(system, comm_name) %>% 
    summarize(data_quality_avg=mean(data_quality)) %>% 
    ungroup() %>% 
    arrange(system, desc(data_quality_avg))
  
  # Attribute stats
  stats_att2 <- data2 %>% 
    group_by(attribute) %>% 
    summarize(data_quality_avg=mean(data_quality)) %>% 
    ungroup() %>% 
    arrange(desc(data_quality_avg))
  
  # Order data
  data_ordered2 <- data2 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp2$comm_name),
           attribute=factor(attribute, levels=stats_att2$attribute)) %>% 
    mutate(attribute=stringr::str_pad(attribute, width=nchar_max, side="left", pad=" "))
  
  # Plot exposure
  g2 <- ggplot(data_ordered2,
               aes(x=attribute, y=comm_name, fill=data_quality)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="B", title = "Sensitivity subscore data quality") +
    # Legend
    scale_fill_gradientn(name="Data quality", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() + my_theme +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  g2
  
  # Merge
  ##############################
  
  g <- gridExtra::grid.arrange(g1, g2, nrow=1)
  
}

# Plot exposure subscores
# region <- "Bering Sea"
plot_subscores_exposure <- function(data, region){
  
  # Prepare data
  region_do <- region
  data1 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Exposure")
  
  # Species stats
  stats_spp1 <- data1 %>% 
    group_by(system, comm_name) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(system, desc(score_avg))
  
  # Attribute stats
  stats_att1 <- data1 %>% 
    group_by(attribute) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(desc(score_avg))
  
  # Order data
  data_ordered1 <- data1 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp1$comm_name),
           attribute=factor(attribute, levels=stats_att1$attribute))
  
  # Plot exposure
  ggplot(data_ordered1,
         aes(x=attribute, y=comm_name, fill=score_avg)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="A", title = "Exposure scores") +
    # Legend
    scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
}


# Plot sensitivity subscores
# region <- "Bering Sea"
plot_subscores_sensitivity <- function(data, region){
  
  # Prepare data
  region_do <- region
  data1 <- data %>% 
    # Filter to region of interest
    filter(region==region_do & metric=="Sensitivity")
  
  # Species stats
  stats_spp1 <- data1 %>% 
    group_by(system, comm_name) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(system, desc(score_avg))
  
  # Attribute stats
  stats_att1 <- data1 %>% 
    group_by(attribute) %>% 
    summarize(score_avg=mean(score_avg)) %>% 
    ungroup() %>% 
    arrange(desc(score_avg))
  
  # Order data
  data_ordered1 <- data1 %>% 
    mutate(comm_name=factor(comm_name, levels=stats_spp1$comm_name),
           attribute=factor(attribute, levels=stats_att1$attribute))
  
  # Plot exposure
  ggplot(data_ordered1,
         aes(x=attribute, y=comm_name, fill=score_avg)) +
    facet_grid(system~., space="free_y", scales="free_y", 
               labeller = label_wrap_gen(width=10, multi_line = T)) +
    geom_raster() +
    # Labels
    labs(x="", y="", tag="B", title = "Sensitivity scores") +
    # Legend
    scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value = "white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Theme
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
}

# 
# # Sensitivity
# #####################################
# 
# # Prepare data
# data2 <- data %>% 
#   # Filter to region of interest
#   filter(region==region_do & metric=="Sensitivity")
# 
# # Species stats
# table(data2$component_score)
# stats_spp2 <- data2 %>% 
#   group_by(system, comm_name) %>% 
#   summarize(mean=mean(mean),
#             score=unique(component_score)) %>% 
#   ungroup() %>% 
#   arrange(system, desc(mean)) %>% 
#   mutate(score=factor(score, levels=c("Low", "Moderate", "High", "Very High")))
# 
# # Attribute stats
# stats_att2 <- data2 %>% 
#   group_by(attribute) %>% 
#   summarize(mean=mean(mean)) %>% 
#   ungroup() %>% 
#   arrange(desc(mean))
# 
# # Order data
# data_ordered2 <- data2 %>% 
#   mutate(comm_name=factor(comm_name, levels=stats_spp2$comm_name),
#          attribute=factor(attribute, levels=stats_att2$attribute))
# 
# # Plot exposure
# ggplot(data_ordered2,
#        aes(x=attribute, y=comm_name, fill=mean)) +
#   facet_grid(system~., space="free_y", scales="free_y", 
#              labeller = label_wrap_gen(width=10, multi_line = T)) +
#   geom_raster() +
#   # Labels
#   labs(x="", y="", tag="B", title = "Sensitivity scores") +
#   # Legend
#   scale_fill_gradientn(name="Score", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
#   # Theme
#   theme_bw() +
#   theme(strip.text.y = element_text(angle = 0), 
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# 
