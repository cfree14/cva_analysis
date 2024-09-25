

# Read data
scores <- readRDS("data/cva/processed/cva_data.Rds")
subscores <- readRDS("data/cva/processed/cva_subscores.Rds")

# Export data
saveRDS(scores, "shiny_app/data/cva_scores.Rds")
saveRDS(subscores, "shiny_app/data/cva_subscores.Rds")
