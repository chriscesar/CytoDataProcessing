# data2WB.R ####
# import data and assign values to WFD water bodies

## load packages ####
ld_pkgs <- c("tidyverse", "tictoc","sf","maps","nngeo")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

tictoc::tic.clearlog()
#Load data####
tic("Load data")
source("R/metadata.R")

df0 <- readxl::read_xlsx(
  "data_in/VALmerge_VAL_clusSumTotal_241031_154709-with_SitenameEastNorthDepth.xlsx",
  sheet = "VALmerge_VAL_clusSumTotal_24103")

base_WBs <- sf::read_sf(paste0(GISfol,
                               "C3_WFD_Waterbody/",
                               "EnglandTRAC_C3_ReducedFields.shp"))
toc(log = TRUE)

# Join ####
tic("Join")
## remove rows with missing geo data
df0 %>%
  filter(.,!Eastings == "NaN") %>%
  filter(.,!Eastings == "0") %>%
  mutate(.,
         Eastings = as.numeric(Eastings),
         Northings = as.numeric(Northings)) -> df0_trim

# Convert df0_trim to an sf object (assuming Eastings and Northings as coordinates)
df0_trim_sf <- st_as_sf(df0_trim, coords = c("Eastings", "Northings"), crs = 27700)

# Ensure both objects are in the same CRS
if (!st_crs(df0_trim_sf) == st_crs(base_WBs)) {
  stop("CRS mismatch between df0_trim and base_WBs")
  }

# Calculate distances from each point to all polygons
distances <- st_distance(df0_trim_sf, base_WBs)

# Find the index of the nearest polygon for each point
nearest_idx <- apply(distances, 1, which.min)

# Extract the nearest polygons and their attributes
nearest_polygons <- base_WBs[nearest_idx, ]

# Add the nearest polygon attributes and distance to the points
df0_trim_with_nearest <- df0_trim_sf %>%
  mutate(
    # replace 'id' with the identifier column in base_WBs
    nearest_polygon_id = nearest_polygons$OBJECTID,  
    distance_to_nearest = as.numeric(apply(distances, 1, min))
    ) %>%
  cbind(st_drop_geometry(nearest_polygons))
toc(log=TRUE)

# Write data ####
tic("Write data")
write.csv(
  df0_trim_with_nearest,
  file = "data_out/joined_data.csv",
  row.names = FALSE)
df <- df0_trim_with_nearest

toc(log = TRUE)

unlist(tictoc::tic.log())

# tidy up ####
rm(base_WBs,df0,df0_trim, df0_trim_sf, df0_trim_with_nearest, nearest_polygons,
   distances, nearest_idx)
