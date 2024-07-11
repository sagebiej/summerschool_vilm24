library(exactextractr)
library(ggspatial)
library(viridis)
library(sf)
library(terra)
library(tidyverse)

### Summarizing function

sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
  
}

#### 1 Define parameters and functions ####

buffer_dist = 15000 # distance we use in dce simulation 



#### 3 Read in data for Germany with administrative borders ####

#  https://gadm.org/download_country.html #
germany_county <- read_sf("GIS/Data/prepro/gadm41_DEU_shp", "gadm41_DEU_2") # county borders
germany_gemeinde <- read_sf("GIS/Data/prepro/gadm41_DEU_shp", "gadm41_DEU_3") # Gemeinde borders

# Load in HNV raster data (preprocessed)
hnv <- rast("GIS/Data/prepro/hnv_germany.tif")

# Load in Protected areas data (preprocessed)
protected_areas <- readRDS("GIS/Data/prepro/protected_areas.RDS")
protected_areas <- ifel(protected_areas == 0, 0, 1)

# Load in possible areas for change 
corine_hnv <- readRDS("GIS/Data/prepro/corine_hnv.rds")
corine_pa <- readRDS("GIS/Data/prepro/corine_pa.rds")

germany_county <- st_transform(germany_county, crs(corine_hnv))
germany_gemeinde <- st_transform(germany_gemeinde, crs(corine_hnv))

##### Calculate actual and possible HNV share for all counties ######

counties <- germany_county$NAME_2
hnv_list <- list()
hnv_possible_list <- list()

z= 1
for (county in counties) {
  temp_county <- germany_county %>% filter(NAME_2 == counties[z])
  hnv_possible_temp <- as.data.frame(
    exact_extract(corine_hnv, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover)) # caclulate possible area
  hnv_possible_list[[county]] <- hnv_possible_temp
  hnv_temp <- as.data.frame(
    exact_extract(hnv, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover)) # calculate HNV share on total area
  hnv_list[[county]] <- hnv_temp
  print(paste0(n_distinct(germany_county$NAME_2) - z, " remaining"))
  z = z+1
}

hnv_counties <- as.data.frame(sapply(hnv_list, function(x) x$proportion[2]))
hnv_counties <- rownames_to_column(hnv_counties)
hnv_counties$possible <- as.numeric(sapply(hnv_possible_list, function(x) x$proportion[2]))
hnv_counties$area <- as.numeric(sapply(hnv_list, function(x) x$total_area[2]))
colnames(hnv_counties) <- c("County", "HNV_share", "HNV_Possible", "HNV_Area")
hnv_counties <- hnv_counties %>% mutate(HNV_share = replace_na(HNV_share, 0), 
                                                      HNV_on_corine = HNV_share/(HNV_Possible+HNV_share)) 

hnv_counties <- hnv_counties %>% mutate(Total_area = HNV_Area*(1/HNV_share),
                                        Average_HNV_SQ = (HNV_share * buffer_dist^2 * pi)/10000)


###### Calculate WTP based on share for HNV ####

wtp_hnv <- function(x, y) {100*-((y$estimate[["mu_hnv"]] + 2*y$estimate["mu_hnv2"]*x)/y$estimate["mu_cost"])}

hnv_counties <- hnv_counties %>% mutate(WTP_HNV = 100*wtp_hnv(Average_HNV_SQ/100))

###### Create HNV map ####

# Merge county shapes with data frame

germany_county <- germany_county %>% rename("County" = NAME_2)
merge_hnv <- hnv_counties %>%  left_join(germany_county, by="County") %>% 
  select(County, HNV_share, HNV_on_corine, WTP_HNV,  geometry)

# Plot HNV share distribution
ggplot(data=germany_county) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=merge_hnv, aes(fill=HNV_on_corine, geometry=geometry), size=2) +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "HNV Share") +
  scale_fill_gradientn(colours = c("white", "cornsilk1", "peru")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ggsave("Figures/hnv_cont.png", width=6, height=5, dpi="print")


# Plot HNV WTP distribution
ggplot(data=germany_county) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=merge_hnv, aes(fill=WTP_HNV, geometry=geometry), size=2) +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "WTP(€/100ha) HNV") +
  scale_fill_gradientn(colours = c("white", "cornsilk1", "peru")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ggsave("Figures/hnv_wtp.png", width=6, height=5, dpi="print")

## discontinous map ##

# quantile(merge_hnv$WTP_HNV)
# merge_hnv$cuts <- cut(merge_hnv$WTP_HNV, breaks = c(14, 22, 30, 38, 47))
# 
# 
# ggplot(data=germany_county) + 
#   geom_sf(fill="gray98") +
#   geom_sf(data=merge_hnv, aes(fill=cuts, geometry=geometry), size=2) +
#   annotation_scale(location = "bl", width_hint = 0.12,plot_unit="m", line_width = 0.5) +
#   annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   labs(fill= "WTP (€)") +
#   scale_fill_manual(values = c("red3","orange", "gold1", "chartreuse3", "forestgreen"),
#                     labels = c("< -17", " -17 - 0", "0 - 23", "23 - 46", "46 - 64"))
# 
# ggsave("Figures/hnv_disc.png", dpi="print", width=7, height=5)


#### Protected Areas
prot_list <- list()
prot_possible_list <- list()

z= 1
for (county in counties) {
 temp_county <- germany_county %>% filter(County == counties[z])
 prot_corine_temp <- as.data.frame(
   exact_extract(corine_pa, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
 prot_possible_list[[county]] <- prot_corine_temp
 prot_temp <- as.data.frame(
    exact_extract(protected_areas, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  prot_list[[county]] <- prot_temp
  print(paste0(n_distinct(germany_county$County) - z, " remaining"))
  z = z+1
}

prot_counties <- as.data.frame(sapply(prot_list, function(x) x$proportion[2]))
prot_counties <- rownames_to_column(prot_counties)
prot_counties$Prot_Possible <- as.numeric(sapply(prot_possible_list, function(x) x$proportion[2]))
prot_counties$area <- as.numeric(sapply(prot_possible_list, function(x) x$total_area[2]))
colnames(prot_counties) <- c("County", "Prot_share", "Prot_possible", "Prot_area")
prot_counties <- prot_counties %>% mutate(Prot_share = replace_na(Prot_share , 0))
prot_counties <- prot_counties %>% mutate(Prot_share_on_corine = Prot_share/(Prot_possible+Prot_share))


counties_gis <- hnv_counties %>% left_join(prot_counties, by="County")

counties_gis <- counties_gis %>% mutate(Average_Prot_SQ = (Prot_share * buffer_dist^2 * pi)/10000)

germany_county_cl <- germany_county %>% select(County, geometry)

counties_gis <- counties_gis %>% left_join(germany_county_cl, by="County")


saveRDS(counties_gis, "counties_gis.rds")

merge_prot <- prot_counties %>%  left_join(germany_county, by="County") %>% 
  select(County, Prot_share, Prot_share_on_corine, geometry)


# Plot PA share distribution
ggplot(data=germany_county) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=merge_prot, aes(fill=Prot_share_on_corine, geometry=geometry), size=2) +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "PA Share") +
  scale_fill_gradientn(colours = c("cornsilk1", "darkgreen")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

###### Calculate WTP based on share for Protected areas ####

wtp_prot <- function(x) {-(model1$Estimate[[2]] + 2*model1$Estimate[3]*x)/model1$Estimate[6]}

prot_counties <- prot_counties %>% mutate(WTP_Prot = wtp_prot(Prot_share*100))

merge_prot <- prot_counties %>%  left_join(germany_county, by="County") %>% 
  select(County, Prot_share, WTP_Prot, geometry)

ggplot(data=germany_county) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=merge_prot, aes(fill=WTP_Prot, geometry=geometry), size=2) +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "WTP (€)") +
  #scale_fill_viridis(option="C")
  scale_fill_gradientn(colours = c("cornsilk1", "darkgreen")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))


ggsave("Figures/prot_cont.png",width=6, height=5, dpi="print")

# make map for Gemeinde protected

germany_gemeinde <- germany_gemeinde %>% filter(CC_3 != "NA") # remove pure water bodies without CC_3 code 

gemeinden <- germany_gemeinde$NAME_3
gemeinden_code <- germany_gemeinde$CC_3 # use unique code since names are doubling for some gemeinden 
prot_list_g <- list()
prot_list_g_po <- list()

z= 1
for (gemeinde in gemeinden) {
  temp_county <- germany_gemeinde %>% filter(CC_3 == gemeinden_code[z])
  prot_corine_temp <- as.data.frame(
    exact_extract(corine_pa, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  prot_list_g_po[[gemeinde]] <- prot_corine_temp
  prot_temp <- as.data.frame(
    exact_extract(protected_areas, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  prot_list_g[[gemeinde]] <- prot_temp
  print(paste0(n_distinct(germany_gemeinde$CC_3) - z, " remaining"))
  z = z+1
}

prot_gemeinde <- as.data.frame(sapply(prot_list_g, function(x) x$proportion[2]))
prot_gemeinde <- rownames_to_column(prot_gemeinde)
prot_gemeinde$Prot_corine <- as.numeric(sapply(prot_list_g_po, function(x) x$proportion[2]))
colnames(prot_gemeinde) <- c("Gemeinde", "Prot_share", "Prot_corine")
prot_gemeinde <- prot_gemeinde %>% mutate(Prot_share = replace_na(Prot_share , 0))

prot_gemeinde <- prot_gemeinde %>% mutate(Prot_on_corine = Prot_share/(Prot_share + Prot_corine))

prot_gemeinde <- prot_gemeinde %>% mutate(WTP_Prot = wtp_prot(Prot_share*100))

germany_gemeinde <- germany_gemeinde %>% rename("Gemeinde" = NAME_3)

merge_prot_g <- prot_gemeinde %>%  left_join(germany_gemeinde, by="Gemeinde") %>% 
  select(Gemeinde, Prot_share, Prot_on_corine, geometry)

ggplot(data=germany_gemeinde) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=merge_prot_g, aes(fill=Prot_on_corine, geometry=geometry), size=2, col="NA") +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "PA Share") +
  scale_fill_gradientn(colours = c("cornsilk", "darkgreen")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

#ggsave("Figures/prot_g_cont.png", dpi="print", width=6, height=5)


### Do plot for protected shares gemeinde ###

ggplot(data=merge_prot_g) +
  geom_density(aes(x=Prot_share*100), col="darkseagreen", fill="darkseagreen", alpha=0.5) +
  xlab("Share of Protected Areas (%)") +
  ggtitle("Overview Protected Area Shares Gemeinden")

ggsave("Figures/prot_share_g.png", width = 7, height = 5, dpi="print")


# Do plot for HNV Share counties
ggplot(data=merge_hnv) +
  geom_density(aes(x=HNV_share*100), col="royalblue", fill="royalblue", alpha=0.5) +
  xlab("HNV Share (%)") +
  ggtitle("Overview HNV Shares Counties")

ggsave("Figures/hnv_share.png", width = 7, height = 5, dpi="print")


### 7 Raster values WTP #####

##### Scaling ####

scale_factor <- pi*(buffer_dist/1000)^2*100 # Calculate total area that is valued in the DCE as scaling factor

###### Raster HNV (need to work on this further) ####
#inefficient as hell 

agg_hnv <- aggregate(hnv, fact=50, fun=mean)

####### HERE ################################################################################################################################
grid <- rast(agg_hnv, vals = values(agg_hnv)) # empty raster

grid_points <- as.points(grid, values=TRUE, na.rm=TRUE, na.all=FALSE)

grid_points_gg <- as.data.frame(grid, xy=TRUE)



buffer_pts <- buffer(grid_points, buffer_dist)

# Compute people within Buffer 
hnv_in_buffer <- terra::extract(agg_hnv, buffer_pts, fun= sum, na.rm =T, weights = TRUE) 

hnv_cells <- hnv_in_buffer

hnv_cells$`eea_r_3035_100_m_hnv-farmland-ac_2012_v1_r00` <- hnv_cells$`eea_r_3035_100_m_hnv-farmland-ac_2012_v1_r00`*((buffer_dist/1000)^2*pi/5^2)*100



grid_points_gg <- rowid_to_column(grid_points_gg, var = "ID")

grid_merge <- left_join(grid_points_gg, hnv_cells, by="ID")

colnames(grid_merge) <- c("ID", "x", "y", "HNV_Share", "HNV Area within 15km")


grid_merge <- grid_merge %>% mutate(WTP_HNV = wtp_hnv(`HNV Area within 15km`/100, model_gp))

ggplot(data=grid_merge) +
  geom_tile(aes(x = x, y = y, fill = WTP_HNV)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_gradientn(colours = c("white", "cornsilk1", "peru")) 



density_raster <- rast("GIS/data/BBSR_Landnutzung_Bevoelkerung/ewz250_2011_v1_multi.tif")


agg_dens <- terra::aggregate(density_raster, fact=20, fun=sum, na.rm=TRUE)

dens_gg <- as.data.frame(agg_dens, xy=TRUE)
colnames(dens_gg) <- c("x", "y", "Density")


ggplot(data=dens_gg) +
  geom_tile(aes(x = x, y = y, fill = Density)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis(option="C", direction = 1)

new_raster <- rast(ncols=ncol(agg_dens), nrows=nrow(agg_dens), 
                   xmin=xmin(agg_dens), xmax=xmax(agg_dens), 
                   ymin=ymin(agg_dens), ymax=ymax(agg_dens), 
                   crs=crs(agg_dens))

# Convert the data frame to a SpatVector
points <- vect(grid_merge, geom=c("x", "y"), crs=crs(agg_dens))

# Rasterize the points using HNV_Share as the values
wtp_raster <- rasterize(points, new_raster, field="WTP_HNV", fun="mean")

wtp_raster_agg <- wtp_raster * agg_dens

writeRaster(wtp_raster_agg, "hnv_wtp.tif")

library(tidyterra)
library(ggpubr)
wtp_hnv_plot <- ggplot() +
  geom_spatraster(data = wtp_raster_agg/1000000) +
  scale_fill_viridis_c(option = "F") +
  labs(fill="Agg. WTP (Million€/100ha) HNV")


pop_dense_plot <- ggplot() +
  geom_spatraster(data = agg_dens) +
  scale_fill_viridis_c(option = "F") +
  labs(fill="Population Density")

ggarrange(wtp_hnv_plot, pop_dense_plot, legend = "bottom")


wtp_hnv_gg <- as.data.frame(wtp_raster_agg, xy=TRUE)

wtp_hnv_gg$mean <- wtp_hnv_gg$mean/1000000

wtp_hnv_gg$cuts <- cut(wtp_hnv_gg$mean, breaks = c(-2, 0, 0.5, 1, 2, 5, 8, 12, 20, 30, 50))

ggplot(data=wtp_hnv_gg) +
  geom_tile(aes(x = x, y = y, fill = cuts)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis(option = "F", discrete=TRUE, 
                     labels=c("< 0", "0 - 0.5", "0.5 - 1", "1 - 2", "2 - 5", "5 - 8", "8 - 12",
                              "12 - 20", "20 - 30", "> 30")) +
  xlab("") +
  ylab("") +
  labs(fill ="Aggregated WTP \n(Mio. €/100ha) HNV") +
  theme(legend.position = "right")

ggsave("Figures/agg_wtp.png", width=12, height = 10, dpi=1000)
############################################################################################################################################

# try something, check this 
agg_hnv_wtp <- wtp_hnv(check_list*scale_factor, model_gp)

values(agg_hnv) <- agg_hnv_wtp/scale_factor



agg_hnv_gg <- as.data.frame(agg_hnv, xy=TRUE)
colnames(agg_hnv_gg) <- c("x", "y", "WTP")

# somehow raster only works sometimes, tile works but looks not as nice
ggplot(data=agg_hnv_gg) +
  geom_tile(aes(x = x, y = y, fill = WTP)) +
  coord_quickmap() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  #scale_fill_viridis(option="C", direction = 1)
  scale_fill_gradientn(colours = c("gold", "cornsilk1", "royalblue3")) 

ggsave("Figures/hnv_wtp_raster.png", width=6, height=5, dpi="print")

###### Raster protected ####

agg_prot <- aggregate(protected_areas, fact=10, fun=mean)

agg_prot <- mask(agg_prot, germany_county)

values_vec <- c(unlist(values(agg_prot)))

prot_wtp <- -(model1$Estimate[[2]] + 2*model1$Estimate[3]*values_vec*100)/model1$Estimate[6]

values(agg_prot) <- prot_wtp


agg_prot_gg <- as.data.frame(agg_prot, xy=TRUE)
colnames(agg_prot_gg) <- c("x", "y", "WTP")

# Plot protected
ggplot(data=agg_prot_gg) +
  geom_raster(aes(x = x, y = y, fill = WTP)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  #scale_fill_viridis(option="C", direction = 1)
  scale_fill_gradientn(colours = c("cornsilk", "white", "darkseagreen", "darkgreen")) 

ggsave("Figures/prot_wtp_raster.png", width=6, height=5, dpi="print")

#### 8 Load in Pop density data ####

density_raster <- rast("data/gis/BBSR_Landnutzung_Bevoelkerung/ewz250_2011_v1_multi.tif")

dens_rast_1000 <- aggregate(density_raster, fact=4, fun=sum)

dens_gg <- as.data.frame(dens_rast_1000, xy=TRUE)
colnames(dens_gg) <- c("x", "y", "Density")



ggplot(data=dens_gg) +
  geom_tile(aes(x = x, y = y, fill = Density)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis(option="C", direction = 1)


#### Calculate people within circle for cell centroids, a lot of work to do here
library(raster)

# subset for testing code

#subset_extent <- ext(4330000, 4380000, 2780000, 2830000)

#hnv_sub <- crop(agg_hnv, subset_extent)
hnv_sub <- aggregate(hnv, fact=50, fun=mean) # 5km x 5km cells

hnv_sub_gg <- as.data.frame(hnv_sub, xy=TRUE)
hnv_sub_gg <- rowid_to_column(hnv_sub_gg, var = "ID")
hnv_to_merge <- hnv_sub_gg %>% dplyr::select(ID, "HNV_share" = "eea_r_3035_100_m_hnv-farmland-ac_2012_v1_r00")

#### 9 Calculate WTP per cell ####
check_list <- c(unlist(values(hnv_sub)))

agg_hnv_wtp <- -(model1$Estimate[[4]] + 2*model1$Estimate[5]*(check_list*100))/model1$Estimate[6]

values(hnv_sub) <- agg_hnv_wtp/scale_factor

# transform density raster to same resolution #
dens_sub <- aggregate(density_raster, fact=20, fun=sum) # 5km x 5km cells

grid <- rast(hnv_sub, vals = values(hnv_sub)) # empty raster

grid_points <- as.points(grid, values=TRUE, na.rm=TRUE, na.all=FALSE)

grid_points_gg <- as.data.frame(grid, xy=TRUE)


gridPoints <- SpatialPoints(grid_points_gg)

vec_gP <- vect(gridPoints)
crs(vec_gP) <- crs(dens_sub)


buffer_pts <- buffer(vec_gP, buffer_dist)

# Compute people within Buffer 
density_in_buffer <- extract(dens_sub, buffer_pts, fun= sum, na.rm =T, weights = TRUE) 

# Check if this is really correct with the ID

grid_points_gg <- rowid_to_column(grid_points_gg, var = "ID")
                                  
grid_merge <- left_join(grid_points_gg, density_in_buffer, by="ID")

colnames(grid_merge) <- c("ID", "x", "y", "marg_WTP", "Density")

grid_merge <- left_join(grid_merge, hnv_to_merge, by="ID")

## calculate WTP for 1ha increase, produces infinity values if SQ = 0 ! ##
#### work on here ### 1% increase is always the same area actually not depended on SQ with the current design
#need to define this clearly for the DCE #
# For now assumed that we value 1% increase of HNV on the total land area which is always the same area
# Aggregated WTP is the Aggregated WTP for a one hectare increase of HNV in the raster cell
grid_merge <- grid_merge %>% mutate(SQ_HNV = (HNV_share * unique(res(hnv_sub))^2)/10000,
                                    agg_WTP = marg_WTP*Density/(((unique(res(hnv_sub))^2)/10000)*0.01))

###
ggplot(data=grid_merge) +
  geom_tile(aes(x = x, y = y, fill = agg_WTP)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_gradientn(colours = c("white", "cornsilk1", "peru")) 

ggsave("Figures/agg_wtp_example.png", dpi="print", width = 7, height = 5)


# discontinuous map with cuts 
grid_merge$cuts  <- cut(grid_merge$agg_WTP, c(-1000, 0, 100, 200, 350, 1000, 3000, 5000, 8600))



ggplot(data=grid_merge) +
  geom_tile(aes(x = x, y = y, fill = cuts)) +
  coord_equal() +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill="Aggregated WTP (€/ha)") +
  xlab("") +
  ylab("") +
  scale_fill_viridis(option="C", direction = 1, discrete = T, 
                     labels=c("< 0", "0 - 100", "100 - 200", "200 - 350", "350 - 1,000", "1,000 - 3,000", "3,000 - 5,000",
                              " > 5,000")) 

ggsave("Figures/agg_wtp_hnv_ex_disc.png", dpi="print", width=6, height = 5)





