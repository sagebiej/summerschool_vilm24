# library(tidyverse)
# library(sf)
# library(raster)
# library(terra)


random_points <- spatSample(density_raster, resps, method="weights", replace= TRUE, as.points=TRUE) 


test_po <- project(random_points, vect(germany_sf))
test_po_df <- as.data.frame(test_po, geom="XY")

##### Coordinates data frame with radius #####

test_coords <- test_po_df %>% dplyr::select(lon = "x", lat = "y") %>% rowwise () %>% 
  mutate(radius = 15000 )  #sample(c(15000,2), 1))   # SQ wird für alle leute konstant gehalten.



##############
test_coords$HNV_SQ <- 0
test_coords$PA_SQ <- 0
test_coords$HNV_share <- 0
test_coords$PA_share <- 0

for (i in 1:nrow(test_coords)) {
  radius <- test_coords$radius[i]
  initial_coord <- as.matrix(cbind(test_coords$lon[i], test_coords$lat[i]))


  point <- vect(initial_coord, crs=crs(germany_sf))
  point_buffer <- buffer(point, width=radius)
  point_buffer_vec <- project(point_buffer, hnv)


  # For share calculations
  hnv_y <- crop(hnv, point_buffer_vec)
  pa_y <- crop(protected_areas, point_buffer_vec)
  hnv_y <- mask(hnv_y, point_buffer_vec)
  pa_y <- mask(pa_y, point_buffer_vec)

  # Corine possible change data
  hnv_corine_x <- crop(corine_hnv, point_buffer_vec)
  pa_corine_x <- crop(corine_pa, point_buffer_vec)
  hnv_corine_x <- mask(hnv_corine_x, point_buffer_vec)
  pa_corine_x <- mask(pa_corine_x, point_buffer_vec)

  hnv_y <- ifel(hnv_y==1, 1, NA)
  pa_y <- ifel(pa_y==1, 1, NA)
  hnv_corine_x <- ifel(hnv_corine_x ==1, 1, NA)
  pa_corine_x <- ifel(pa_corine_x ==1, 1, NA)
  
  # use_data$HNV_SQ[i] <- expanse(hnv_y, unit="ha")
  # use_data$PA_SQ[i] <- expanse(pa_y, unit="ha")
  hnv_sum <- sum(table(values(hnv_y)))
  pa_sum <- sum(table(values(pa_y)))
  
  test_coords$HNV_SQ[i] <- hnv_sum
  test_coords$PA_SQ[i] <- pa_sum
  
  test_coords$HNV_share[i] <- round(hnv_sum/(hnv_sum + sum(table(values(hnv_corine_x))))*100, 2)
  test_coords$PA_share[i] <- round(pa_sum/(pa_sum+ sum(table(values(pa_corine_x))))*100, 2)
  
  print(i)

}

final_set <- test_coords %>% ungroup() %>% 
  mutate(ID=1:n()) %>% 
  rowwise() %>% 
      mutate(Split=sample(c(10,15,20,30,40),1))

rm(hnv_y,hnv_sum,pa_corine_x,hnv_corine_x, pa_y,pa_sum, initial_coord, point, point_buffer,point_buffer_vec,random_points,test_coords,test_po_df,test_po,radius)