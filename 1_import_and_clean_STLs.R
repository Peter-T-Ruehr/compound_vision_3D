## ---------------------------
##
## PURPOSE OF SCRIPT: 
##      Importing and cleaning of STL files of compound eyes
##      
##
## AUTHOR:
##      Peter T. Rühr
##
## DATE CREATED:
##      2022-04-07
##
## Copyright (c) Peter T. Rühr, 2022
## Email: peter.ruehr@gmail.com
##
## ---------------------------
##
## NOTES:
##      
##
## ---------------------------
## INPUTS:
##
## OUTPUTS
##
##  -------------------------------------------------------------------------
##  DEPENDENCIES

# load tidyverse for its various conveniences
library(tidyverse)

# compound vision analysis
library(compoundvision3D)

# 3D plotting
library(rgl)

# fast csv wrighting and reading
library(readr)

# 

## -------------------------------------------------------------------------
## General variable definitions

## -------------------------------------------------------------------------
## Input and output variable definitions
# define STL file to import
stl_file_name <- "//blanke-nas-1/DATA/RAWDATA/Hexapoda/3_Polyneoptera/Dermaptera/7079_F_auricularia_Dermaptera_reduced/7079_xxx_head/checkpoint_surface_export/eye_analysis_test_2.stl"


# -------------------------------------------------------------------------
# reading external data

# -------------------------------------------------------------------------
# data loading
# convert STL to tibble with triangle center coordinates and their normals
tri_centers_normals <- STL_2_tibble(stl_file_name)

# plot for checking
plot3d(tri_centers_normals$x, tri_centers_normals$y, tri_centers_normals$z, 
       aspect = "iso", col = "blue")

# load search diameters
# xxx: for final pub, put this in csv file
search_diams <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1LH5ImpeIKUKZau7GzUNKhgtqqWvEc4jyZJVIjfmBMfM/edit#gid=0")

# -------------------------------------------------------------------------
# data cleaning 



# -------------------------------------------------------------------------
# data processing
# find search diameter interactively (only run once - then stored in sheet)
if(basename(stl_file_name) %in% search_diams$STL == FALSE){
  print("Search diameter needs to be defined.")
  search_diam <- search_diam_interactive(df = tri_centers_normals)
} else {
  search_diam <- search_diams %>% 
    filter(STL == basename(stl_file_name)) %>% 
    pull(search_diameter)
  print(paste0("Search diameter already defined: ", search_diam))
}

# save triangle centers and their normals in new (possibly reduced) tibble for 
#   tri_centers_normals_useing.
# Comment [..., ] out if no reduction is desired.
tri_centers_normals_use <- tri_centers_normals[1:10000,]

# Calculate distances of vertices from local plane. This is multi-threaded
#   but may still take a while.
tri_centers_normals_use <- get_local_height(df = tri_centers_normals_use,
                                            search_diam = search_diam,
                                            cores = 12)

# plot eye in 'SEM colours'
plot3d(tri_centers_normals_use[, 2:4], 
       col = tri_centers_normals_use$local_height_cols, 
       aspect = "iso")

rough_peaks <- find_facet_peaks_rough(df = tri_centers_normals_use,
                                      clust_melt_rad = 1.5*round(1/8*search_diam,2),
                                      iterations = 1,
                                      cores = 12)
# plot rough peaks in 'SEM colours'
plot3d(rough_peaks[, ((ncol(rough_peaks)-2):ncol(rough_peaks))], 
       col = rough_peaks$local_height_cols, 
       aspect = "iso")


fine_peaks <- find_facet_peaks_fine(df = rough_peaks,
                      cols_to_use = (ncol(rough_peaks)-2):ncol(rough_peaks))


# -------------------------------------------------------------------------
# output section

write_csv(fine_peaks, paste0("./data/", gsub("\\.stl", "", basename(stl_file_name)), "_fine_peaks.csv"))

# ---------------------

