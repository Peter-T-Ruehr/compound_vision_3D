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


# data cleaning ----------------------------------------------------------



# -------------------------------------------------------------------------
# data processing and analysis
plot3d(tri_centers_normals$x, tri_centers_normals$y, tri_centers_normals$z, 
       aspect = "iso", col = "blue")
spheres3d(tri_centers_normals$x[1000], tri_centers_normals$y[1000], tri_centers_normals$z[1000], 
          radius = search.diam/2, alpha = 0.9, aspect3d = "iso", col="red")
tri_centers_normals$angle <- NA
tri_centers_normals <- tri_centers_normals %>% ungroup()


tri_centers_normals_test <- tri_centers_normals#[1:10000,]
plot3d(tri_centers_normals_test[,2:4])

# -------------------------------------------------------------------------
# output section

# save cleaned BF_table_clean that includes TaxRef taxonomy


# ---------------------

