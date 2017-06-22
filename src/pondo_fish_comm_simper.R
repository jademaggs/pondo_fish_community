# AUTHOR:       Jade Q. Maggs MSc
# POSITION:     Assistant Scientist
# AFFILIATION:  Oceanographic Research Institute / University of KwaZulu-Natal
# CONTACT:      email: jmaggs@ori.org.za / mobile: +2783 515 1079 

# 1. SCRIPT METADATA ------------------------------------------------------

# INSTRUCTIONS    Execute "SETUP" first then individual sections in "MAIN"
# OVERVIEW:       Performs SIMPER analysis
# START DATE:     2017-06-07
# INPUTS:         Imports text file from "/data" folder in working directory
# OUTPUTS:        Exports all output to "/output" folder in working directory
# DATA ORIGIN:    Pondoland - underwater visual census, bruv, fishing
# DATE EXTRACTED: 2017-06-06
# COVERAGE:       Pondoland MPA
#                 2002 - 2016
#                 marine

# 2. SETUP ----------------------------------------------------------------

# Erase all previous variables from memory
rm(list = ls())

# Set working directory
setwd("c:/Users/jmaggs/Documents/Publications/pondo_fish_community/") # Microsoft Windows
#setwd("/home/jmaggs/") # Linux Ubuntu

# 2.1. IMPORT OTHER SOURCE CODE

# 2.2. IMPORT PACKAGES
library(vegan)  # Multivariate functions

# 2.3. FUNCTION DEFINITIONS

# 2.4. IMPORT DATASETS

# Read in datasets
point_count_env <- read.csv("data/point_count_env.csv", row.names=1, na="")
point_count <- read.csv("data/point_count.csv", row.names=1, na="")

transect_env <- read.csv("data/transect_env.csv", row.names=1, na="")
transect <- read.csv("data/transect.csv", row.names=1, na="")

bruv_env <- read.csv("data/bruv_env.csv", row.names=1, na="")
bruv <- read.csv("data/bruv.csv", row.names=1, na="")

fishing_env <- read.csv("data/fishing_env.csv", row.names=1, na="")
fishing <- read.csv("data/fishing.csv", row.names=1, na="")

# 2.5. EXPLORE DATA

# 2.6. FORMAT DATA
# datetime <- "%Y-%M-%d %H:%M:%S"

# 2.7. GLOBAL DECLARATIONS
trans <- 2  # transformation (1:untransformed, 2:root, 3:log)
#dissim <- "bray"  # Dissimilarity measure

# 3. MAIN -----------------------------------------------------------------

# SIMPER point-count
if (trans == 1) {
  with(point_count_env, simper(point_count, baci))  # Untransformed
} else if (trans == 2) {
  with(point_count_env, simper(sqrt(point_count), baci))  #  root transformed
} else if (trans == 3) {
  with(point_count_env, simper(log(point_count+1), baci))  # log transformed
} else {
  print("Incorrect option")
}

# SIMPER transect
if (trans == 1) {
  with(transect_env, simper(transect, baci))  # Untransformed
} else if (trans == 2) {
  with(transect_env, simper(sqrt(transect), baci))  #  root transformed
} else if (trans == 3) {
  with(transect_env, simper(log(transect+1), baci))  # log transformed
} else {
  print("Incorrect option")
}

# SIMPER bruv
if (trans == 1) {
  with(bruv_env, simper(bruv, baci))  # Untransformed
} else if (trans == 2) {
  with(bruv_env, simper(sqrt(bruv), baci))  #  root transformed
} else if (trans == 3) {
  with(bruv_env, simper(log(bruv+1), baci))  # log transformed
} else {
  print("Incorrect option")
}

# SIMPER fishing
if (trans == 1) {
  with(fishing_env, simper(fishing, baci))  # Untransformed
} else if (trans == 2) {
  with(fishing_env, simper(sqrt(fishing), baci))  #  root transformed
} else if (trans == 3) {
  with(fishing_env, simper(log(fishing+1), baci))  # log transformed
} else {
  print("Incorrect option")
}

# 4.END OF FILE -----------------------------------------------------------