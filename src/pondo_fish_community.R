# AUTHOR:       Jade Q. Maggs PhD
# POSITION:     Scientist
# AFFILIATION:  Oceanographic Research Institute / University of KwaZulu-Natal
# CONTACT:      jademaggs@gmail.com

# 1. SCRIPT METADATA ------------------------------------------------------

# INSTRUCTIONS    Execute "SETUP" first then individual sections in "MAIN"
# OVERVIEW:       <Purpose of code>
# START DATE:     2017-06-07
# INPUTS:         Imports text file from "/data" folder in working directory
# OUTPUTS:        Exports all output to "/output" folder in working directory
# DATA ORIGIN:    Pondoland - diving, bruv, fishing
# DATE EXTRACTED: 2017-06-06
# COVERAGE:       Pondoland MPA
#                 yyyy-mm-dd - yyyy-mm-dd
#                 marine

# 2. SETUP ----------------------------------------------------------------

# Erase all previous variables from memory
rm(list = ls())

# Set working directory
setwd("/home/jmaggs/")

# 2.1. IMPORT OTHER SOURCE CODE

# 2.2. IMPORT PACKAGES
library(vegan)  # Multivariate functions
library(sparcl) # For ColorDendrogram
library(MASS)

# 2.3. FUNCTION DEFINITIONS

# 2.4. IMPORT DATASETS

# Read in dataset
raw_point_count <- read.csv("data/point_count.csv", row.names=1, na="")

# 2.5. EXPLORE DATA
# prep.data <- raw.data
# 
# names(prep.data)
# head(prep.data)
# tail(prep.data)
# summary(prep.data)
# str(prep.data)

# 2.6. FORMAT DATA
# datetime <- "%Y-%M-%d %H:%M:%S"

# 2.7. GLOBAL DECLARATIONS

# 3. MAIN -----------------------------------------------------------------

# Point counts ----
point_count <- raw_point_count 

mat_point <- as.matrix(point_count) # Convert dataframe to a matrix
mat_point <- vegdist(mat_point,method="bray") # Convert matrix to a distance object

# Clustering with customised resemblance
hc <- hclust(mat_point, method="average")
plot(hc, ylab="% Similarity",axes=FALSE, cex=.7)

fit <- isoMDS(mat_point, k=2)

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS")

text(x, y, labels = row.names(point_count), cex=.7)

# Section 2 ----
dat <- prep.data

# 4.END OF FILE -----------------------------------------------------------
