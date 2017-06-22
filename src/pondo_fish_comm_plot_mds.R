# AUTHOR:       Jade Q. Maggs MSc
# POSITION:     Assistant Scientist
# AFFILIATION:  Oceanographic Research Institute / University of KwaZulu-Natal
# CONTACT:      email: jmaggs@ori.org.za / mobile: +2783 515 1079 

# 1. SCRIPT METADATA ------------------------------------------------------

# INSTRUCTIONS    Execute "SETUP" first then individual sections in "MAIN"
# OVERVIEW:       Run nMDS analysis and output plot
# START DATE:     2017-06-07
# INPUTS:         Imports text file from "/data" folder in working directory
# OUTPUTS:        Exports all output to "/output" folder in working directory
# DATA ORIGIN:    Pondoland - diving, bruv, fishing
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
# Choose transformation and dissimilarity measure 
trans <- 2  # Transformation (1:untransformed, 2:root, 3:log)
dissim <- "bray"  # Dissimilarity measure

# 2.8. Setup plotting
win.metafile('output/mds_rel_abund.emf')  # Output to windows metafile
par(mfrow=c(2,2))  # Setup 2x2 plotting window

# 3. MAIN -----------------------------------------------------------------
# Point counts ----

# Fit MDS with chosen transformation and dissim measure
if (trans == 1) {
  fit_pcount <- metaMDS(vegdist(point_count,method=dissim), k=2)
  trans_text <- "Untransformed point count data"
} else if (trans == 2) {
  fit_pcount <- metaMDS(vegdist(sqrt(point_count),method=dissim), k=2)
  trans_text <- "Root-transformed point count data"
} else if (trans == 3) {
  fit_pcount <- metaMDS(vegdist(log(point_count+1),method=dissim), k=2)
  trans_text <- "Log-transformed point count data"
} else {
  print("Incorrect option")
}

# Extract mds points
x_pcount <- fit_pcount$points[,1]
y_pcount <- fit_pcount$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Plot axes
plot(x_pcount,y_pcount, xlab="Coordinate 1", ylab="Coordinate 2",
     main=c("Point-counts"),type='n')

# Add points
points(x_pcount, y_pcount, col=cols[point_count_env$baci],
     pch=shps[point_count_env$protection])

# Add labels
#text(x_pcount, y_pcount, adj=c(1.2,0.5), labels = point_count_env$year, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(point_count_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_pcount$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_pcount, point_count_env$baci, kind="sd")

# Transects ----

# Fit MDS with chosen transformation and dissim measure
if (trans == 1) {
  fit_transect <- metaMDS(vegdist(transect,method=dissim), k=2)
  trans_text <- "Untransformed transect data"
} else if (trans == 2) {
  fit_transect <- metaMDS(vegdist(sqrt(transect),method=dissim), k=2)
  trans_text <- "Root-transformed transect data"
} else if (trans == 3) {
  fit_transect <- metaMDS(vegdist(log(transect+1),method=dissim), k=2)
  trans_text <- "Log-transformed transect data"
} else {
  print("Incorrect option")
}

# Extract mds points
x_trans <- fit_transect$points[,1]
y_trans <- fit_transect$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Plot axes
plot(x_trans,y_trans, xlab="Coordinate 1", ylab="Coordinate 2",
     main=c("Transects"),type='n')

# Add points
points(x_trans, y_trans, col=cols[transect_env$baci],
       pch=shps[transect_env$protection])

# Add labels
#text(x_trans, y_trans, adj=c(1.2,0.5), labels = transect_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(transect_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_transect$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_transect, transect_env$protection, kind="sd")

# BRUV ----

# Fit MDS with chosen transformation and dissim measure
if (trans == 1) {
  fit_bruv <- metaMDS(vegdist(bruv,method=dissim), k=2)
  trans_text <- "Untransformed BRUV data"
} else if (trans == 2) {
  fit_bruv <- metaMDS(vegdist(sqrt(bruv),method=dissim), k=2)
  trans_text <- "Root-transformed BRUV data"
} else if (trans == 3) {
  fit_bruv <- metaMDS(vegdist(log(bruv+1),method=dissim), k=2)
  trans_text <- "Log-transformed BRUV data"
} else {
  print("Incorrect option")
}

# Extract mds points
x_bruv <- fit_bruv$points[,1]
y_bruv <- fit_bruv$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Plot axes
plot(x_bruv,y_bruv, xlab="Coordinate 1", ylab="Coordinate 2",
     main=c("BRUV"),type='n')

# Add points
points(x_bruv, y_bruv, col=cols[bruv_env$baci],
       pch=shps[bruv_env$protection])

# Add labels
#text(x_bruv, y_bruv, adj=c(1.2,0.5), labels = bruv_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(bruv_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_bruv$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_bruv, bruv_env$protection, kind="sd")

# Fishing ----

# Fit MDS with chosen transformation and dissim measure
if (trans == 1) {
  fit_fishing <- metaMDS(vegdist(fishing,method=dissim), k=2)
  trans_text <- "Untransformed CAS data"
} else if (trans == 2) {
  fit_fishing <- metaMDS(vegdist(sqrt(fishing),method=dissim), k=2)
  trans_text <- "Root-transformed CAS data"
} else if (trans == 3) {
  fit_fishing <- metaMDS(vegdist(log(fishing+1),method=dissim), k=2)
  trans_text <- "Log-transformed CAS data"
} else {
  print("Incorrect option")
}

# Extract mds points
x_fishing <- fit_fishing$points[,1]
y_fishing <- fit_fishing$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Plot axes
plot(x_fishing,y_fishing, xlab="Coordinate 1", ylab="Coordinate 2",
     main=c("CAS"),type='n')

# Add points
points(x_fishing, y_fishing, col=cols[fishing_env$baci],
       pch=shps[fishing_env$protection])

# Add labels
#text(x_fishing, y_fishing, adj=c(1.2,0.5), labels = fishing_env$site, cex=.7)

# Add legend
legend('top', col=cols, 
       legend=levels(fishing_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_fishing$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_fishing, fishing_env$protection, kind="sd")

# Turn off plotting device ----
dev.off()

# 4.END OF FILE -----------------------------------------------------------