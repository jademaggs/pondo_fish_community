# -------------------------------------------------------------------------
# PONDOLAND MPA - MULTIVARIATE FISH COMMUNITY STUDY
# Set up project 
# Jade Maggs
# jademaggs@gmail.com
# -------------------------------------------------------------------------

# Erase all previous variables from memory
rm(list = ls())

# Import packages
library(vegan)  # Multivariate functions

# Read in datasets
pc_env <- read.csv("input/point_count_env.csv", row.names=1, na="")
pc_abund <- read.csv("input/point_count_abundance.csv", row.names=1, na="")
pc_abund_raw <- pc_abund
pc_biom <- read.csv("input/point_count_biomass.csv", row.names=1, na="")
pc_biom_raw <- pc_biom

transect_env <- read.csv("input/transect_env.csv", row.names=1, na="")
transect_abund <- read.csv("input/transect_abundance.csv", row.names=1, na="")
transect_abund_raw <- transect_abund
transect_biom <- read.csv("input/transect_biomass.csv", row.names=1, na="")
transect_biom_raw <- transect_biom

bruv_env <- read.csv("input/bruv_env.csv", row.names=1, na="")
bruv_abund <- read.csv("input/bruv_abundance.csv", row.names=1, na="")
bruv_abund_raw <- bruv_abund

cas_env <- read.csv("input/fishing_env.csv", row.names=1, na="")
cas_abund <- read.csv("input/fishing_abundance.csv", row.names=1, na="")
cas_abund_raw <- cas_abund
cas_biom <- read.csv("input/fishing_biomass.csv", row.names=1, na="")
cas_biom_raw <- cas_biom

# Read in configuration file
config <- read.csv("code/config.txt", na="")

# Set configuration parameters from external config file
stand <- config$value[config$parameter == 'standardise'] 
xform <- config$value[config$parameter == 'xform']  # transformation (1:untransformed, 2:root, 3:log)
dissim <- config$value[config$parameter == 'dissimilarity']   # Dissimilarity measure

# -------------------------------------------------------------------------
# Convert specific variables to factor
# -------------------------------------------------------------------------
pc_env$protection <- factor(pc_env$protection)
pc_env$baci <- factor(pc_env$baci)

transect_env$protection <- factor(transect_env$protection)
transect_env$baci <- factor(transect_env$baci)

bruv_env$protection <- factor(bruv_env$protection)
bruv_env$baci <- factor(bruv_env$baci)

cas_env$protection <- factor(cas_env$protection)
cas_env$baci <- factor(cas_env$baci)

# -------------------------------------------------------------------------
# Standardise abundance and biomass data to percentages 
# -------------------------------------------------------------------------
if (stand == TRUE) {
  stand_text <- "standardised"
  pc_abund <- decostand(pc_abund[,-1], method="total")
  transect_abund <- decostand(transect_abund[,-1], method="total")
  bruv_abund <- decostand(bruv_abund[,-1], method="total")
  cas_abund <- decostand(cas_abund[,-1], method="total")
  pc_biom <- decostand(pc_biom[,-1], method="total")
  transect_biom <- decostand(transect_biom[,-1], method="total")
  cas_biom <- decostand(cas_biom[,-1], method="total")
} else {
  stand_text <- "unstandardised"
}

# -------------------------------------------------------------------------
# Transform abundance and biomass data
# -------------------------------------------------------------------------
if (xform == 1) {
  xform_text <- "untransformed"
} else if (xform == 2) {
  pc_abund <- sqrt(pc_abund)
  transect_abund <- sqrt(transect_abund)
  bruv_abund <- sqrt(bruv_abund)
  cas_abund <- sqrt(cas_abund)
  pc_biom <- sqrt(pc_biom)
  transect_biom <- sqrt(transect_biom)
  cas_biom <- sqrt(cas_biom)
  xform_text <- "root-transformed"
} else if (xform == 3) {
  pc_abund <- log(pc_abund+1)
  transect_abund <- log(transect_abund+1)
  bruv_abund <- log(bruv_abund+1)
  cas_abund <- log(cas_abund+1)
  pc_biom <- log(pc_biom+1)
  transect_biom <- log(transect_biom+1)
  cas_biom <- log(cas_biom+1)
  xform_text <- "log-transformed"
} else {
  xform_text <- "Incorrect option chosen in input_parameters file"
}

# Print configuration
print(paste("Data", stand_text, "and", xform_text, 
            "using", dissim, "dissimilarity."))

# -------------------------------------------------------------------------
# END OF FILE
# -------------------------------------------------------------------------
