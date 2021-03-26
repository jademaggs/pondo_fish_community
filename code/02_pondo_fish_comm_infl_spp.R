# -------------------------------------------------------------------------
# PONDOLAND MPA - MULTIVARIATE FISH COMMUNITY STUDY
# Influential species - simper analyses
# Jade Maggs
# jademaggs@gmail.com
# -------------------------------------------------------------------------

# IMPORT SOURCE 
#source("code/00_pondo_fish_comm_init.R")

# -------------------------------------------------------------------------
# POINT COUNT ABUNDANCE 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_pc_abund <- with(pc_env, simper(pc_abund, baci))
sim_pc_abund  # Cumulative contributions of most influential species

# Average dissimilarity between pairs
lapply(sim_pc_abund, FUN=function(x){x$overall})

# Summarise results with ave, sd, ratio, etc 
sum_sim_pc_abund <- summary(sim_pc_abund, ordered = TRUE, digits=3)

# Top five discriminating species
for (i in names(sum_sim_pc_abund)) {
  df <- data.frame(sum_sim_pc_abund[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
} 

# Characteristic species
for (i in levels(pc_env$baci)) {
  v <- sort(colMeans(pc_abund_raw[pc_env$baci == i,]), decreasing = TRUE)
  df <- data.frame(species = names(v), mean = v, row.names = NULL)
  colnames(df)[colnames(df) == 'mean'] <- paste0(i, '_mean')
  print(df[1:5,]) 
  #print(i)
  #print(v[1:5])
}


# -------------------------------------------------------------------------
# TRANSECT ABUNDANCE 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_trans_abund <- with(transect_env, simper(transect_abund, baci))
sim_trans_abund

# Average dissimilarity between pairs 
lapply(sim_trans_abund, FUN=function(x){x$overall})
sim_trans_abund$post_no_take_post_exploited$overall  # Alternative code

# Summarise results
sum_sim_trans_abund <- summary(sim_trans_abund)

# Top five discriminating species
for (i in names(sum_sim_trans_abund)) {
  df <- data.frame(sum_sim_trans_abund[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# BRUV ABUNDANCE 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_bruv_abund <- with(bruv_env, simper(bruv_abund, baci)) 
sim_bruv_abund

# Average dissimilarity between pairs 
lapply(sim_bruv_abund, FUN=function(x){x$overall})

# Summarise results
sum_sim_bruv_abund <- summary(sim_bruv_abund)

# Top five discriminating species
for (i in names(sum_sim_bruv_abund)) {
  df <- data.frame(sum_sim_bruv_abund[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# CONTROLLED ANGLING SURVEY ABUNDANCE 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_cas_abund <- with(cas_env, simper(cas_abund, baci))
sim_cas_abund

# Average dissimilarity between pairs 
lapply(sim_cas_abund, FUN=function(x){x$overall})

# Summarise results
sum_sim_cas_abund <- summary(sim_cas_abund)

# Top five discriminating species
for (i in names(sum_sim_cas_abund)) {
  df <- data.frame(sum_sim_cas_abund[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# POINT-COUNT BIOMASS 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_pc_biom <- with(pc_env, simper(pc_biom, baci))
sim_pc_biom  # Cumulative contributions of most influential species

# Average dissimilarity between pairs 
lapply(sim_pc_biom, FUN=function(x){x$overall})

# Summarise results with ave, sd, ratio, etc 
sum_sim_pc_biom <- summary(sim_pc_biom, ordered = TRUE, digits=3)

# Top five discriminating species
for (i in names(sum_sim_pc_biom)) {
  df <- data.frame(sum_sim_pc_biom[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# TRANSECT BIOMASS 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_trans_biom <- with(transect_env, simper(transect_biom, baci))
sim_trans_biom

# Average dissimilarity between pairs 
lapply(sim_trans_biom, FUN=function(x){x$overall})
sim_trans_biom$post_no_take_post_exploited$overall  # Alternative code

# Summarise results
sum_sim_trans_biom <- summary(sim_trans_biom)

# Top five discriminating species
for (i in names(sum_sim_trans_biom)) {
  df <- data.frame(sum_sim_trans_biom[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# CONTROLLED ANGLING SURVEY BIOMASS 
# -------------------------------------------------------------------------

# Create SIMPER object
sim_cas_biom <- with(cas_env, simper(cas_biom, baci))
sim_cas_biom

# Average dissimilarity between pairs 
lapply(sim_cas_biom, FUN=function(x){x$overall})

# Summarise results
sum_sim_cas_biom <- summary(sim_cas_biom)

# Top five discriminating species
for (i in names(sum_sim_cas_biom)) {
  df <- data.frame(sum_sim_cas_biom[[i]][1:5,])
  write.csv(df, file = paste0('output/discrim_spp_',i,'.csv'))
}

# -------------------------------------------------------------------------
# END OF FILE
# -------------------------------------------------------------------------

