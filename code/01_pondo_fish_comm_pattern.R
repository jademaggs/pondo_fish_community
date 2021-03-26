# -------------------------------------------------------------------------
# PONDOLAND MPA - MULTIVARIATE FISH COMMUNITY STUDY
# Community pattern - MDS plots
# Jade Maggs
# jademaggs@gmail.com
# -------------------------------------------------------------------------

# IMPORT SOURCE
#source("code/00_pondo_fish_comm_init.R")

# -------------------------------------------------------------------------
# COMMUNITY PATTERN - ABUNDANCE
# -------------------------------------------------------------------------

# Fit MDS (abundance)
fit_pcount_a <- metaMDS(vegdist(pc_abund, method = dissim), k = 2)
fit_transect_a <- metaMDS(vegdist(transect_abund, method = dissim), k = 2)
fit_bruv_a <- metaMDS(vegdist(bruv_abund, method = dissim), k = 2)
fit_cas_a <- metaMDS(vegdist(cas_abund, method = dissim), k = 2)

# Extract mds points for abundance
x_pcount_a <- fit_pcount_a$points[,1]
y_pcount_a <- fit_pcount_a$points[,2]

x_trans_a <- fit_transect_a$points[,1]
y_trans_a <- fit_transect_a$points[,2]

x_bruv_a <- fit_bruv_a$points[,1]
y_bruv_a <- fit_bruv_a$points[,2]

x_cas_a <- fit_cas_a$points[,1]
y_cas_a <- fit_cas_a$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Start device - output abundance plots to svg
svg('output/mds_rel_abund.svg')  # Start device - output to svg
par(mfrow=c(2,2), mai = c(0.2, 0.2, 0.2, 0.2))  # Setup 2x2 plotting window

# Plot mds
# Plot point-counts
plot(x_pcount_a,y_pcount_a,main="Point-counts",
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_pcount_a, y_pcount_a, col=cols[pc_env$baci],
     pch=shps[pc_env$protection])

# Add labels
#text(x_pcount_a, y_pcount_a, adj=c(1.2,0.5), labels = pc_env$year, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(pc_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_pcount_a$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_pcount_a, pc_env$baci, kind="sd")

# Plot transects
plot(x_trans_a,y_trans_a, main="Transects", 
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_trans_a, y_trans_a, col=cols[transect_env$baci],
       pch=shps[transect_env$protection])

# Add labels
#text(x_trans_a, y_trans_a, adj=c(1.2,0.5), labels = transect_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(transect_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_transect_a$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_transect_a, transect_env$protection, kind="sd")

# Plot BRUV
plot(x_bruv_a,y_bruv_a, main="Baited Remote Underwater Video", 
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_bruv_a, y_bruv_a, col=cols[bruv_env$baci],
       pch=shps[bruv_env$protection])

# Add labels
#text(x_bruv_a, y_bruv_a, adj=c(1.2,0.5), labels = bruv_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(bruv_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_bruv_a$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_bruv_a, bruv_env$protection, kind="sd")

# Plot CAS 
plot(x_cas_a,y_cas_a, main="Controlled Angling Survey", 
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_cas_a, y_cas_a, col=cols[cas_env$baci],
       pch=shps[cas_env$protection])

# Add labels
#text(x_cas_a, y_cas_a, adj=c(1.2,0.5), labels = fishing_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(cas_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_cas_a$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_cas_a, cas_env$protection, kind="sd")

# Turn off plotting device
dev.off()

# -------------------------------------------------------------------------
# COMMUNITY PATTERN - BIOMASS
# -------------------------------------------------------------------------

# Fit MDS (biomass)
fit_pcount_b <- metaMDS(vegdist(pc_biom, method = dissim), k = 2)
fit_transect_b <- metaMDS(vegdist(transect_biom, method = dissim), k = 2)
fit_cas_b <- metaMDS(vegdist(cas_biom, method = dissim), k = 2)

# Extract mds points for biomass
x_pcount_b <- fit_pcount_b$points[,1]
y_pcount_b <- fit_pcount_b$points[,2]

x_trans_b <- fit_transect_b$points[,1]
y_trans_b <- fit_transect_b$points[,2]

x_cas_b <- fit_cas_b$points[,1]
y_cas_b <- fit_cas_b$points[,2]

# Setup plotting parameters
cols = c('darkgreen', 'red','green', 'orange')
shps = c(16,17)

# Start device - output biomass plots to svg
svg('output/mds_rel_biom.svg')  # Start device - output to svg
par(mfrow=c(2,2), mai = c(0.2, 0.2, 0.2, 0.2))  # Setup 2x2 plotting window

# Plot mds
# Plot point-counts
plot(x_pcount_b,y_pcount_b,main="Point-counts",
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_pcount_b, y_pcount_b, col=cols[pc_env$baci],
       pch=shps[pc_env$protection])

# Add labels
#text(x_pcount_b, y_pcount_b, adj=c(1.2,0.5), labels = pc_env$year, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(pc_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_pcount_b$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_pcount_b, pc_env$baci, kind="sd")

# Plot transects
plot(x_trans_b,y_trans_b, main="Transects", 
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_trans_b, y_trans_b, col=cols[transect_env$baci],
       pch=shps[transect_env$protection])

# Add labels
#text(x_trans_b, y_trans_b, adj=c(1.2,0.5), labels = transect_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(transect_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_transect_b$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_transect_b, transect_env$protection, kind="sd")

# Plot CAS 
plot(x_cas_b,y_cas_b, main="Controlled Angling Survey", 
     xaxt='n',yaxt='n', xlab='', ylab='', type='n')

# Add points
points(x_cas_b, y_cas_b, col=cols[cas_env$baci],
       pch=shps[cas_env$protection])

# Add labels
#text(x_cas_b, y_cas_b, adj=c(1.2,0.5), labels = fishing_env$site, cex=.7)

# Add legend
legend('topleft', col=cols, 
       legend=levels(cas_env$baci), pch = shps, cex = 0.7)

# Add stress value
legend('topright', 
       legend=paste("2D Stress: ",round(fit_cas_b$stress,digits=2)), 
       bty="n",
       cex=0.7)

# Add ellipse based on standard deviation
ordiellipse(fit_cas_b, cas_env$protection, kind="sd")

# Turn off plotting device
dev.off()

# -------------------------------------------------------------------------
# END OF FILE
# -------------------------------------------------------------------------
