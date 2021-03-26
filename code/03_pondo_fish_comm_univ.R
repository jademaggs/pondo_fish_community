# -------------------------------------------------------------------------
# PONDOLAND MPA - MULTIVARIATE FISH COMMUNITY STUDY
# Univariate summaries of multivariate fish community data 
# Jade Maggs
# jademaggs@gmail.com
# -------------------------------------------------------------------------

# IMPORT SOURCE
source("code/00_pondo_fish_comm_init.R")

# -------------------------------------------------------------------------
# SHANNON DIVERSITY
# -------------------------------------------------------------------------

index <- "shannon"

if(xform == 1){
  H_pc <- diversity(point_count, index)
  H_trans <- diversity(transect, index)
  H_bruv <- diversity(bruv, index)
  H_cas <- diversity(cas, index)
} else if(xform == 2){
  H_pc <- diversity(sqrt(point_count), index)
  H_trans <- diversity(sqrt(transect), index)
  H_bruv <- diversity(sqrt(bruv), index)
  H_cas <- diversity(sqrt(cas), index)
} else if(xform == 3) {
  H_pc <- diversity(log(point_count+1), index)
  H_trans <- diversity(log(transect+1), index)
  H_bruv <- diversity(log(bruv+1), index)
  H_cas <- diversity(log(cas+1), index)
}

# Mean shannon values
tapply(H_pc, point_count_env$baci, mean)
tapply(H_trans, transect_env$protection, mean)
tapply(H_bruv, bruv_env$protection, mean)
tapply(H_cas, cas_env$protection, mean)

# Start device - output to EMF
if (print_to_file == TRUE){
  win.metafile('output/shannon_div.emf')  # Start device - output to EMF
}

par(mfrow=c(2,2), mai = c(0.4, 0.4, 0.4, 0.4))  # Setup 2x2 plotting window

# Plot
plot(H_pc~point_count_env$baci, ylim=c(0,5))
plot(H_trans~transect_env$protection, ylim=c(0,5))
plot(H_bruv~bruv_env$protection, ylim=c(0,5))
plot(H_cas~cas_env$protection, ylim=c(0,5))

# Turn off plotting device
if (print_to_file == TRUE){
  dev.off()
}

# Check for compliance with parametric data analyses
# Point count
shapiro.test(H_pc[point_count_env$baci == "prior_no-take"])  # Test normality
shapiro.test(H_pc[point_count_env$baci == "post_no-take"])
shapiro.test(H_pc[point_count_env$baci == "prior_exploited"])
shapiro.test(H_pc[point_count_env$baci == "post_exploited"])
bartlett.test(H_pc, point_count_env$baci)  # Test of constant variance
# Transect
shapiro.test(H_trans[transect_env$protection == "exploited"])
shapiro.test(H_trans[transect_env$protection == "no-take"])
bartlett.test(H_trans, transect_env$protection)  # use parametric
# BRUV
shapiro.test(H_bruv[bruv_env$protection == "exploited"])
shapiro.test(H_bruv[bruv_env$protection == "no-take"])
bartlett.test(H_bruv, bruv_env$protection)  # use non-parametric
# CAS
shapiro.test(H_cas[cas_env$protection == "exploited"])
shapiro.test(H_cas[cas_env$protection == "no-take"])
bartlett.test(H_cas, cas_env$protection)  # use non-parametric

# Test for significant differences
kruskal.test(H_pc~point_count_env$baci)  # Point-count
t.test(H_trans~transect_env$protection)  # Transect
wilcox.test(H_bruv~bruv_env$protection)  # BRUV
wilcox.test(H_cas~cas_env$protection)    # CAS

# -------------------------------------------------------------------------
# Simpson diversity
# -------------------------------------------------------------------------

index <- "simpson"

if(xform == 1){
  D_pc <- diversity(point_count, index)
  D_trans <- diversity(transect, index)
  D_bruv <- diversity(bruv, index)
  D_cas <- diversity(cas, index)
} else if(xform == 2){
  D_pc <- diversity(sqrt(point_count), index)
  D_trans <- diversity(sqrt(transect), index)
  D_bruv <- diversity(sqrt(bruv), index)
  D_cas <- diversity(sqrt(cas), index)
} else if(xform == 3) {
  D_pc <- diversity(log(point_count+1), index)
  D_trans <- diversity(log(transect+1), index)
  D_bruv <- diversity(log(bruv+1), index)
  D_cas <- diversity(log(cas+1), index)
}

# Mean simpson values
tapply(D_pc, point_count_env$baci, mean)
tapply(D_trans, transect_env$protection, mean)
tapply(D_bruv, bruv_env$protection, mean)
tapply(D_cas, cas_env$protection, mean)

# Plot
plot(D_pc~point_count_env$baci)
plot(D_trans~transect_env$protection)
plot(D_bruv~bruv_env$protection)
plot(D_cas~cas_env$protection)

# Check for compliance with parametric data analyses
# Point count
shapiro.test(D_pc[point_count_env$baci == "prior_no-take"])  # Test normality
shapiro.test(D_pc[point_count_env$baci == "post_no-take"])
shapiro.test(D_pc[point_count_env$baci == "prior_exploited"])
shapiro.test(D_pc[point_count_env$baci == "post_exploited"])
bartlett.test(D_pc, point_count_env$baci)  # Test of constant variance
# Transect
shapiro.test(D_trans[transect_env$protection == "exploited"])
shapiro.test(D_trans[transect_env$protection == "no-take"])
bartlett.test(D_trans, transect_env$protection)  # use non-parametric
# BRUV
shapiro.test(D_bruv[bruv_env$protection == "exploited"])
shapiro.test(D_bruv[bruv_env$protection == "no-take"])
bartlett.test(D_bruv, bruv_env$protection)  # use non-parametric
# CAS
shapiro.test(D_cas[cas_env$protection == "exploited"])
shapiro.test(D_cas[cas_env$protection == "no-take"])
bartlett.test(D_cas, cas_env$protection)  # use non-parametric

# Test for significant differences
kruskal.test(D_pc~point_count_env$baci)  # Point-count
wilcox.test(D_trans~transect_env$protection)  # Transect
wilcox.test(D_bruv~bruv_env$protection)  # BRUV
wilcox.test(D_cas~cas_env$protection)  # CAS

# -------------------------------------------------------------------------
# Species richness
# -------------------------------------------------------------------------

if(xform == 1){
  S_pc <- specnumber(point_count)
  S_trans <- specnumber(transect)
  S_bruv <- specnumber(bruv)
  S_cas <- specnumber(cas)
} else if(xform == 2){
  S_pc <- specnumber(sqrt(point_count))
  S_trans <- specnumber(sqrt(transect))
  S_bruv <- specnumber(sqrt(bruv))
  S_cas <- specnumber(sqrt(cas))
} else if(xform == 3) {
  S_pc <- specnumber(log(point_count+1))
  S_trans <- specnumber(log(transect+1))
  S_bruv <- specnumber(log(bruv+1))
  S_cas <- specnumber(log(cas+1))
}

# Mean simpson values
tapply(S_pc, point_count_env$baci, mean)
tapply(S_trans, transect_env$protection, mean)
tapply(S_bruv, bruv_env$protection, mean)
tapply(S_cas, cas_env$protection, mean)

# Plot
plot(S_pc~point_count_env$baci)
plot(S_trans~transect_env$protection)
plot(S_bruv~bruv_env$protection)
plot(S_cas~cas_env$protection)

# Check for compliance with parametric data analyses
# Point count
shapiro.test(S_pc[point_count_env$baci == "prior_no-take"])  # Test normality
shapiro.test(S_pc[point_count_env$baci == "post_no-take"])
shapiro.test(S_pc[point_count_env$baci == "prior_exploited"])
shapiro.test(S_pc[point_count_env$baci == "post_exploited"])
bartlett.test(S_pc, point_count_env$baci)  # Test of constant variance
# Transect
shapiro.test(S_trans[transect_env$protection == "exploited"])
shapiro.test(S_trans[transect_env$protection == "no-take"])
bartlett.test(S_trans, transect_env$protection)  # use non-parametric
# BRUV
shapiro.test(S_bruv[bruv_env$protection == "exploited"])
shapiro.test(S_bruv[bruv_env$protection == "no-take"])
bartlett.test(S_bruv, bruv_env$protection)  # use non-parametric
# CAS
shapiro.test(S_cas[cas_env$protection == "exploited"])
shapiro.test(S_cas[cas_env$protection == "no-take"])
bartlett.test(S_cas, cas_env$protection)  # use non-parametric

# Test for significant differences
anova(S_pc~point_count_env$baci)  # Point-count
wilcox.test(S_trans~transect_env$protection)  # Transect
wilcox.test(S_bruv~bruv_env$protection)  # BRUV
wilcox.test(S_cas~cas_env$protection)  # CAS

# -------------------------------------------------------------------------
# Pielou's evenness
# -------------------------------------------------------------------------

J_pc <- H_pc/log(S_pc)
J_trans <- H_trans/log(S_trans)
J_bruv <- H_bruv/log(S_bruv)
J_cas <- H_cas/log(S_cas)

# Mean Pielou's values
tapply(J_pc, point_count_env$baci, mean)
tapply(J_trans, transect_env$protection, mean)
tapply(J_bruv, bruv_env$protection, mean)
tapply(J_cas, cas_env$protection, mean)

# Plot
plot(J_pc~point_count_env$baci)
plot(J_trans~transect_env$protection)
plot(J_bruv~bruv_env$protection)
plot(J_cas~cas_env$protection)

# Check for compliance with parametric data analyses
# Point count
shapiro.test(S_pc[point_count_env$baci == "prior_no-take"])  # Test normality
shapiro.test(S_pc[point_count_env$baci == "post_no-take"])
shapiro.test(S_pc[point_count_env$baci == "prior_exploited"])
shapiro.test(S_pc[point_count_env$baci == "post_exploited"])
bartlett.test(S_pc, point_count_env$baci)  # Test of constant variance
# Transect
shapiro.test(S_trans[transect_env$protection == "exploited"])
shapiro.test(S_trans[transect_env$protection == "no-take"])
bartlett.test(S_trans, transect_env$protection)  # use parametric
# BRUV
shapiro.test(S_bruv[bruv_env$protection == "exploited"])
shapiro.test(S_bruv[bruv_env$protection == "no-take"])
bartlett.test(S_bruv, bruv_env$protection)  # use non-parametric
# CAS
shapiro.test(S_cas[cas_env$protection == "exploited"])
shapiro.test(S_cas[cas_env$protection == "no-take"])
bartlett.test(S_cas, cas_env$protection)  # use non-parametric

# Test for significant differences
anova(S_pc~point_count_env$baci)  # Point-count
wilcox.test(S_trans~transect_env$protection)  # Transect
wilcox.test(S_bruv~bruv_env$protection)  # BRUV
wilcox.test(S_cas~cas_env$protection)  # CAS

# -------------------------------------------------------------------------
# END OF FILE
# -------------------------------------------------------------------------
