# Escape thresholds (which can be updated)
SV_threshold = 0.25  # suppressed to variable threshold
VE_threshold = 0.75  # variable to escape threshold

# Ensemble base link
ens_base_loc <- "http://www.ensembl.org/Homo_sapiens/Location/View?r=X:"
ens_base_gene <- "http://www.ensembl.org/Homo_sapiens/Location/View?g="

# Par/centre boundaires according to GRCh38
par1_boundaries <- c(100001,2781479)
par2_boundaries <- c(155701383,156030895)
centre_boundaries <- c(58100001,63800000)

# Save as RDS for easier compiling
saveRDS(SV_threshold, "rds/SV_threshold.rds")
saveRDS(VE_threshold, "rds/VE_threshold.rds")
saveRDS(ens_base_loc, "rds/ens_base_loc.rds")
saveRDS(ens_base_gene, "rds/ens_base_gene.rds")
saveRDS(par1_boundaries, "rds/par1_boundaries.rds")
saveRDS(par2_boundaries, "rds/par2_boundaries.rds")
saveRDS(centre_boundaries, "rds/centre_boundaries.rds")