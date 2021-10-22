# Adding to x_expr data
# This will return x_expr_mod, a modified version of 
# the original x_expr_mod data frame.

# Create vector of BandColors based on gene_stat_table
BandColor <- rep("",nrow(x_expr))
ChromPos <- rep("",nrow(x_expr))
i <- 0
for (gene in x_expr$GENE){
    i <- i + 1
    BandColor[i] <- gene_stat_table[gene_stat_table$GENE == gene, "BAND_COLOR"]
    ChromPos[i] <- gene_stat_table[gene_stat_table$GENE == gene, "CHROM_POS"]
}

# Reformat p-values if they are too low
# Set any p = 0 to p = P_MIN
# Create a flag if this occurs
P_MIN <- 2.845412e-304
p_value_mod <- ifelse(x_expr$p_value == 0, P_MIN, x_expr$p_value)
p_mod_flag <- ifelse(x_expr$p_value == 0, TRUE, FALSE)

# Add -log10 column (for testing)
p_value_mod_neglog10 <- -log10(p_value_mod)

# Reformat skew values to be characters
skew <- format(x_expr$f, digits = 3)

# Add the new elements to x_expr_mod
x_expr_mod <- data.frame()
x_expr_mod <- cbind(x_expr, BandColor, ChromPos, 
                    p_value_mod, p_mod_flag, skew,
                    p_value_mod_neglog10)

# Determine the "variable" state of genes in our data set
# First get percent of samples for which gene escaped (single)
x_expr_mod$status_adv <- rep("",nrow(x_expr))
x_expr_mod$color <- rep("",nrow(x_expr))
x_expr_mod$perc_samples_esc <- rep("",nrow(x_expr))
x_expr_mod$perc_samples_esc_tauplus <- rep("",nrow(x_expr))
for(gene in unique(x_expr$GENE)){
    perc_samples_esc = mean(x_expr[x_expr$GENE == gene, "status"] == "E")
    perc_samples_esc_tauplus = mean(x_expr[x_expr$GENE == gene & x_expr$f <= 0.25, "status"] == "E")
    ifelse(length(perc_samples_esc_tauplus) == 0, perc <- perc_samples_esc, perc <- perc_samples_esc_tauplus)
    if(perc < SV_threshold){
        st = "S"
        color = "lightsteelblue3"
    } else if (perc >= SV_threshold & perc < VE_threshold) {
        st = "V"
        color = "turquoise3"
    } else if (perc >= VE_threshold){
        st = "E"
        color = "purple"
    }
    for(i in 1:nrow(x_expr_mod)){
        if(x_expr_mod$GENE[i] == gene){
            x_expr_mod$status_adv[i] <- st
            x_expr_mod$color[i] <- color
            x_expr_mod$perc_samples_esc[i] <- perc_samples_esc
            x_expr_mod$perc_samples_esc_tauplus[i] <- perc_samples_esc_tauplus
        }
    }
}

# add an escape color column
#for(i in 1:nrow(x_expr_mod)){
#    status <- x_expr_mod$status[i]
#    color <- ifelse(status == "E", "purple", 
#                    ifelse(status == "S", 
#                           "lightsteelblue3", "turquoise3"))
#    x_expr_mod$color[i] <- color
#}

# clean up variables 
rm(i) 
rm(gene)
rm(BandColor)
rm(ChromPos)
rm(p_value_mod)
rm(p_mod_flag)
rm(p_value_mod_neglog10)
rm(skew)
rm(st)
rm(perc)
rm(perc_samples_esc)
rm(perc_samples_esc_tauplus)
rm(color)

# save as RDS for easier compiling
saveRDS(x_expr_mod, "rds/x_expr_mod.rds")
