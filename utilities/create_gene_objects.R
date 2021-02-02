#### Create gene class: attributes for each gene ######
create_single_gene_stats <- function(gene, ref_table)
    ### User passes in a gene name from the ref_table list
    ### Function returns the object "<gene>_stats" with attributes of gene
    ### Usage:  assign("geneofinterest_stats", create_single_gene_stats(gene, x_expr))
# First, set up function for finding gene color stain and region
{
    gene_start <- gene_stat_table[gene_stat_table$GENE==gene,"START"]
    for (row_num in (1:nrow(xchrom_map_colored))){
        in_region <- between(gene_start, xchrom_map_colored[row_num,"bp_start"],xchrom_map_colored[row_num,"bp_stop"])
        gene_stain <- ifelse(in_region==T, xchrom_map_colored[row_num,"BandColor"],"")
        chrom_tmp <- ifelse(in_region==T, xchrom_map_colored[row_num,"X.chromosome"],"")
        arm_tmp <- ifelse(in_region==T, xchrom_map_colored[row_num,"arm"],"")
        band_tmp <- ifelse(in_region==T, xchrom_map_colored[row_num,"band"],"")
        # Break out of this loop once we find a stain color
        if(isTruthy(gene_stain != "")){break}
    }
# Establish a TRUE/FALSE vector for matching disease/traits to UKBIO names
    # (this is just to make code cleaner further down)

# Then, assign attributes to gene
    assign(paste0(gene, "_stats"),
        # Add any attributes of interest to this list
        (list(
            # Gene name (single)
            gene_name = gene,
            # Gene ID (single)
            gene_id = c(id=ref_table[ref_table$GENE==gene,"gene_id"])[1],
            # Sample where the gene came from (vector)
            parent_sample = c(ps=ref_table[ref_table$GENE==gene,"sample"]),
            # Start and end bp (single)
            start = c(start=ref_table[ref_table$GENE==gene,"start"])[1],
            end = c(end=ref_table[ref_table$GENE==gene,"end"])[1],
            # Stain color (single)
            band_color = gene_stain,
            # Chromosomal location 
            chrom = chrom_tmp,
            arm = arm_tmp,
            band = band_tmp,
            # Escape status (vector)
            status = c(stat=ref_table[ref_table$GENE==gene,"status"]),
            # P_value (vector)
            p_values = c(p=ref_table[ref_table$GENE==gene,"p_value"]),
            # Average, min, and max p_value (single)
            avg_p_value = mean(c(ref_table[ref_table$GENE==gene,"p_value"])),
            min_p_value = min(c(ref_table[ref_table$GENE==gene,"p_value"])),
            max_p_value = max(c(ref_table[ref_table$GENE==gene,"p_value"])),
            # Skew
            skew_values = c(skew=ref_table[ref_table$GENE==gene,"f"]),
            avg_skew = mean(c(skew=ref_table[ref_table$GENE==gene,"f"])),
            min_skew = min(c(skew=ref_table[ref_table$GENE==gene,"f"])),
            max_skew = max(c(skew=ref_table[ref_table$GENE==gene,"f"])),
            # Skew < %25
            skew_values_plus = c(skew=ref_table[ref_table$GENE==gene & ref_table$f <= 0.25,"f"]),
            # Escape state (based on Vector of escape calls) (single)
            escape_category = ifelse(all(c(esc_cat=ref_table[ref_table$GENE==gene,"status"]) == "S"), "SUPPRESS",
                           ifelse(all(c(esc_cat=ref_table[ref_table$GENE==gene,"status"]) == "E"),"ESCAPE",
                                  "VARIABLE")),
            # Percentage of samples for which gene escaped (single)
            perc_samples_esc = mean(ref_table[ref_table$GENE == gene, "status"] == "E"),
            perc_samples_esc_tauplus = mean(ref_table[ref_table$GENE == gene & ref_table$f <= 0.25, "status"] == "E"),
            # Tau (vector)
            tau = c(tau=ref_table[ref_table$GENE==gene,"tau"]),
            # Average, min, and max tau_value (single)
            avg_tau_value = mean(c(ref_table[ref_table$GENE==gene,"tau"])),
            min_tau_value = min(c(ref_table[ref_table$GENE==gene,"tau"])),
            max_tau_value = max(c(ref_table[ref_table$GENE==gene,"tau"])),
            # Stain color
            #stain_color = BandColor[xchrom_map$density == "NA"] = xcolors[1],
            # Cell Type (vector)
            cell_type = c(ctype=rep("lymphoblast",length(ref_table[ref_table$GENE==gene,"status"])))
            # Escape State DF
        )
        )
)
}

# Create gene class object for multiple genes at once
create_multiple_gene_stats <- function(gene_list, ref_table){
    ### User passes a list of genes 
    ### Function returns a list of "<gene>_stats" for each passed argument
    ### Usage 
    for(gene in gene_list) {
        assign((paste0(gene, "_stats")), create_single_gene_stats(gene, ref_table),
               env = globalenv())
    }
}

# Create a table summarizing genes and escape states
create_table_with_multiple_gene_stats <- function(gene_list, ref_table){
    ### User passes a list of genes
    ### Function returns a table with gene statistics based on 
    ###   the attributes in the "<gene>_stats" object
    df <- data.frame(matrix(vector(), 0, 2))
    for (gene in gene_list){
        # Create gene stat df
        gene_stat <- create_single_gene_stats(gene, ref_table)
        # Collect parameters of interest and add to gene_stat_table
        gene_category_vector <- c(gene_stat$gene_name, 
                                  gene_stat$escape_category,
                                  gene_stat$avg_p_value,
                                  gene_stat$start,
                                  gene_stat$end,
                                  gene_stat$band_color,
                                  paste0(gene_stat$chrom, gene_stat$arm, gene_stat$band))
        df <- rbind(df, gene_category_vector)
    }
    names(df)[1] <- "GENE"
    names(df)[2] <- "ESCAPE_CATEGORY"
    names(df)[3] <- "AVG_P_VAL"
    names(df)[4] <- "START"
    names(df)[5] <- "END"
    names(df)[6] <- "BAND_COLOR"
    names(df)[7] <- "CHROM_POS"
    return(df)
}
# Create csv file with summary information for each gene
# Save gene list to csv file in data_intermediate
gene_list_all <- c(unique(x_expr[,"GENE"]))
gene_stat_table <- create_table_with_multiple_gene_stats(gene_list_all, x_expr)
write.csv(gene_stat_table, "data_intermediate/gene_stat_table.csv")

# clean up variables
rm(gene_list_all)
