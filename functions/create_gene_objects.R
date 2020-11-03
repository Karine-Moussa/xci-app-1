#### Create gene class: attributes for each gene ######
create_single_gene_stats <- function(gene)
    ### User passes in a gene name from the x_expr list
    ### Function returns the object "<gene>_stats" with attributes of gene
    ### Usage: assign((paste0(gene, "_stats")), create_single_gene_stats(gene))
{assign(paste0(gene, "_stats"),
        # Add any attributes of interest to this list
        (list(
            # Gene name (single)
            gene_name = gene,
            # Gene ID (single)
            gene_id = c(id=x_expr[x_expr$GENE==gene,"gene_id"])[1],
            # Escape status (vector)
            status = c(stat=x_expr[x_expr$GENE==gene,"status"]),
            # P_value (vector)
            p_value = c(p=x_expr[x_expr$GENE==gene,"p_value"]),
            # Average, min, and max p_value (each single)
            avg_p_value = mean(c(x_expr[x_expr$GENE==gene,"p_value"])),
            min_p_value = min(c(x_expr[x_expr$GENE==gene,"p_value"])),
            max_p_value = max(c(x_expr[x_expr$GENE==gene,"p_value"])),
            # Sample where the gene came from (vector)
            parent_sample = c(ps=x_expr[x_expr$GENE==gene,"sample"]),
            # Escape state (based on Vector of escape calls) (single)
            escape_category = ifelse(all(c(esc_cat=x_expr[x_expr$GENE==gene,"status"]) == "S"), "SUPPRESS",
                           ifelse(all(c(esc_cat=x_expr[x_expr$GENE==gene,"status"]) == "E"),"ESCAPE",
                                  "VARIABLE")),
            # Percentage of samples for which gene escaped (single)
            #num_esc = sum(c(x_expr[x_expr$GENE==gene,"status"]) == "E"),
            #num_sup = sum(c(x_expr[x_expr$GENE==gene,"status"]) == "S"),
            perc_samples_esc = sum(c(x_expr[x_expr$GENE==gene,"status"]) == "E")/length(c(x_expr[x_expr$GENE==gene,"status"])),
            # Tau (vector)
            tau = c(tau=x_expr[x_expr$GENE==gene,"tau"]),
            # Cell Type (vector)
            cell_type = c(ctype=rep("lymphoblast",length(x_expr[x_expr$GENE==gene,"status"])))
        )
        )
)
}

# Create gene class object for multiple genes at once
create_multiple_gene_stats <- function(gene_list){
    ### User passes a list of genes 
    ### Function returns a list of "<gene>_stats" for each passed argument
    ### Usage 
    for(gene in gene_list) {
        assign((paste0(gene, "_stats")), create_single_gene_stats(gene),
               env = globalenv())
    }
}

# Create a table summarizing genes and escape states
create_table_with_multiple_gene_stats <- function(gene_list){
    ### User passes a list of genes
    ### Function returns a table with gene statistics based on 
    ###   the attributes in the "<gene>_stats" object
    df <- data.frame(matrix(vector(), 0, 2))
    for (gene in gene_list){
        # Create gene stat df
        gene_stat <- create_single_gene_stats(gene)
        # Collect parameters of interest and add to gene_stat_table
        gene_category_vector <- c(gene_stat$gene_name, 
                                  gene_stat$escape_category, 
                                  gene_stat$avg_p_value)
        df <- rbind(df, gene_category_vector)
    }
    names(df)[1] <- "GENE"
    names(df)[2] <- "ESCAPE.CATEGORY"
    names(df)[3] <- "AVG.P.VAL"
    return(df)
}
