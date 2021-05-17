# This script sources everything in order.

#source("utilities/format_input_data.R", local = TRUE)
#source("utilities/create_global_variables.R", local = TRUE)
#source("utilities/x_expr_mods.R", local = TRUE)
#source("utilities/format_additional_studies.R", local = TRUE)
#source("utilities/format_plot_aesthetics.R", local = TRUE)
source("utilities/read_rds.R", local = TRUE)
source("utilities/create_association_df.R", local = TRUE)
source("utilities/create_trait_objects.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/create_escape_df.R", local = TRUE)

gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")