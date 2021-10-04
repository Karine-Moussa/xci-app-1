# This script sources everything in order.

# Formatting and data
# source("utilities/add_colors_xchrom_map.R", local = TRUE)
# source("utilities/format_input_data.R", local = TRUE)
# source("utilities/create_global_variables.R", local = TRUE)
# source("utilities/x_expr_mods.R", local = TRUE)
# source("utilities/format_additional_studies.R", local = TRUE)
# source("utilities/format_plot_aesthetics.R", local = TRUE)
# source("utilities/create_lists_of_genes.R", local = TRUE)
# source("utilities/consolidate_escape_states.R", local = TRUE)
if ( exists("rds/study0_df.rds") ){
    rm("rds/study0_df.rds") # clear out the last study0 df
}
source("utilities/read_rds.R", local = TRUE)

# Functions
source("utilities/create_association_df.R", local = TRUE)
source("utilities/create_trait_objects.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/create_escape_df.R", local = TRUE)
source("utilities/create_template.R", local = TRUE)

# Extras
source("utilities/create_study0_df.R", local = TRUE)
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")
