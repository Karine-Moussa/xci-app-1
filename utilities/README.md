### Formatting and Functions used in app.R

**format_input_data.R**\
*initalization*\
Initiliazes input x_expr data and global variables\
used in app.R

**x_expr_mods.R**\
*initalization*\
Adds information to x_expr data frame\
and intializes x_expr_mod for use in downstream\
processing/visualization.

**create_gene_objects.R**\
*functions*\
Includes functions to create class objects for\
each gene, and function to creates a summary\
chart with all genes.\
*initialization*\
Outputs summary chart to data_intermediate/gene_stat_table.csv

**format_plot_aesthetics.R**\
*initializatoin*\
Formats axis/labels/other essential features for plots.\
BP positions for PAR regoins obtained from [GRCh38.13](https://www.ncbi.nlm.nih.gov/grc/human)