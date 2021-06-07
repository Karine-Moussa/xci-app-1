## /utilities

### Formatting and data
**format_input_data.R**
Initializes input x_expr data and global variables\
used in app.R

**add_colors_xchrom_map.R**
Adds gg-plot specific colors to the xchrom map in\
data_sources/xchrom_map_2020-10-20.

**create_global_variables.R**
Initializes global variables.

**x_expr_mods.R**
Adds additional columns to GEUVADIS data set.\
(color, adjusted escape state by escape threshold, ...)

**format_additional_studies.R**
Formats data structures for studies 2-6. 

**format_plot_aesthetics.R**
Creates gg-plot aesthetics and x-chromosome elements.

**find_femalebias_associations.R**
Parses through GWAS traits that are mapped to X chromosome\
and returns those which are female biased based on UK Biobank ratios.


### Functions
**create_lists_of_genes.R**
Creates a master list of all genes. The genes are collected from\
each study, biomaRt is used to complete full list of X-genes.

**create_association_df.R**
Receives gene as input and creates a data frame of all of the GWAS\
traits which are mapped to the passed gene. 

**create_trait_objects.R**
Receives a GWAS trait as input and returns an object with further\
information on that trait (UK Biobank term, sex-ratio, ...)

**create_gene_objects.R**
Receives a gene as input and returns an object with further\
information on that gene (skew_values, perc_samples_esc, ...).\
This is solely used for creating the Tau Plots.

**create_escape_df.R**
Receives a gene as input and returns a data frame with its escape\
state for each study. 

**create_template.R**
Receives the user's template selections and creates a customized\
template. The template is saved as "rds/usr_temp.rds".

**create_ukbio_gwas_table.R**
Uses the  UK_Biobank_master_file.tsv from [EFO-UKB-mappings](https://github.com/EBISPOT/EFO-UKB-mappings) to initialize\
a conversion chart for GWAS to UK Biobank traits. The conversion rds\
object is stored in "rds/GWAS_UKBIO_MAPPING.rds" created by\
"data_intermediate/gwas_ukbio_mapping_xchrom.rds".

**create_study0_df.R**
Creates a blank data frame to initialize the user uploaded study.

### Initialization
**source_all_scripts.R**
Sources scripts at application intialization. Scripts are commented\
out if their RDS objects have already been created and do not need to\
be created again. 

**read_rds.R**
Loads rds objects in R workspace. 
