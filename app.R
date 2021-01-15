# Karine Moussa
# XCI-app 
# App Settings
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Libraries
library(shiny, warn.conflicts = FALSE)
library(dqshiny, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(png, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(magick, warn.conflicts = FALSE)
library(cowplot, warn.conflicts = FALSE)

### Source Data and Functions ###
source("utilities/format_input_data.R", local = TRUE)
source("utilities/create_global_variables.R", local = TRUE)
source("utilities/x_expr_mods.R", local = TRUE)
source("utilities/format_additional_studies.R", local = TRUE)
source("utilities/create_association_df.R", local = TRUE)
source("utilities/create_trait_objects.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/format_plot_aesthetics.R", local = TRUE)

### Load files and pre-processed data
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

### Save publication date
publication_date <- "2021-01-13 12:45:14 EST" # Sys.time()

### Options for Loading Spinner (for TAB1 main plot) #####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

#############################################################
######### MAIN SHINYAPP STARTS HERE #########################
#############################################################
source("myUI.R", local = TRUE) # view myUI.R for code
ui <- myUI

source("myServer.R", local = TRUE) # view myServer.R for code
server <- myServer

#This lets script know its a shiny app
shinyApp(server = server, ui = ui)