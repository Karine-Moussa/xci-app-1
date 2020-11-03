# Visualizing x expression data
### Packages
install.packages("ggplot2")

### Libraries #####
library(ggplot2)
library(plotly)
library(rstudioapi)

### Set Working Directory ###
# For now, just setting it to where this script is saved
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)

### Source Functions ####
source("functions/format_input_data.R")
source("functions/create_gene_objects.R")

# Save gene list to csv file in data_intermediate
gene_list_all = c(unique(x_expr[,"GENE"])) # for testing
gene_stat_table <- create_table_with_multiple_gene_stats(gene_list_all)
write.csv(gene_stat_table, "data_intermediate/gene_stat_table.csv")

####################################################################
# PLOTS
####################################################################
# GENE VS. TAU SCATTER PLOT
# create color palette to emphasize tau level
# (arbitrarily chose 0.3)
tau_sig_factor = 0.3
tau_color <- ifelse(tau>tau_sig_factor, "blue",  # In R when providing a condition an equality HAS to be written with '=='
                ifelse(tau<=tau_sig_factor, "grey",
                       "black"))
# create color palette for showing gene significance
# for testing, use any gene
geneofinterest = "XIST"
gene_color <-ifelse(GENE==geneofinterest,"blue",
                    ifelse(GENE!=geneofinterest,"grey",
                        "black"))
genepvalue_color <- gene_color
# ggplot scatter ONE SAMPLE
sample_name <- SAMPLE_NAMES[1]
mytheme <- theme(axis.text.x = element_blank(),
                 plot.title = element_text(family = "Courier", face = "bold", size = (18), hjust = 0.5), 
                 legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica", size = (14)), 
                 legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica", size = (14)), 
                 axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4", face = "bold"),
                 axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10), face = "bold"))
genepvalue <- ggplot(data = subset(x_expr, sample == sample_name), aes(x = reorder(GENE, start), y = -log10(p_value))) + 
  ylim(0, 400) + 
  ggtitle("Gene vs. Escape Call") + 
  xlab(paste0("Sample ", sample_name," Genes")) + ylab("-log10(p_value)") +
  mytheme + 
  geom_point(color="gray")
genepvalue
ggplotly(genepvalue)
# ggplot scatter PER SAMPLE


# ESCAPE STATE FREQUENCY BAR PLOT
# ggplot bar for gene stats
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")
gene_frequency_plot <- ggplot(gene_stat_table, aes(x = ESCAPE.STATE, label = "")) + 
  geom_bar(position = "dodge", fill = "lightblue") + 
  geom_text(stat = 'count', aes(label = scales::percent(..prop..), group = 1), 
                                vjust = -1) + 
  labs(y="Counts", x = "Escape State") +
  ggtitle(label = "Gene Escape State Frequency")
gene_frequency_plot
ggplotly(gene_frequency_plot, tooltip = c("text", "fill", "x", "y"))

# VIOLIN PLOTS FOR INDIVUDAL GENE-TAU
geneofinterest <- "ASMTL"
assign("geneofinterest_stats", create_single_gene_stats(geneofinterest))
geneofinterest_tautable <- data.frame("gene" = rep(geneofinterest_stats$gene_name,length(geneofinterest_stats$tau)),
                          "tau" = geneofinterest_stats$tau)
ggplot(geneofinterest_tautable, aes(x = gene, y = tau)) +
            ylim(0,.5) + 
            geom_violin(fill = "purple") + 
            geom_boxplot(width = 0.03, fill = "white") + 
            labs("Tau distribution per Gene", x = "Gene", y = "Tau")