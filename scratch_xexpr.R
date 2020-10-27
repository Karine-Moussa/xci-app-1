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

### Output Directories ###

### Import data ###
x_expr=read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
xci_status=read.csv("data_sources/Suppl.Table.1.csv",header=T,na.strings="?")
attach(x_expr)

####################################################################
# FORMAT INPUT DATA
####################################################################
##### X_expr Data Formatting #####
XCI=as.factor(XCI)
GENE=as.factor(GENE)

#### XCI_status Data Formatting #####
# Drop NA columns
xci_status <- xci_status[-c(15:26)]
# Collect names of each column

# Prepend GEN19_ to Gencode.19 data

# Prepend COMB_ to Combined data

# Prepend COTT_ to Cotten et. al data

# Combine all names in one vector to update matrix column names

#### Create new table with combined information #####

####################################################################
# FUNCTIONS
####################################################################
#### Create gene class: attributes for each gene ######
create_single_gene_stats <- function(gene)
    ### User passes in a gene name from the x_expr list
    ### Function returns the object "<gene>_stats" with attributes of gene
    ### Usage: assign((paste0(gene, "_stats")), create_single_gene_stats(gene))
  {assign(paste0(gene, "_stats"),
           # Add any attributes of interest to this list
           (list(
               # Gene name
               gene_name = gene,
               # Vector of escape calls:
               escape = c(XCI=x_expr[x_expr$GENE==gene,"XCI"]),
               # Sample where the gene came from:
               parent_sample = c(XCI=x_expr[x_expr$GENE==gene,"sample"]),
               # Escape state (based on Vector of escape calls): 
               state = ifelse(all(c(XCI=x_expr[x_expr$GENE==gene,"XCI"]) == "S"), "SUPPRESS",
                       ifelse(all(c(XCI=x_expr[x_expr$GENE==gene,"XCI"]) == "E"),"ESCAPE",
                       "VARIABLE")),
               tau = c(XCI=x_expr[x_expr$GENE==gene,"tau"])
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
        gene_stat_vector <- c(gene_stat$gene_name, gene_stat$state)
        df <- rbind(df, gene_stat_vector)
    }
    names(df)[1] <- "GENE"
    names(df)[2] <- "ESCAPE.STATE"
    return(df)
}

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
# (for testing, chose XIST)
gene_color <-ifelse(GENE=="XIST","blue",
                    ifelse(GENE!="XIST","grey",
                        "black"))
genetau_color <- gene_color
# ggplot scatter
mytheme <- theme(axis.text.x=element_blank())
genetau <- ggplot(data = x_expr, aes(x=GENE, y=tau)) + 
    ggtitle('GENE vs. TAU') + mytheme + 
    geom_point(color=genetau_color)
genetau
plotly(genetau)

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


