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

### Source Data and Functions ###
source("utilities/format_input_data.R", local = TRUE)
source("utilities/create_global_variables.R", local = TRUE)
source("utilities/x_expr_mods.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/format_plot_aesthetics.R", local = TRUE)

### Load files and pre-processed data
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

### Save publication date
publication_date <- "2020-12-17 16:27:21 EST"

### Options for Loading Spinner #####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# shinyapp
ui <- fluidPage(title = "XCI Data",
                tabsetPanel(
                    ## TAB 1
                    tabPanel(title = "All Escape Expressions",
                        # Create a layout with a sidebar and main area ----
                        sidebarLayout(
                            # Create a sidebar panel containing input controls ----
                            sidebarPanel(
                                h3("Observing XCI Escape Calls"),
                                selectInput("searchType", "Search Type",
                                            c(Gene = "gene", Disease = "disease")
                                ),
                                conditionalPanel(
                                    condition = "input.searchType == 'gene'",
                                    autocomplete_input("geneofinterest1", "Gene of Interest:", c(unique(x_expr_mod[,"GENE"])), value = ""),
                                ),
                                conditionalPanel(
                                    condition = "input.searchType == 'disease'",
                                    selectizeInput("diseaseofinterest1", "Disease of Interest:", c("",list_of_diseases))
                                ),
                                br(),
                                strong("Directions for Use", style = "font-size:12px"),br(),
                                em("---<update directions>", style = "font-size:12px"),br(),
                                em("---<update directions>", style = "font-size:12px"),br(),
                                em("---<update directions>", style = "font-size:12px"),br(),
                                br(),
                                strong("Parameters"),
                                p("Gene =", span(a("268 X-Chromosome Genes", href="null", target="_blank")), style = "font-size:12px"),
                                br(),
                                br(),
                                em(paste("Last published:",publication_date), style = "font-size:12px;color:grey")
                            ),
                            # Create plot and Action Buttons in Main Panel
                            mainPanel(
                                withSpinner(plotOutput(outputId = "gene_pvalue", height = "500px"), type = 2),
                                plotOutput(outputId = "gene_pvalue_xchromosome", height = "100px"),
                               # Only show this panel if the plot type is a histogram
                                conditionalPanel(
                                    condition = "input.geneofinterest1 != ''",
                                    strong("GWAS Catalog Search (Gene)", style = "font-size:16px"),
                                    p(span(a("Searching \"All Assocations v1.0\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                    (dataTableOutput(outputId = "gene_gwas_data"))
                                ),
                                conditionalPanel(
                                    condition = "input.diseaseofinterest1 != ''",
                                    p("GWAS Catalog Search (Disease)", style = "font-size:16px"),
                                    p(span(a("Searching \"All Assocations v1.0\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                    (dataTableOutput(outputId = "gene_disease_data"))
                                )
                            )
                        )
                    ),
                    ## TAB 2
                    tabPanel(title = "Individual Gene Search",
                        # Create a layout with a sidebar and main area
                        sidebarLayout(
                            sidebarPanel(
                                h3("Observing XCI escape calls per Gene"),
                                autocomplete_input("geneofinterest2", "Gene of Interest:", c(unique(x_expr_mod[,"GENE"])), value = ""),
                                br(),
                                strong("Directions for Use", style = "font-size:12px"),br(),
                                em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                                em("---Examples: XIST, ZBED1, ASMTL", style = "font-size:12px"),br(),
                                em("---Hover over data points for more information", style = "font-size:12px"),br(),
                                br(),
                                strong("TAU and TAU+", style = "font-size:12px"),br(),
                                em("---All samples  are included in TAU", style = "font-size:12px"),br(),
                                em("---Those samples from skew data < 25% are represented in TAU+", style = "font-size:12px"),br(),
                                br(),
                                strong("Parameters"),
                                p("Tau = (Xi Expression)/(Total Expression)", style = "font-size:12px"),
                                p("Gene =", span(a("268 X-Chromosome Genes", href="null", target="_blank")), style = "font-size:12px"),
                                br(),
                                br(),
                                em(paste("Last published:",publication_date), style = "font-size:12px;color:grey")
                            ),
                            mainPanel(
                                (plotOutput(outputId = "individual_gene_tau_plot")),
                                conditionalPanel(
                                    condition = "input.geneofinterest2 != ''",
                                        fluidRow(
                                            column(12, "",
                                                   fixedRow(
                                                    column(4,
                                                           p("TAU Data", style = "font-size:16px"),
                                                           (downloadButton("table1_download", "Download TAU Data")),
                                                           br(),
                                                           br(),
                                                           (dataTableOutput(outputId = "gene_detail_table"))
                                                    ),
                                                    column(4, offset = 4,
                                                           p("TAU+ Data", style = "font-size:16px"),
                                                           (downloadButton("table2_download", "Download TAU+ Data")),
                                                           br(),
                                                           br(),
                                                           (dataTableOutput(outputId = "gene_detail_table_tauplus")) 
                                                    )
                                                   )
                                                )
                                        )
                                )
                            )
                        )
                    ),
                    ## TAB 3
                    tabPanel(title = "Terminology",
                        p("Will update..."),
                        p("XCI:"),
                        p("Tau:"),
                        p("Skew:"),
                    )
                )
)

server <- function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
        geneofinterest1 = "",
        geneofinterest2 = "",
        diseaseofinterest1 = "",
        disease_df = ""
    )
    observeEvent(input$geneofinterest1, { rv$geneofinterest1 <- input$geneofinterest1 })
    observeEvent(input$geneofinterest2, { rv$geneofinterest2 <- input$geneofinterest2 })
    observeEvent(input$diseaseofinterest1, { rv$diseaseofinterest1 <- input$diseaseofinterest1 })
    
    ##############################
    ## DOWNLOAD HANDLERS #########
    ##############################
    ### TAB 2
    # Tau Download button for geneofinterest
    output$table1_download <- downloadHandler(
        filename =  function(){
            # Name of created file
            paste(rv$geneofinterest1, "_tau_table.csv", sep = "")
        },
        content = function(file){
            # Get the data source
            mydata <- readRDS('data_output/geneofinterest_tau_table.rds')
            write.csv(mydata, file)
        }
    )
    # Tau Plus Download button for geneofinterest
    output$table2_download <- downloadHandler(
        filename = function(){
            # Name of created file
            paste(input$genofinterest2, "_tauplus_table.csv", sep = "")
        },
        content = function(file){
            # Get the data source
            mydata <- readRDS('data_output/geneofinterest_tau_plus_table.rds')
            write.csv(mydata, file)
        }
    )
    ##############################
    ## DATA TABLES ###############
    ##############################
    ### TAB 1
    # Gene GWAS table 
    output$gene_gwas_data <- renderDataTable({
        validate(need(input$geneofinterest1,""))
        geneofinterest <- rv$geneofinterest1
        assign(("gene_stats"), create_single_gene_stats(geneofinterest, x_expr))
        df <- gene_stats$gwas_df
        df},
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2))
        )
    )
    # Disease GWAS table
    output$gene_disease_data <- renderDataTable({
        validate(need(input$diseaseofinterest1,""))
        diseaseofinterest <- rv$diseaseofinterest1
        # Get list of matching genes
        mapped_genes <- gwas_associations_v1_xonly[gwas_associations_v1_xonly$DISEASE.TRAIT == diseaseofinterest,'MAPPED_GENE']
        returned_genes_list <- c()
        returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
            ifelse(TRUE %in% grepl(gene, mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
        }
        df <- data.frame()
        for(gene in returned_genes_list){
            assign(("gene_stats"), create_single_gene_stats(gene, x_expr))
            df <- rbind(df, gene_stats$gwas_df[gene_stats$gwas_df$Disease.Trait == diseaseofinterest,])
            # ^subsets the GWAS table only for the disease of interest
        }
        df
    })
    ### TAB 2
    # TAU Table
    output$gene_detail_table <- renderDataTable({
        validate(need(input$geneofinterest2,""))
        geneofinterest <- rv$geneofinterest2
        assign(("gene_stats"), create_single_gene_stats(geneofinterest, x_expr))
        genestatdf <- data.frame(sample = gene_stats$parent_sample,
                                 cell = gene_stats$cell_type,
                                 state = gene_stats$status,
                                 tau = gene_stats$tau,
                                 skew = gene_stats$skew_values,
                                 p = gene_stats$p_values)
        saveRDS(genestatdf,'data_output/geneofinterest_tau_table.rds')
        genestatdf
    })
    # TAU PLUS Table
    # Table for TAU+ geneofinterest tau and p_values
    output$gene_detail_table_tauplus <- renderDataTable({
        validate(need(input$geneofinterest2,""))
        geneofinterest <- rv$geneofinterest2
        assign(("gene_stats_tauplus"), create_single_gene_stats(geneofinterest, x_expr_tauplus))
        genestatdf <- data.frame(sample = gene_stats_tauplus$parent_sample,
                                 cell = gene_stats_tauplus$cell_type,
                                 state = gene_stats_tauplus$status,
                                 tau_plus = gene_stats_tauplus$tau,
                                 skew = gene_stats_tauplus$skew_values,
                                 p = gene_stats_tauplus$p_values)
        saveRDS(genestatdf,'data_output/geneofinterest_tau_plus_table.rds')
        genestatdf
    })
    ##############################
    ## PLOTS/IMAGES ##############
    ##############################
    ### TAB 1
    ## Main Plot
    output$gene_pvalue <- renderPlot({
        # Save geneofinterest
        geneofinterest <- rv$geneofinterest1
        geneofinterest_df <- x_expr_mod[x_expr_mod$GENE==geneofinterest,]
        geneofinterest_max_point <- max(geneofinterest_df[,'p_value_mod_neglog10'])
        # Save disease of interest datapoints
        diseaseofinterest <- rv$diseaseofinterest1
        mapped_genes <- gwas_associations_v1_xonly[gwas_associations_v1_xonly$DISEASE.TRAIT == diseaseofinterest,'MAPPED_GENE']
        returned_genes_list <- c()
        returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
            ifelse(TRUE %in% grepl(gene, mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
        }
        disease_geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% returned_genes_list,]
        # Split data by -10log(p) > or < 300
        p_less_300 <- x_expr_mod[x_expr_mod$p_mod_flag == FALSE,]
        p_more_300 <- x_expr_mod[x_expr_mod$p_mod_flag == TRUE,]
        # Get axis breaks
        x_breaks <- seq(0, max(x_expr$start), 10000000)
        first_label <- x_breaks[1]
        rest_of_labels <- x_breaks[2:length(x_breaks)]
        x_labels <- c(paste(first_label, "bp"),
                      paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Courier", face = "bold", size = (20), hjust = 0.0), 
                         legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         #legend.position = "right", # removing legend
                         legend.position = "none",
                         axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                         #axis.title.x = element_text(family = "Helvetica", size = (18), colour = "steelblue4", face = "bold"),
                         axis.title.x = element_blank(), # Removing X-title
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.text.x = element_text(family = "Helvetica", 
                                                    colour = "steelblue4", size = (10), face = "bold", angle=0, hjust=0.5),
                         panel.background = element_rect(fill = "white"))
        genepvalue <- ggplot(data = p_less_300, aes(x=start, y=-log10(p_value_mod),
                                                    shape=p_mod_flag, label=GENE, label2=end, 
                                                    label3=ChromPos, group=1)) +
            mytheme + ggtitle("X-Chromosome Escape Profile") + 
            xlab("X-Chromosome Position") + ylab("-log10(p)") + 
            # Add PAR and CENTROMERE shading
            geom_rect(data=NULL, aes(xmin=par1_boundaries[1], xmax=par1_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=par2_boundaries[1], xmax=par2_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=centre_boundaries[1], xmax=centre_boundaries[2], ymin=0, ymax=330), 
                      fill="pink", alpha=0.25) + 
            # Data Points
            geom_point(fill = p_less_300$BandColor, size = 2) + 
            geom_point(p_more_300, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill=p_more_300$BandColor, size=2, group=2) +
            # Data points below significance
            geom_point(p_less_300[p_less_300$p_value_mod > P_SIG,],  
                       mapping=aes(x=p_less_300[p_less_300$p_value_mod > P_SIG, 'start'], 
                                   y=-log10(p_less_300[p_less_300$p_value_mod > P_SIG, 'p_value_mod']), 
                                   shape=p_less_300[p_less_300$p_value_mod > P_SIG, 'p_mod_flag']), 
                       fill=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       color=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       size=2, group=3) + 
            # Scaling and Legends
            scale_x_continuous(breaks=x_breaks, labels = x_labels) + 
            scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(1,5,20,100,300), limits = c(-0.5,400)) + 
            # Annotations
            geom_hline(yintercept = -log10(P_SIG), linetype='dotted') + 
            annotate("text", x=par1_boundaries[2]+1e6, y=400, label="PAR1", size=4, color = "steelblue", hjust=0) + 
            annotate("text", x=par2_boundaries[1]-1e6, y=400, label="PAR2", size=4, color = "steelblue", hjust=1) +
            annotate("text", x=mean(centre_boundaries[1],centre_boundaries[2])+3e6, y=400, 
                     label="CENTROMERE", size=4, color = "red", alpha = 0.5) +
            annotate("text", x = 130000000, y = -log10(P_SIG)+3, hjust=0.5, 
                     label = paste0("-log10(p) = ", format(-log10(P_SIG), digits = 3)), size = (4)) + 
            # Data points added by user reactive values: Gene of Interest
            geom_point(geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill='red', size=3, group=2) + 
            annotate("text", label = geneofinterest, x = geneofinterest_df$start, y = 0, 
                     color = "red", vjust = 2, group = 4) +
            # Data points added by user reactive values: Disease of Interest
            geom_point(disease_geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill='green', size=3, group=2) + 
            annotate("text", label = disease_geneofinterest_df$GENE, x = disease_geneofinterest_df$start, y = -0.25, 
                     color = "forestgreen", vjust = 2, group = 4) +
            # Scale shape manual (though right now this is disabled)
            scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300")) 
        genepvalue  
    })
    ## X chromosome "image"
    output$gene_pvalue_xchromosome <- renderPlot({
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Courier", face = "bold", size = (20), hjust = 0.0), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         legend.position = "none",
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                         axis.title.x = element_text(family = "Helvetica", size = (18), colour = "steelblue4", face = "bold"),
                         axis.text.x = element_text(family = "Helvetica", 
                                                    colour = "steelblue4", size = (10), face = "bold", angle=90, hjust=1, vjust = 1),
                         axis.ticks.y = element_blank(),
                         panel.background = element_rect(fill = "white"))
        xchromosome_plot <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod),
                                                    shape=p_mod_flag, label=GENE, label2=end, 
                                                    label3=ChromPos, group=1)) +
            mytheme + 
            xlab("X-Chromosome Position") + ylab(" ") + 
            # Scaling and Legends
            scale_x_continuous(breaks=x_region_breaks, labels = x_region_labels) + 
            scale_y_continuous(breaks = c(0,1), labels= c("  ","  "), limits = c(-2,0)) + 
            # Add chromosome map
            geom_segment(aes(x = colormap_df$bp_start[1], y = y_place, xend = colormap_df$bp_stop[1], yend = y_place),
                         size = chrom_size, color = colormap_df$BandColor[1], lineend = "round") + 
            geom_segment(aes(x = colormap_df$bp_start[length(colormap_df$bp_start)], y = y_place, 
                             xend = colormap_df$bp_stop[length(colormap_df$bp_stop)], yend = y_place),
                         size = chrom_size, color = colormap_df$BandColor[length(colormap_df$BandColor)], lineend = "round") + 
            chrom_segments[2:40]
        xchromosome_plot
    })
    ### TAB 2 OUTPUT
    ## Violin Plots - Gene of Interest
    output$individual_gene_tau_plot <- renderPlot({
        validate(
            need(input$geneofinterest2 !="", "Please input a gene of interest")
        )
        geneofinterest <- rv$geneofinterest2
        #geneofinterest <- "XIST" # For testing
        assign("geneofinterest_stats", create_single_gene_stats(geneofinterest, x_expr))
        # Assign object attributes to variables
        avg_p_value <- geneofinterest_stats$avg_p_value
        min_p_value <- geneofinterest_stats$min_p_value
        max_p_value <- geneofinterest_stats$max_p_value
        avg_tau_value <- geneofinterest_stats$avg_tau_value
        min_tau_value <- geneofinterest_stats$min_tau_value
        max_tau_value <- geneofinterest_stats$max_tau_value
        skew_values <- geneofinterest_stats$skew_values
        avg_skew_value <- geneofinterest_stats$avg_skew
        min_skew_value <- geneofinterest_stats$min_skew
        max_skew_value <- geneofinterest_stats$max_skew
        perc_samples_esc <- geneofinterest_stats$perc_samples_esc
        # Assign object attributes to variables with skew < 25%
        assign("geneofinterest_tauplus_stats", create_single_gene_stats(geneofinterest, x_expr_tauplus))
        skew_tauplus <- geneofinterest_tauplus_stats$skew_values
        avg_skew_tauplus <- geneofinterest_tauplus_stats$avg_skew
        min_skew_tauplus <- geneofinterest_tauplus_stats$min_skew
        max_skew_tauplus <- geneofinterest_tauplus_stats$max_skew
        tau_tauplus <- geneofinterest_tauplus_stats$tau
        avg_tauplus <- geneofinterest_tauplus_stats$avg_tau_value
        min_tauplus <- geneofinterest_tauplus_stats$min_tau_value
        max_tauplus <- geneofinterest_tauplus_stats$max_tau_value
        # Create tautable
        tautable <- data.frame("tau__tauplus" = c(rep("TAU",length(geneofinterest_stats$tau)),
                                                  rep("TAU+",length(skew_tauplus))),
                               "tau__tauskewvalues25" = c(geneofinterest_stats$tau,tau_tauplus))
        # If there are two or less entries for tau+, remove the
        # tau+ entries from the table (it's not enough info to display)
        if(sum(tautable$tau__tauplus == "TAU+") <= 2) 
        {tautable <- tautable[tautable$tau__tauplus == "TAU",]}
        # My Theme
        mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (22), hjust = 0.5), 
                         legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         legend.position = "right",
                         axis.title = element_text(family = "Courier", size = (22), colour = "steelblue4", face = "bold"),
                         axis.text.x = element_text(family = "Helvetica", colour = "black", size = (18), face = "bold"),
                         axis.title.y = element_blank())
        # Locations for annotations. Some locations will vary depending on:
        # ---if we display the TAU+ violin plot
        # ---if the violin plot is top-heavy or bottom-heavy
        x_center = ifelse(sum(tautable$tau__tauplus == "TAU+") >= 3, 1.5, 1)
        labelx = 1.5
        labely_tau = ifelse(avg_tau_value < 0.25, 0.48, 0.1)
        labely_tauplus = ifelse(avg_tauplus < 0.25, 0.48, 0.1)
        # Create Plot
        geneofinterest_tauplot <- ggplot(tautable, 
              aes(x = tau__tauplus, y = tau__tauskewvalues25, fill = tau__tauplus)) +
            geom_violin(alpha = 0.5) + 
            mytheme + 
            ylim(-0.05,.5) + 
            xlab(geneofinterest) + 
            # Add boxplots
            geom_boxplot(width = 0.03, fill = "white") + 
            # Annotate with tau values and p-values
            annotate("text", x=0.45, y=c(labely_tau,labely_tau-0.025,labely_tau-0.05), 
                     label=c(paste0('avg tau: ', sprintf("%1.2f", avg_tau_value)), 
                             paste0('min tau: ', sprintf("%1.2f", min_tau_value)),
                             paste0('max tau: ', sprintf("%1.2f", max_tau_value))),
                     family = 'Courier', color = "steelblue", size = 4, hjust = 0) + 
            annotate("text", x=x_center,y=-.03,
                     label=paste0('escapes in ',sprintf("%3.1f",perc_samples_esc*100),'% of samples'),
                     family = 'Courier', color = "black", size = 6, hjust = 0.5, alpha = 0.5) + 
            scale_fill_manual("", values = c("cornflowerblue","purple"))
        # Remove legend if only one violin plot will be displayed
        if(sum(tautable$tau__tauplus == "TAU+") < 2){
            geneofinterest_tauplot <- geneofinterest_tauplot +
                theme(legend.position = "none")
        }
        # If there is enough information to display tau+ data,
        # display it. Criteria: TAU+ needs at least 2 entries.
        if(sum(tautable$tau__tauplus == "TAU+") >= 2){
            geneofinterest_tauplot <- geneofinterest_tauplot + 
                annotate("text", x=2.55, y=c(labely_tauplus,labely_tauplus-0.025,labely_tauplus-0.05), 
                         label=c(paste0('avg tau+: ', sprintf("%1.2f", avg_tauplus)), 
                                 paste0('min tau+: ', sprintf("%1.2f", min_tauplus)),
                                 paste0('max tau+: ', sprintf("%1.2f", max_tauplus))),
                         family = 'Courier', color = "purple", size = 4, hjust = 1) + 
                scale_fill_manual("TAU vs TAU+", values = c("cornflowerblue","purple"), 
                                  labels=c("For all skew values", "Skew < 25%")) + 
                scale_x_discrete(limits = c("TAU","TAU+"))
        }
        geneofinterest_tauplot
    })
    ##################
    ## TAB 3 OUTPUT
    ##################
}

#This lets script know its a shiny app
shinyApp(server = server, ui = ui)