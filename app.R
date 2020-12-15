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

### Source Data and Functions ###
source("utilities/format_input_data.R", local = TRUE)
source("utilities/create_global_variables.R", local = TRUE)
source("utilities/x_expr_mods.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/format_plot_aesthetics.R", local = TRUE)

### Load files and pre-processed data
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

### Default values #####

# shinyapp
ui <- fluidPage(title = "XCI Data",
                tabsetPanel(
                    # TAB 1
                    tabPanel(title = "All Escape Expressions",
                        # Create a layout with a sidebar and main area ----
                        sidebarLayout(
                            # Create a sidebar panel containing input controls ----
                            sidebarPanel(
                                h3("Observing XCI escape calls from 102 samples"),
                                autocomplete_input("geneofinterest1", "Gene of Interest:", c(unique(x_expr_mod[,"GENE"])), value = ""),
                                br(),
                                strong("Directions for Use", style = "font-size:12px"),br(),
                                em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                                em("---Examples: XIST, ZBED1, ASMTL", style = "font-size:12px"),br(),
                                em("---Hover over data points for more information", style = "font-size:12px"),br(),
                                br(),
                                strong("Parameters"),
                                p("Tau = (Xi Expression)/(Total Expression)", style = "font-size:12px"),
                                p("Gene =", span(a("268 X-Chromosome Genes", href="null", target="_blank")), style = "font-size:12px"),
                                br(),
                                br(),
                                em("Data was produced by ", span(a("XCIR Package", href="https://www.bioconductor.org/packages/release/bioc/html/XCIR.html", target="_blank")), style = "font-size:12px"),
                                br(),
                            ),
                            # Create plot and Action Buttons in Main Panel
                            mainPanel(
                                plotOutput(outputId = "gene_pvalue", height = "350px"),
                                img(src = "xchrom-850bp-margin.png", width="600px")
                            )
                        )
                    ),
                    # TAB 2
                    tabPanel(title = "Individual Gene Frequencies",
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
                                em("Data was produced by ", span(a("XCIR Package", href="https://www.bioconductor.org/packages/release/bioc/html/XCIR.html", target="_blank")), style = "font-size:12px"),
                                br(),
                            ),
                            mainPanel(
                                (plotOutput(outputId = "individual_gene_tau_plot")),
                                fluidRow(
                                    column(12, "",
                                           fixedRow(
                                            column(4,
                                                (htmlOutput("table1_label")),
                                                (downloadButton("table1_download", "Download TAU Data")),
                                                (dataTableOutput(outputId = "gene_detail_table"))
                                            ),
                                            column(4, offset = 4,
                                                (htmlOutput("table2_label")),
                                                (dataTableOutput(outputId = "gene_detail_table_tauplus")) 
                                            )
                                           )
                                        )
                                )
                            )
                        )
                    ),
                    # TAB 3
                    tabPanel(title = "scratch work",
                             plotOutput("tau_hist"),
                    )
                )
)

server <- function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
        tau_data = x_expr_mod$tau,
        geneofinterest1 = "",
        geneofinterest2 = ""
    )
    observeEvent(input$geneofinterest1, { rv$geneofinterest1 <- input$geneofinterest1 })
    observeEvent(input$geneofinterest2, { rv$geneofinterest2 <- input$geneofinterest2 })

    ##################
    ## TAB 1 OUTPUT
    ##################
    output$gene_pvalue <- renderPlot({
        # Save geneofinterest
        geneofinterest <- rv$geneofinterest1
        geneofinterest_df <- x_expr_mod[x_expr_mod$GENE==geneofinterest,]
        geneofinterest_max_point <- max(geneofinterest_df[,'p_value_mod_neglog10'])
        # Split data by -10log(p) > or < 300
        p_less_300 <- x_expr_mod[x_expr_mod$p_mod_flag == FALSE,]
        p_more_300 <- x_expr_mod[x_expr_mod$p_mod_flag == TRUE,]
        # Create xchrom color map df
        colormap_df <- data.frame(xchrom_map_colored[,c('bp_start','bp_stop','BandColor')])
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Courier", face = "bold", size = (18), hjust = 0.0), 
                         legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         legend.position = "right",
                         axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4", face = "bold"),
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10), face = "bold", angle=45, hjust=1),
                         panel.background = element_rect(fill = "white"))
        genepvalue <- ggplot(data = p_less_300, aes(x=start, y=-log10(p_value_mod),
                                                    shape=p_mod_flag, label=GENE, label2=end, 
                                                    label3=ChromPos, group=1)) +
            mytheme + ggtitle("X-Chromosome Escape Profile") + 
            xlab("X-Chromosome Position (bp)") + ylab("-log10(p)") + 
            geom_rect(data=NULL, xmin=par1_boundaries[1], xmax=par1_boundaries[2], ymin=0, ymax=330, 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, xmin=par2_boundaries[1], xmax=par2_boundaries[2], ymin=0, ymax=330, 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=centre_boundaries[1], xmax=centre_boundaries[2], ymin=0, ymax=330), 
                      fill="pink", alpha=0.25) + 
            # Data Points
            geom_point(fill = p_less_300$BandColor, size = 2) + 
            geom_point(p_more_300, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill=p_more_300$BandColor, size=2, group=2) +
            # Data points below signifance
            geom_point(p_less_300[p_less_300$p_value_mod > P_SIG,],  
                       mapping=aes(x=p_less_300[p_less_300$p_value_mod > P_SIG, 'start'], 
                                   y=-log10(p_less_300[p_less_300$p_value_mod > P_SIG, 'p_value_mod']), 
                                   shape=p_less_300[p_less_300$p_value_mod > P_SIG, 'p_mod_flag']), 
                       fill=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       color=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       size=2, group=3) + 
            # Scaling and Legends
            scale_x_continuous(breaks=seq(1, max(x_expr$start), 10000000)) + 
            scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(1,5,20,100,300)) + 
            # Annotations
            geom_hline(yintercept = -log10(P_SIG), linetype='dotted') + 
            annotate("text", x=par1_boundaries[2]+1e6, y=400, label="PAR1", size=4, color = "steelblue", hjust=0) + 
            annotate("text", x=par2_boundaries[1]-1e6, y=400, label="PAR2", size=4, color = "steelblue", hjust=1) +
            annotate("text", x = 130000000, y = -log10(P_SIG)+3, hjust=0.5, 
                     label = paste0("-log10(p) = ", format(-log10(P_SIG), digits = 3)), size = (4)) + 
            # Data points added by user reactive values
            geom_point(geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill='red', size=2, group=2) + 
            geom_text(data = geneofinterest_df[geneofinterest_df$p_value_mod_neglog10==geneofinterest_max_point,],
                      mapping=aes(x=start,y=-log10(p_value_mod)), colour='red',vjust=-1, group=4) + 
            # Scale shape manual
            scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300")) + 
########## !! WILL DEFINITELY CHANGE THIS!!! ####################
            geom_segment(aes(x = colormap_df$bp_start[1], y = -0.6, xend = colormap_df$bp_stop[1], yend = -0.6),
                            size = 4, color = colormap_df$BandColor[1], lineend = "round") + 
            geom_segment(aes(x = colormap_df$bp_start[length(colormap_df$bp_start)], y = -0.6, 
                             xend = colormap_df$bp_stop[length(colormap_df$bp_stop)], yend = -0.6),
                         size = 4, color = colormap_df$BandColor[length(colormap_df$BandColor)], lineend = "round") + 
            geom_segment(aes(x = 4400001, y = -0.6, xend = 6100000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 6100001, y = -0.6, xend = 9600000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 9600001, y = -0.6, xend = 17400000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 17400001, y = -0.6, xend = 19200000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 19200001, y = -0.6, xend = 21900000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 21900001, y = -0.6, xend = 24900000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 24900001, y = -0.6, xend = 29300000, yend = -0.6), size = 4, color = 'gray0') + 
            geom_segment(aes(x = 29300001, y = -0.6, xend = 31500000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 31500001, y = -0.6, xend = 37800000, yend = -0.6), size = 4, color = 'gray0') + 
            geom_segment(aes(x = 37800001, y = -0.6, xend = 42500000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 42500001, y = -0.6, xend = 47600000, yend = -0.6), size = 4, color = 'gray20') + 
            geom_segment(aes(x = 47600001, y = -0.6, xend = 50100000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 50100001, y = -0.6, xend = 54800000, yend = -0.6), size = 4, color = 'gray60') + 
            geom_segment(aes(x = 54800001, y = -0.6, xend = 58100000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 58100001, y = -0.6, xend = 61000000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 61000001, y = -0.6, xend = 63800000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 63800001, y = -0.6, xend = 65400000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 65400001, y = -0.6, xend = 68500000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 68500001, y = -0.6, xend = 73000000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 73000001, y = -0.6, xend = 74700000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 74700001, y = -0.6, xend = 76800000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 76800001, y = -0.6, xend = 85400000, yend = -0.6), size = 4, color = 'gray0') + 
            geom_segment(aes(x = 85400001, y = -0.6, xend = 87000000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 87000001, y = -0.6, xend = 92700000, yend = -0.6), size = 4, color = 'gray0') + 
            geom_segment(aes(x = 92700001, y = -0.6, xend = 94300000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 94300001, y = -0.6, xend = 99100000, yend = -0.6), size = 4, color = 'gray20') + 
            geom_segment(aes(x = 99100001, y = -0.6, xend = 103300000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 103300001, y = -0.6, xend = 104500000, yend = -0.6), size = 4, color = 'gray40') + 
            geom_segment(aes(x = 104500001, y = -0.6, xend = 109400000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 109400001, y = -0.6, xend = 117400000, yend = -0.6), size = 4, color = 'gray20') + 
            geom_segment(aes(x = 117400001, y = -0.6, xend = 121800000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 121800001, y = -0.6, xend = 129500000, yend = -0.6), size = 4, color = 'gray0') + 
            geom_segment(aes(x = 129500001, y = -0.6, xend = 131300000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 131300001, y = -0.6, xend = 134500000, yend = -0.6), size = 4, color = 'gray60') + 
            geom_segment(aes(x = 134500001, y = -0.6, xend = 138900000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 138900001, y = -0.6, xend = 141200000, yend = -0.6), size = 4, color = 'gray20') + 
            geom_segment(aes(x = 141200001, y = -0.6, xend = 143000000, yend = -0.6), size = 4, color = 'gray80') + 
            geom_segment(aes(x = 143000001, y = -0.6, xend = 148000000, yend = -0.6), size = 4, color = 'gray0')
        genepvalue  
    })
    
    ##################
    ## TAB 2 OUTPUT
    ##################
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
    # Label for first table
    output$table1_label <- renderText({
        validate(need(input$geneofinterest2,""))
        # Create html
        formatedFont1 <- sprintf('<font color="%s">%s</font>',"black","TAU Data")
        # Append to text to show
        outTxt <- paste(formatedFont1, collapse=' ')
        outTxt
        })
    # Download button for geneofinterest
    output$table1_download <- downloadHandler(
        filename = function(){
            # Name of created file
            gene <- rv$genofinterest2
            paste0(gene, "_tau_table.csv")
        },
        content = function(file){
            # Get the data source
            mydata <- readRDS('data_output/geneofinterest_tau_table.rds')
            write.csv(mydata, file)
        }
    )
    # Table for geneofinterest tau and p_vales
    output$gene_detail_table <- renderDataTable({
        validate(need(input$geneofinterest2,""))
        geneofinterest <- rv$geneofinterest2
        assign(("gene_stats"), create_single_gene_stats(geneofinterest, x_expr))
        assign(("gene_stats_tauplus"), create_single_gene_stats(geneofinterest, x_expr_tauplus))
        genestatdf <- data.frame(sample = gene_stats$parent_sample,
                                 cell = gene_stats$cell_type,
                                 state = gene_stats$status,
                                 tau = gene_stats$tau,
                                 skew = gene_stats$skew_values,
                                 p = gene_stats$p_values)
        saveRDS(genestatdf, "data_output/geneofinterest_tau_table.rds")
    })
    # Label for second table
    output$table2_label <- renderText({
        validate(need(input$geneofinterest2,""))
        # Create html
        formatedFont1 <- sprintf('<font color="%s">%s</font>',"black","TAU+ Data")
        # Append to text to show
        outTxt <- paste(formatedFont1, collapse=' ')
        outTxt
    })
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
    })
    
    ##################
    ## TAB 3 OUTPUT
    ##################
    output$tau_hist <- renderPlot({

        hist(rv$tau_data, breaks = 30, col = "grey", border = "white",
             xlab = "Xi/TotalExpression",
             main = "TAU")
        # next step ^ ggplot this
    })
}

#This lets script know its a shiny app
shinyApp(server = server, ui = ui)