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
source("functions/format_input_data.R", local = TRUE)
source("functions/create_global_variables.R", local = TRUE)
source("functions/x_expr_mods.R", local = TRUE)
source("functions/create_gene_objects.R", local = TRUE)
source("functions/format_plot_aesthetics.R", local = TRUE)

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
                                (dataTableOutput(outputId = "gene_detail_table"))
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
            scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300"))
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
        assign("geneofinterest_stats", create_single_gene_stats(geneofinterest))
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
        # Collect data for skew values > 25%
        skew_skewvalues25 <- (x_expr[x_expr$GENE==geneofinterest & x_expr$f > 0.25,"f"])
        avg_skew_skewvalues25 <- mean(skew_skewvalues25)
        min_skew_skewvalues25 <- ifelse(is.na(skew_skewvalues25), 0, min(skew_skewvalues25))
        max_skew_skewvalues25 <- ifelse(is.na(skew_skewvalues25), 0, max(skew_skewvalues25))
        tau_skewvalues25 <- (x_expr[x_expr$GENE==geneofinterest & x_expr$f > 0.25,"tau"])
        avg_tau_skewvalues25 <- mean(tau_skewvalues25)
        min_tau_skewvalues25 <- ifelse(is.na(tau_skewvalues25)[1], 0, min(tau_skewvalues25))
        max_tau_skewvalues25 <- ifelse(is.na(tau_skewvalues25)[1], 0, max(tau_skewvalues25))
        # Create tautable
        tautable <- data.frame("tau__tauplus" = c(rep("TAU",length(geneofinterest_stats$tau)),
                                    rep("TAU+",length(skew_skewvalues25))),
                               "tau__tauskewvalues25" = c(geneofinterest_stats$tau,tau_skewvalues25))
        
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
        labely_tauplus = ifelse(avg_tau_skewvalues25 < 0.25, 0.48, 0.1)
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
                     family = 'Courier', color = "steelblue", size = 6, hjust = 0) + 
            annotate("text", x=x_center,y=-.03,
                     label=paste0('escapes in ',sprintf("%3.1f",perc_samples_esc*100),'% of samples'),
                     family = 'Courier', color = "black", size = 6, hjust = 0.5, alpha = 0.5) + 
            scale_fill_manual("", values = c("cornflowerblue","purple"), 
                              labels=c("For all skew values", "Skew > 25%")) 
        # If there is enough information to display tau+ data,
        # display it. Criteria: TAU+ needs at least 2 entries.
        if(sum(tautable$tau__tauplus == "TAU+") >= 2){
            geneofinterest_tauplot <- geneofinterest_tauplot + 
                annotate("text", x=2.55, y=c(labely_tauplus,labely_tauplus-0.025,labely_tauplus-0.05), 
                         label=c(paste0('avg tau+: ', sprintf("%1.2f", avg_tau_skewvalues25)), 
                                 paste0('min tau+: ', sprintf("%1.2f", min_tau_skewvalues25)),
                                 paste0('max tau+: ', sprintf("%1.2f", max_tau_skewvalues25))),
                         family = 'Courier', color = "purple", size = 6, hjust = 1) + 
                scale_fill_manual("TAU vs TAU+", values = c("cornflowerblue","purple"), 
                                  labels=c("For all skew values", "Skew > 25%")) + 
                scale_x_discrete(limits = c("TAU","TAU+"))
        }
        geneofinterest_tauplot
    })
    output$individual_gene_pvalue_plot <- renderPlot({plot(iris)})
    output$gene_detail_table <- renderDataTable({
        validate(need(input$geneofinterest2,""))
        geneofinterest <- rv$geneofinterest2
        assign(("gene_stats"), create_single_gene_stats(geneofinterest))
        genestatdf <- data.frame(sample = gene_stats$parent_sample,
                                 cell_type = gene_stats$cell_type,
                                 status = gene_stats$status,
                                 tau = gene_stats$tau,
                                 p_value = gene_stats$p_values)
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