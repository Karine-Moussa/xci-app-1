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
                                plotlyOutput(outputId = "gene_pvalue", height = "500px"),
                                img(src = "xchrom-850bp.png", width = "900px")
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
                    tabPanel(title = "scratch",
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
    output$gene_pvalue <- renderPlotly({
        # This is the interactive plot which covers the default plot
        #validate(
        #    need(input$geneofinterest !="", "Please input a gene of interest")
        #)
        geneofinterest <- rv$geneofinterest1
        gene_color <- ifelse(GENE==geneofinterest,"blue",
                        ifelse(GENE!=geneofinterest,"grey",
                            "black"))
        genepvalue_color = gene_color
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Courier", face = "bold", size = (18), hjust = 0.0), 
                         legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica", size = (14)), 
                         legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica", size = (14)), 
                         axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4", face = "bold"),
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10), face = "bold", angle=45, hjust=1),
                         panel.grid.major = element_blank(),
                         panel.background = element_rect(fill = "white"))
        genepvalue <- ggplot(data = x_expr_mod, aes(x = start, y = -log10(p_value_mod),
              label=GENE, label2=start, label3=end, label4=ChromPos, group=1)) + 
            mytheme + 
            ggtitle("X-Chromosome Escape Calls") + 
            xlab("X-Chromosome Position (bp)") + ylab("-log10(p)") + 
            geom_rect(data=NULL, aes(xmin=par1_boundaries[1], xmax=par1_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=par2_boundaries[1], xmax=par2_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=centre_boundaries[1], xmax=centre_boundaries[2], ymin=0, ymax=330), 
                      fill="pink", alpha=0.25) + 
            geom_point(shape = shape_vector, colour = x_expr_mod$BandColor, size = 2) + 
            geom_hline(yintercept = -log10(P_SIG), linetype='dotted') + 
            annotate("text", x = 135285791, y = -log10(P_SIG)+4.0,  
                     label = paste0("p = ", format(-log10(P_SIG), digits = 3)), size = (3)) + 
            annotate("text", x=7147291, y=320, label="PAR1", size=4, color = "steelblue") + 
            annotate("text", x=151396566, y=320, label="PAR2", size=4, color = "steelblue") + 
            scale_x_continuous(breaks=seq(1, max(x_expr$start), 10000000)) + 
            scale_y_continuous(limits = c(0,330))
        genepvalue
        ggplotly(genepvalue, tooltip = c("label", "label2", "label3","label4"))
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
        avg_p_value <- geneofinterest_stats$avg_p_value
        min_p_value <- geneofinterest_stats$min_p_value
        max_p_value <- geneofinterest_stats$max_p_value
        avg_tau_value <- geneofinterest_stats$avg_tau_value
        min_tau_value <- geneofinterest_stats$min_tau_value
        max_tau_value <- geneofinterest_stats$max_tau_value
        perc_samples_esc <- geneofinterest_stats$perc_samples_esc
        tautable <- data.frame("gene" = rep(geneofinterest_stats$gene_name,length(geneofinterest_stats$tau)),
                               "tau" = geneofinterest_stats$tau,
                               "p_value" = geneofinterest_stats$p_value)
        
        mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (22), hjust = 0.5), 
                        legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica", size = (16)), 
                        legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica", size = (16)), 
                        axis.title = element_text(family = "Helvetica", size = (16), colour = "steelblue4", face = "bold"),
                        axis.text.x = element_text(family = "Courier", colour = "cornflowerblue", size = (22), face = "bold"),
                        axis.text.y = element_text(family = "Courier", colour = "cornflowerblue", size = (14), face = "bold"))
        labelx = 1.5
        labely = 0.25
        
        geneofinterest_tauplot <- ggplot(tautable, aes(x = gene, y = tau)) +
            ylim(-0.05,.5) + 
            geom_violin(fill = "cornflowerblue", alpha = 0.5) + 
            ggtitle("Tau Distribution") + 
            xlab("Gene") + 
            ylab("Tau") + mytheme
        geneofinterest_tauplot <- geneofinterest_tauplot + 
            # Add boxplots
            geom_boxplot(width = 0.03, fill = "white") + 
            # Annotate with tau values and p-values
            annotate("text", x=labelx-1.0, y=c(labely,labely-0.05,labely-0.1), 
                     label=c(paste0('avg tau: ', sprintf("%1.3f", avg_tau_value)), 
                            paste0('min tau: ', sprintf("%1.3f", min_tau_value)),
                            paste0('max tau: ', sprintf("%1.3f", max_tau_value))),
                     family = 'Courier', color = "steelblue", size = 6, hjust = 0) + 
            annotate("text", x=1,y=-.03,
                     label=paste0('escapes in ',sprintf("%3.1f",perc_samples_esc*100),'% of samples'),
                     family = 'Courier', color = "black", size = 6, hjust = 0.5, alpha = 0.5) +
            annotate("text", x=labelx, y=c(labely,labely-0.05,labely-0.1), 
                     label=c(paste0('avg p-value: ', sprintf("%1.3f", avg_p_value)), 
                             paste0('min p-value: ', sprintf("%1.3f", min_p_value)),
                             paste0('max p-value: ', sprintf("%1.3f", max_p_value))),
                     family = 'Courier', color = "black", size = 6, hjust = 1)
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