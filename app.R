# Karine Moussa

# App Settings
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Packages
library(shiny)
library(dqshiny)
library(ggplot2)
library(plotly)

### Set Working Directory ###
# For now, just setting it to where this script is saved
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)

### Source Functions ###
source("functions/format_input_data.R")
source("functions/create_gene_objects.R")

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
                                autocomplete_input("geneofinterest1", "Gene of Interest:", c(unique(x_expr[,"GENE"])), value = ""),
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
                                plotlyOutput(outputId = "gene_pvalue", height = "600px")
                            )
                        )
                    ),
                    # TAB 2
                    tabPanel(title = "Individual Gene Frequencies",
                        # Create a layout with a sidebar and main area
                        sidebarLayout(
                            sidebarPanel(
                                h3("Observing XCI escape calls per Gene"),
                                autocomplete_input("geneofinterest2", "Gene of Interest:", c(unique(x_expr[,"GENE"])), value = ""),
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
                                plotOutput(outputId = "individual_gene_pvalue_plot")
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
        tau_data = x_expr$tau,
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
        mytheme <- theme(axis.text.x = element_blank())
        genepvalue <- ggplot(data = x_expr, aes(x = reorder(GENE, start), y = -log10(p_value))) + 
            ylim(0, 400) + 
            labs("GENE vs. -10log(p_value)", x = "Gene", y = "-log10(p_value)") + 
            mytheme + 
            geom_point(color=genepvalue_color)
        genepvalue
        ggplotly(genetau, tooltip = c("x","y","text"))
    })
    
    ##################
    ## TAB 2 OUTPUT
    ##################
    output$individual_gene_pvalue_plot <- renderPlot({
        validate(
            need(input$geneofinterest2 !="", "Please input a gene of interest")
        )
        geneofinterest <- rv$geneofinterest2
        assign("geneofinterest_stats", create_single_gene_stats(geneofinterest))
        geneofinterest_tautable <- data.frame("gene" = rep(geneofinterest_stats$gene_name,length(geneofinterest_stats$tau)),
                                              "tau" = geneofinterest_stats$tau)
        geneofinterest_tauplot <- ggplot(geneofinterest_tautable, aes(x = gene, y = tau)) +
            ylim(0,.5) + 
            geom_violin(fill = "purple") + 
            labs("Tau distribution per Gene", x = "Gene", y = "Tau") 
            #geom_text(label=paste0('p-value = ', p_value),
                      #family = 'Helvetica', size = 4)
        geneofinterest_tauplot <- geneofinterest_tauplot + 
            geom_boxplot(width = 0.03, fill = "white")
        geneofinterest_tauplot
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