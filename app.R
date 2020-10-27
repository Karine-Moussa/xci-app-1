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

# Load files and pre-processed data
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

# Change parameters into a categorical bins
attach(x_expr)
XCI=as.factor(XCI)
GENE=as.factor(GENE)

# Default values

# Functions 
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
                                autocomplete_input("geneofinterest1", "Gene of Interest:", GENE, value = ""),
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
                                plotlyOutput(outputId = "gene_tau", height = "600px")
                            )
                        )
                    ),
                    # TAB 2
                    tabPanel(title = "Individual Gene Frequencies",
                        # Create a layout with a sidebar and main area
                        sidebarLayout(
                            sidebarPanel(
                                h3("Observing XCI escape calls per Gene"),
                                autocomplete_input("geneofinterest2", "Gene of Interest:", GENE, value = ""),
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
                                plotOutput(outputId = "individual_gene_tau_plot")
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
    output$gene_tau <- renderPlotly({
        # This is the interactive plot which covers the default plot
        #validate(
        #    need(input$geneofinterest !="", "Please input a gene of interest")
        #)
        geneofinterest <- rv$geneofinterest1
        gene_color <- ifelse(GENE==geneofinterest,"blue",
                        ifelse(GENE!=geneofinterest,"grey",
                            "black"))
        genetau_color = gene_color
        mytheme <- theme(axis.text.x=element_blank())
        genetau_plot <- ggplot(data = x_expr, aes(x=GENE, y=tau)) + 
            ggtitle('Gene vs. Tau') + 
            xlab("Gene") + ylab("Tau") + 
            mytheme + 
            geom_point(color=genetau_color)
        # next steps: make this plot look better
        ggplotly(genetau_plot)
    })
    
    ##################
    ## TAB 2 OUTPUT
    ##################
    output$individual_gene_tau_plot <- renderPlot({
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
        geneofinterest_tauplot <- geneofinterest_tauplot + 
            geom_boxplot(width = 0.01, fill = "gray")
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