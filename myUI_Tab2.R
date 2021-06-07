# Invidivual Gene Search
TAB2 <- tabPanel(title = "Individual Gene Search",
                 # Create a layout with a sidebar and main area
                 sidebarLayout(
                     sidebarPanel(
                         h3("Observing XCI escape calls across studies"),
                         #selectizeInput("geneofinterest2", "Gene of Interest:", c("", c("ASTML","MECP2")), multiple = TRUE),
                         selectizeInput("geneofinterest2", "Gene of Interest:", c("", all_genes), multiple = TRUE),
                         actionButton("resetButton2", "Clear Genes"),
                         br(),
                         br(),
                         strong("Directions for Use", style = "font-size:12px"),br(),
                         em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                         em("---Examples: XIST, ZBED1, ASMTL", style = "font-size:12px"),br(),
                         br(),
                         em(paste("Last published:", publication_date), style = "font-size:12px;color:grey")
                     ),
                     mainPanel(
                         # Show message if no genes are displayed 
                         span(textOutput("pleaseInput3"), style="font-size:18px;font-style:italic"),
                         # Escape States Table
                         conditionalPanel(
                             condition = "output.geneTableStatus2",
                             p("Escape States", style = "font-size:16px;font-weight:bold"), 
                             (downloadButton("individual_gene_escape_download", "Download Table")),
                             br(),
                             br(),
                             withSpinner(dataTableOutput(outputId = "ind_escape_states_table"), type = 1, size = 1),
                         )
                     )
                 )
)
