# Invidivual Gene Search
TAB2 <- tabPanel(title = "Individual Gene Search",
                 # Create a layout with a sidebar and main area
                 sidebarLayout(
                     sidebarPanel(
                         h3("Observing XCI escape calls across studies"),
                         selectizeInput("geneofinterest2", "Gene of Interest:", c("", all_genes), multiple = TRUE),
                         actionButton("resetButton2", "Clear Genes"),
                         br(),
                         br(),
                         #conditionalPanel(
                         #    condition = "input.geneofinterest2 != ''",
                         #    selectInput("tauStudy", "View Tau Data:",
                         #                c(" " = "empty", "GEUVADIS (lymphoblast)" = "study1")
                         #    ),
                         #),
                         #br(),
                         strong("Directions for Use", style = "font-size:12px"),br(),
                         em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                         em("---Examples: XIST, ZBED1, ASMTL", style = "font-size:12px"),br(),
                         br(),
                         strong("TAU and TAU+", style = "font-size:12px"),br(),
                         em("---All samples  are included in TAU", style = "font-size:12px"),br(),
                         em("---Those samples from higher skew samples (skew < 25%)  are represented in TAU+", style = "font-size:12px"),br(),
                         br(),
                         strong("Parameters"),
                         p("Tau = (Xi Expression)/(Total Expression)", style = "font-size:12px"),
                         p("Gene = X-Chromosome genes from all studies",style = "font-size:12px"),
                         br(),
                         em(paste("Last published:",publication_date), style = "font-size:12px;color:grey")
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
                         ),
                         # Show plot / Tau table if study has this information
                         conditionalPanel(
                             condition = "input.tauStudy == 'study1'",
                             br(),
                             br(),
                             #p("Tau Data", style = "font-size:16px;font-weight:bold"), 
                             # Plot Output
                             #(plotOutput(outputId = "individual_gene_tau_plot")),
                             # Plot Tau Table
                             #fluidRow(
                             #    column(12, "",
                             #           fixedRow(
                             #               column(6,
                             #                      p("TAU Data (GEUVADIS lymphoblast)", style = "font-size:16px"),
                             #                      (downloadButton("table1_download", "Download TAU Data")),
                             #                      br(),
                             #                      br(),
                             #                      (dataTableOutput(outputId = "gene_detail_table"))
                             #               )
                             #          )
                             #    )
                             #)
                         )
                     )
                 )
)
