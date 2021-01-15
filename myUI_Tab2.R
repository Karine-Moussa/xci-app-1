# Invidivual Gene Search
TAB2 <- tabPanel(title = "Individual Gene Search",
                 # Create a layout with a sidebar and main area
                 sidebarLayout(
                     sidebarPanel(
                         h3("Observing XCI escape calls per Gene"),
                         autocomplete_input("geneofinterest2", "Gene of Interest:", c(unique(x_expr_mod[,"GENE"])), value = ""),
                         br(),
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
                         p("Gene =", span(a("268 X-Chromosome Genes (link currently not working)", href="null", target="_blank")), style = "font-size:12px"),
                         br(),
                         br(),
                         p("GEUVIDAS genome build: hg38", style = "font-size:12px"),
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
                                            )
                                        )
                                 )
                             )
                         )
                     )
                 )
)