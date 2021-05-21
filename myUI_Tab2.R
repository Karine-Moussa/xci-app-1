# Invidivual Gene Search
all_genes <- unique(unique(c(x_expr_mod$GENE, TukGTExMod$`Gene name`,cott_carr_will_df$gene),
                    kat_lin_df$gene))
TAB2 <- tabPanel(title = "Individual Gene Search",
                 # Create a layout with a sidebar and main area
                 sidebarLayout(
                     sidebarPanel(
                         h3("Observing XCI escape calls per Gene"),
                         autocomplete_input("geneofinterest2", "Gene of Interest:", all_genes, value = ""),
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
                                            column(5,
                                                   p("TAU Data (GEUVADIS lymphoblast)", style = "font-size:16px"),
                                                   (downloadButton("table1_download", "Download TAU Data")),
                                                   br(),
                                                   br(),
                                                   (dataTableOutput(outputId = "gene_detail_table"))
                                            ),
                                            column(5, offset = 2,
                                                   p("Escape States", style = "font-size:16px"), 
                                                   (downloadButton("individual_gene_escape_download", "Download Escape Data")),
                                                   br(),
                                                   br(),
                                                   (dataTableOutput(outputId = "ind_escape_states_table"))
                                            )
                                        )
                                 )
                             )
                         )
                     )
                 )
)
