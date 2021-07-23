TAB3 <- tabPanel(title = "Terminology",
                 includeHTML("terminology.html"),
                 #tags$iframe(style="height:1000px; width:75%", 
                 #            src="terminology_with_cite.pdf"),
                 # for testing
                 sidebarPanel(
                     h3("Observing TAU and TAU+ in GEUVADIS data set"),
                     selectizeInput("geneofinterest3", "Gene of Interest:", c("", study1_genes), multiple = FALSE),
                     br(),
                     strong("Directions for Use", style = "font-size:12px"),br(),
                     em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                     em("---Examples: AKAP17A, ZBED1, ASMTL", style = "font-size:12px"),br(),
                     br(),
                     strong("TAU and TAU+", style = "font-size:12px"),br(),
                     em("---All samples  are included in TAU", style = "font-size:12px"),br(),
                     em("---Those samples from higher skew samples (skew >25:75)  are represented in TAU+", style = "font-size:12px"),br(),
                     br(),
                     strong("Parameters"),
                     p("Tau = (Xi Expression)/(Total Expression)", style = "font-size:12px"),
                     p("Gene = 268 Genes from GEUVADIS lymphoblast samples",style = "font-size:12px"),
                     br(),
                     em(paste("Last published:", publication_date), style = "font-size:12px;color:grey")
                 ), # don't forget to recomment the comma
                 mainPanel(
                     # Show message if no genes are displayed
                     span(textOutput("pleaseInput4"), style="font-size:18px;font-style:italic"),
                     # Show plot / Tau table if study has this information
                     br(),
                     # Plot Output
                     withSpinner((plotOutput(outputId = "individual_gene_tau_plot")), type = 3),
                     # Plot Tau Table
                     fluidRow(
                         column(12, "",
                                fixedRow(
                                    column(8,
                                        p("TAU Data (GEUVADIS lymphoblast)", style = "font-size:16px"),
                                        (downloadButton("table1_download", "Download TAU Data")),
                                        br(),
                                        br(),
                                        withSpinner((dataTableOutput(outputId = "gene_detail_table")), type = 1)
                                    )
                                )
                         ),
                         br(),
                         br(),
                         includeHTML("citations.html"),
                     )

                 ),
)