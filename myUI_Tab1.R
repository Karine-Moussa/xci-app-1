# All genes, main plot
TAB1 <- tabPanel(title = "All Escape Expressions",
             # Create a layout with a sidebar and main area ----
             sidebarLayout(
                 # Create a sidebar panel containing input controls ----
                 sidebarPanel(
                     h3("Observing XCI Escape Calls"),
                     selectInput("searchType", "Search Type",
                                 c(Gene = "gene", "Disease/Trait" = "disease")
                     ),
                     conditionalPanel(
                         condition = "input.searchType == 'gene'",
                         selectizeInput("geneofinterest1", "Gene of Interest:", c("",unique(x_expr_mod[,"GENE"])), multiple = TRUE),
                         p("(Click on individual data points to add more genes)", style = "font-size:14px")
                     ),
                     #checkboxInput("checkbox_input1", label = "Show all escape genes", value = FALSE),
                     #verbatimTextOutput("test"),
                     conditionalPanel(
                         condition = "input.searchType == 'disease'",
                         selectizeInput("diseaseofinterest1", "Disease/Trait of Interest:",
                                    #    multiple = TRUE,
                                        # search in a combined list of GWAS and Nelson traits
                                        c("", unique(c(LIST_OF_TRAITS_GWAS$GWAS_NAME, LIST_OF_TRAITS_NELSON_2$NELS2_NAME)))),
                         p("(Note: point-click is disabled in Disease/Trait mode)", style = "font-size:14px")
                     ),
                     br(),
                     actionButton("resetButton", "Clear Genes"),
                     br(),
                     br(),
                     selectInput("addStudies", "View Escape States (according to various studies)",
                                 c(" " = "empty",
                                   "present study (GEUVIDAS lymphoblast cells)" = "study1",
                                   "Cotton et al. + Carrel/Willard" = "study2")
                     ),
                     br(),
                     strong("Displayed Genes:", style = "font-size:14px"),br(),
                     verbatimTextOutput("displayedGenes"),
                     br(),
                     strong("Input Dataset"),
                     p("GEUVIDAS: ", span(a("102 Samples, 268 Genes, Lymphoblast Cells (link currently not working)", href="https://raw.githubusercontent.com/Karine-Moussa/xci-app-1/main/data_sources/x_expr.tsv?token=ARBUC3AOO3X6PWUK6HEGB6C73UATC", 
                                            target="_blank")), style = "font-size:12px"),
                     br(),
                     br(),
                     p("GEUVIDAS genome build: hg38", style = "font-size:12px;color:grey"),
                     em(paste("Last published:",publication_date), style = "font-size:12px;color:grey")
                 ),
                 # Create plot and Action Buttons in Main Panel
                 mainPanel(
                     withSpinner(plotOutput(outputId = "gene_pvalue", height = "500px", click = "myclick"), type = 2),
                     plotOutput(outputId = "gene_pvalue_xchromosome", height = "100px"),
                     # Only show this panel if the plot type is a histogram
                     conditionalPanel(
                         condition = "output.geneTableStatus",
                         strong("GWAS Catalog Search (Gene)", style = "font-size:16px"),
                         p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                         (dataTableOutput(outputId = "gene_gwas_data")),
                         strong("<additional> Catalog Search (Gene)", style = "font-size:16px"),
                         p(span(a("Searching \"some source\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                         (dataTableOutput(outputId = "gene_nelson_data"))
                     ),
                     conditionalPanel(
                       #  condition = "input.searchType == 'disease' && input.diseaseofinterest1 != ''",
                         condition = "output.diseaseTableStatus",
                         strong("GWAS Catalog Search (Disease/Trait)", style = "font-size:16px"),
                         p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                         (dataTableOutput(outputId = "gene_disease_gwas_data")),
                         strong("<additional> Catalog Search (Disease/Trait)", style = "font-size:16px"),
                         p(span(a("Searching \"some source\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                         (dataTableOutput(outputId = "gene_disease_nelson_data"))
                     )
                 )
             )
    )