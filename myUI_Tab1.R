# All genes, main plot
TAB1 <- tabPanel(title = "All Escape Expressions",
               #  tags$script(src = "lazy_slider.js"),
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
                        
                         actionButton("resetButton", "Clear Genes"),
                         br(),
                         br(),
                         strong("Displayed Genes:", style = "font-size:14px"),br(),
                         verbatimTextOutput("displayedGenes"),
                         br(),
                         selectInput("addStudies", "View Escape States",
                                     c(" " = "empty",
                                       "GEUVIDAS lymphoblast cells (present study)" = "study1",
                                       "Cotton et al. + Carrel/Willard" = "study2")
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study1'",
                             sliderInput(inputId = "slider1", 
                                         label = "Choose an escape frequency for the [inactive | variable] threshold", 
                                         value = SV_threshold, min = 0.1, max = 0.75),
                             sliderInput(inputId = "slider2", 
                                         label = "Choose an escape frequency for the [variable | escape] threshold", 
                                         value = VE_threshold, min = 0.25, max = 0.99)
                         ),
                         conditionalPanel(
                             condition = "output.sliderWarning",
                          #   verbatimTextOutput("sliderWarningMessage"),
                             h2("Warning: the [inactive | variable] threshold cannot be greater than
                                the [variable | escape] treshold. Threshold value has been set back to previous
                                value", style="font-size:14px;color:red")
                         ),
                         br(),
                         strong("Input Dataset"),
                         p("GEUVIDAS DATA: ", a("102 Samples, 268 Genes, Lymphoblast Cells", href="x_expr.tsv",
                                                target="_blank"), style = "font-size:12px"),
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