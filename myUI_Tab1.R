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
                             selectizeInput("geneofinterest1", "Gene of Interest:", c("DUMMY",unique(x_expr_mod[,"GENE"])), multiple = TRUE),
                             p("(Click on individual data points to add more genes)", style = "font-size:14px")
                         ),
                         #checkboxInput("checkbox_input1", label = "Show all escape genes", value = FALSE),
                #         verbatimTextOutput("test"),
                         conditionalPanel(
                             condition = "input.searchType == 'disease'",
                             selectizeInput("diseaseofinterest1", "Disease/Trait of Interest:",
                                            #    multiple = TRUE,
                                            ## search in a combined list of GWAS and Nelson traits
                                            #c("", unique(c(LIST_OF_TRAITS_GWAS$GWAS_NAME, LIST_OF_TRAITS_NELSON_2$NELS2_NAME)))),
                                            ## search in only the GWAS traits
                                            c("", unique(c("ALL FEMALE BIAS TRAITS (main study genes only)", LIST_OF_TRAITS_GWAS$GWAS_NAME)))),
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
                                       "GEUVADIS: lymphoblast (displayed study)" = "study1",
                                       "Cotton et al: multi-tissue" = "study2",
                                       "Carrel + Willard: hybrid fibroblast" = "studyX",
                                       "Katsir + Linial: scRNA-seq lymphoblast" = "study3",
                                       "Katsir + Linial: scRNA-seq fibroblast" = "study4")
                         ),
                         conditionalPanel(
                             condition = "output.sliderWarning",
                             #   verbatimTextOutput("sliderWarningMessage"),
                             h2("Warning: the [inactive | variable] threshold cannot be greater than
                                the [variable | escape] treshold. Threshold value has been set back to previous
                                value", style="font-size:14px;color:red")
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
                         br(),
                         strong("Main Dataset"),
                         p("GEUVADIS DATA: ", a("102 Samples, 268 Genes, Lymphoblast Cells", href="x_expr.tsv",
                                                target="_blank"), style = "font-size:12px"),
                         br(),
                         br(),
                         p("GEUVADIS genome build: hg38", style = "font-size:12px;color:grey"),
                         em(paste("Last published:",publication_date), style = "font-size:12px;color:grey")
                     ),
                     # Create plot and Action Buttons in Main Panel
                     mainPanel(
                         withSpinner(plotOutput(outputId = "gene_pvalue", height = "500px", click = "myclick", hover = "myhover"), type = 2),
                         plotOutput(outputId = "gene_pvalue_xchromosome", height = "100px"),
                         # LEGENDS
                         # Show the baseline legend if we're not looking at escape states:
                         conditionalPanel(
                             condition = "input.addStudies == 'empty'",
                             p("", style = "font-size:4px"),
                             fluidRow(
                                 column(4, offset = 1,
                                        img(src = "mainplot_legend_horizontal.png", height = 23))
                             ),
                             p("", style = "font-size:4px"),
                         ),
                         # LEGENDS
                         # Only show this panel if we're looking at escape states
                         # Also update the legend here if that's the case
                         conditionalPanel(
                             condition = "input.addStudies != 'empty'",
                             p("", style = "font-size:4px"),
                             fluidRow(
                                 column(4, offset = 1,
                                        img(src = "mainplot_legend_horizontal.png", height = 23)),
                                 column(4, offset = 2,
                                        img(src = "mainplot_additional_studies_legend_inactive_horizontal.png", height = 23))
                                 ),
                             br(),
                         ),
                         tabBox(title = "",
                                tabPanel("Association Data",
                                         # Only show this message if no genes or diseases are inputted
                                         p("", style="font-size:10px"),
                                         span(textOutput("pleaseInput"), style="font-size:18px;font-style:italic"),
                                         br(),
                                         # Only show this panel if the we're looking at genes or diseases 
                                         conditionalPanel(
                                             condition = "output.geneTableStatus",
                                             strong("GWAS Catalog Search (Gene)", style = "font-size:16px"),
                                             p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                             withSpinner(dataTableOutput(outputId = "gene_gwas_data"), type = 1)
                                         ),
                                         conditionalPanel(
                                             #  condition = "input.searchType == 'disease' && input.diseaseofinterest1 != ''",
                                             condition = "output.diseaseTableStatus",
                                             strong("GWAS Catalog Search (Disease/Trait)", style = "font-size:16px"),
                                             p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                             withSpinner(dataTableOutput(outputId = "gene_disease_gwas_data"), type = 1)
                                         )
                                ),
                                tabPanel("Escape States",
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'empty' || input.addStudies == 'study1'",
                                             p("", style = "font-size:14px"),
                                             p("GEUVADIS lymphoblast (displayed study)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study1', '[download table]', style = "font-size:14px")),
                                             checkboxInput("states_filter_study1", 
                                                           "Filter by displayed genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study1"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study2'",
                                             p("", style = "font-size:14px"),
                                             p("Cotton et al: multi-tissue (n=27 tissues)", style = "font-size:18px", 
                                               downloadLink('download_states_study2','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1093/hmg/ddu564", href="https://doi.org/10.1093/hmg/ddu564", target="_blank",)), style = "font-size:14px"),
                                             p("escape states obtained from Suppl.Table.1: ", span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study2", 
                                                           "Filter by displayed genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study2"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'studyX'",
                                             p("", style = "font-size:14px"),
                                             p("Carrel and Willard: hybrid fibroblast", style = "font-size:18px", 
                                               downloadLink('download_states_studyX','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1038/nature03479", href="https://doi.org/10.1038/nature03479", target="_blank",)), style = "font-size:14px"),
                                             p("escape states obtained from Suppl.Table.1: ", span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_studyX", 
                                                           "Filter by displayed genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_studyX"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study3'",
                                             p("", style = "font-size:14px"),
                                             p("Katsir + Linial: scRNA-seq lymphoblast (n = 25)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study3','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s12864-019-5507-6", href="https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-5507-6", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study3", 
                                                           "Filter by displayed genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study3"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study4'",
                                             p("", style = "font-size:14px"),
                                             p("Katsir + Linial: scRNA-seq fibroblast (n = 104)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study4','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s12864-019-5507-6", href="https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-5507-6", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study4", 
                                                           "Filter by displayed genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study4"))
                                         )
                                ),
                                width = NULL, side = "left"
                         )
                     )
                 )
)