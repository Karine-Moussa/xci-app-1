TAB1 <- tabPanel(title = "All Escape Expressions",
               #  tags$script(src = "lazy_slider.js"),
                 # Create a layout with a sidebar and main area ----
                 sidebarLayout(
                     # Create a sidebar panel containing input controls ----
                     sidebarPanel(
                         h3("Observing XCI Escape Calls"),
                         selectInput("addStudies", "Select a Study",
                                     c(" " = "empty",
                                       "MANUAL UPLOAD" = "study0",
                                       "Sauteraud et al: GEUVADIS: lymphoblast" = "study1",
                                       "Tukiainen et al: Fully Skewed Female multi-tissue" = "study6",
                                       "Tukiainen et al: DGEA male/female multi-tissue" = "study10",
                                       "Cotton et al: mDNA multi-tissue" = "study7",
                                       "Cotton et al: lymphoblast & fibroblast" = "study2",
                                       "Balaton + Brown: DNAme (cancer cells)" = "study8",
                                       "Balaton + Brown: Epigenetic Predictor (healthy cells)" = "study9",
                                       "Carrel + Willard: hybrid fibroblast" = "study3",
                                       "Katsir + Linial: scRNA-seq lymphoblast" = "study4",
                                       "Katsir + Linial: scRNA-seq fibroblast" = "study5"
                                       )
                         ),
                         # Only display SearchType if a study is selected
                         conditionalPanel(
                             condition = "input.addStudies != 'empty' & input.addStudies != 'study0' || output.plotStudy0",
                             selectInput("searchType", "Search Type",
                                         c(Gene = "gene", "Disease/Trait" = "disease")
                             ),
                             # Conditional panels within a conditional panel
                             # Gene search if the SearchType box exists
                             # (there is a conditional panel for each study)
                             # study 0
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & output.plotStudy0",
                                 selectizeInput("geneofinterest1_0", "Gene of Interest:", c("Need genes"), multiple = TRUE), # need to update
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 1
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study1'",
                                 # selectizeInput("geneofinterest1", "Gene of Interest:", c("DUMMY",unique(x_expr_mod[,"GENE"])), multiple = TRUE),
                                 selectizeInput("geneofinterest1_1", "Gene of Interest:", sort(study1_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 2
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study2'",
                                 selectizeInput("geneofinterest1_2", "Gene of Interest:", sort(study2_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 3
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study3'",
                                 selectizeInput("geneofinterest1_3", "Gene of Interest:", sort(study3_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 4
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study4'",
                                 selectizeInput("geneofinterest1_4", "Gene of Interest:", sort(study4_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 5
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study5'",
                                 selectizeInput("geneofinterest1_5", "Gene of Interest:", sort(study5_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 6
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study6'",
                                 selectizeInput("geneofinterest1_6", "Gene of Interest:", sort(study6_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 7
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study7'",
                                 selectizeInput("geneofinterest1_7", "Gene of Interest:", sort(study7_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 8
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study8'",
                                 selectizeInput("geneofinterest1_8", "Gene of Interest:", sort(study9_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 9
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study9'",
                                 selectizeInput("geneofinterest1_9", "Gene of Interest:", sort(study9_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # study 10
                             conditionalPanel(
                                 condition = "input.searchType == 'gene' & input.addStudies == 'study10'",
                                 selectizeInput("geneofinterest1_10", "Gene of Interest:", sort(study10_genes), multiple = TRUE),
                                 p("(Click on individual data points to add more genes)", style = "font-size:14px")
                             ),
                             # Disease search if the SearchType box exists
                             conditionalPanel(
                                 condition = "input.searchType == 'disease'",
                                 selectizeInput("diseaseofinterest1", "Disease/Trait of Interest:",
                                                #    multiple = TRUE,
                                                ## search in a combined list of GWAS and Nelson traits
                                                #c("", unique(c(LIST_OF_TRAITS_GWAS$GWAS_NAME, LIST_OF_TRAITS_NELSON_2$NELS2_NAME)))),
                                                ## search in only the GWAS traits
                                                c("", unique(c("ALL FEMALE BIAS TRAITS", LIST_OF_TRAITS_GWAS$GWAS_NAME)))),
                                 p("(Note: point-click is disabled in Disease/Trait mode)", style = "font-size:14px")
                             ), # end of inner conditional panels
                             actionButton("resetButton1", "Clear Genes"),
                             br(),
                             br(),
                             # Only display the "Displayed Genes" if a study is selected / study0 submitted
                             strong("Displayed Genes:", style = "font-size:14px"),br(),
                             verbatimTextOutput("displayedGenes"),
                         ), # end of outer condition panel
                         conditionalPanel(
                             condition = "output.sliderWarning",
                             #   verbatimTextOutput("sliderWarningMessage"),
                             h2("Warning: the [inactive | variable] threshold cannot be greater than
                                the [variable | escape] treshold. Threshold value has been set back to previous
                                value", style="font-size:14px;color:red")
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study1' || input.addStudies == 'study6' || output.plotStudy0",
                             p("", style = "font-size:8px"),
                             strong("Escape Thresholds ", style = "font-size:16px",
                               a('(?)', href = "Tutorial-Sliders.pdf", target="_blank", style = "font-size:14px")
                               ),
                             sliderInput(inputId = "slider1", 
                                         label = "Choose an escape frequency for the [inactive | variable] threshold", 
                                         value = SV_threshold, min = 0.1, max = 0.75),
                             sliderInput(inputId = "slider2", 
                                         label = "Choose an escape frequency for the [variable | escape] threshold", 
                                         value = VE_threshold, min = 0.25, max = 0.95),
                             tags$hr(),
                         ),
                         # Display options if manual upload is selected
                         conditionalPanel(
                             condition = "input.addStudies == 'study0'",
                             strong(a('(Tutorial)', href = "Tutorial-Upload.pdf", target="_blank", style = "font-size:14px")),
                             h4("1. CREATE A TEMPLATE"),
                             # Input: Select Template ----
                             radioButtons("template", "Template Type",
                                          choices = c("Template 1 (gene, state)" = 1,
                                                      "Template 2 (gene, state, tiss_samp)" = 2),
                                          selected = 1),
                             # Input: Check box -------
                             checkboxInput("includes_start", 
                                           "Include 'start' position", 
                                           value = TRUE),
                             # Input: Select separator ----
                             #radioButtons("sep", "Separator",
                             #             choices = c(Comma = ",",
                             #                         Tab = "\t"),
                             #             selected = ","),
                             # Download Template ------
                             (downloadButton("template_download", "Download Template")),
                             br(), br(),
                             # Input: Select a file ----
                             h4("2. UPLOAD DATA SET"),
                             fileInput("file1", "Choose CSV File",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             # Submit File Upload ------
                             h4("3. SUBMIT"),
                             actionButton("submitButton1", "Submit Uploaded Data Set"),
                             # Line Break: -------
                             tags$hr(),
                             # Download templates: --------
                             # This was moved to below  input.slider1
                         ),
                         br(),
                         em(paste("Last published:", publication_date), style = "font-size:12px;color:grey")
                     ),
                     # Create plot and Action Buttons in Main Panel
                     mainPanel(
                         conditionalPanel(
                             condition = "output.plotStudy0",
                             withSpinner(plotOutput(outputId = "plot_study0", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study1'",
                             withSpinner(plotOutput(outputId = "plot_study1", height = "500px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study2'",
                             withSpinner(plotOutput(outputId = "plot_study2", height = "100px", width = "1000px", #here
                                                    dblclick = "study2_dblclick",
                                                    brush = brushOpts(id = "study2_brush", resetOnNew = TRUE)
                             ), type = 2) # spinner type = 2
                         ),
                         # plot 2 before zooming in
                         # conditionalPanel(
                         #     condition = "input.addStudies == 'study2'",
                         #     withSpinner(plotOutput(outputId = "plot_study2", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         # ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study3'",
                             withSpinner(plotOutput(outputId = "plot_study3", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study4'",
                             withSpinner(plotOutput(outputId = "plot_study4", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study5'",
                             withSpinner(plotOutput(outputId = "plot_study5", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study6'",
                             withSpinner(plotOutput(outputId = "plot_study6", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study7'",
                             withSpinner(plotOutput(outputId = "plot_study7", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study8'",
                             withSpinner(plotOutput(outputId = "plot_study8", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study9'",
                             withSpinner(plotOutput(outputId = "plot_study9", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study10'",
                             withSpinner(plotOutput(outputId = "plot_study10", height = "100px", click = "myclick", hover = "myhover"), type = 2)
                         ),
                         # Display the chromosome (image) if study is not selected
                         conditionalPanel(
                             condition = "input.addStudies == 'empty'",
                             img(src = "x-chrom-img.png", width = "750px"),
                         ),
                         # Display the chromosome (graph) if study is selected
                         conditionalPanel(
                             condition = "input.addStudies != 'empty' || output.plotStudy0", #here
                             withSpinner(plotOutput(outputId = "xchromosome", height = "100px", width = "1000px"), type = 1, size = 1)
                         ),
                         # LEGENDS
                         # Show this panel if we're looking at escape states
                         # for non-GEUVADIS studies
                         conditionalPanel(
                             condition = "input.addStudies != 'empty' & input.addStudies != 'study1' & input.addStudies != 'study10'",
                             p("", style = "font-size:4px"),
                             fluidRow(
                                 column(4, offset = 1,
                                        img(src = "mainplot_additional_studies_legend_inactive_horizontal.png", height = 23))
                             ),
                             br(),
                         ),
                         conditionalPanel(
                             condition = "input.addStudies == 'study10'",
                             p("", style = "font-size:4px"),
                             fluidRow(
                                 column(8, offset = 1,
                                        img(src = "mainplot_additional_studies_legend_inactive_horizontal_deg.png", height = "20", width = "425"))
                             ),
                             br(),
                         ),
                         # Show this panel if we're looking at escape states
                         # for GEUVADIS study
                         conditionalPanel(
                             condition = "input.addStudies == 'study1'",
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
                                tabPanel("Escape States",
                                         conditionalPanel(
                                             condition = "!output.ready1",
                                             span("Please wait until app is fully loaded before selecting a study...", style="font-size:18px;font-style:italic;color:purple"),br(),
                                             span("(refresh page if >10 seconds)", style="font-size:18px;font-style:italic;color:purple"),
                                         ),
                                         # Only show this message if no study is selected
                                         p("", style="font-size:10px"),
                                         span(textOutput("pleaseInput2"), style="font-size:18px;font-style:italic"),
                                         br(),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study0'",
                                             p("", style = "font-size:14px"),
                                             p("UPLOADED STUDY  ", style = "font-size:18px", 
                                               downloadLink('download_states_study0', '[download table]', style = "font-size:14px")),
                                             checkboxInput("states_filter_study0", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study0"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study1'",
                                             p("", style = "font-size:14px"),
                                             p("Sauteraud et al. GEUVADIS lymphoblast  ", style = "font-size:18px", 
                                               downloadLink('download_states_study1', '[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1101/gr.275677.121", href="https://genome.cshlp.org/content/early/2021/08/23/gr.275677.121", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study1", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             checkboxInput("tissues_filter_study1", 
                                                           "View all states within samples", 
                                                           value = FALSE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study1"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study6'",
                                             p("", style = "font-size:14px"),
                                             p("Fully Skewed Female Tukiainen et al. (n=16 tissues)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study6', '[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study6", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             checkboxInput("tissues_filter_study6", 
                                                           "View states within each tissue", 
                                                           value = FALSE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study6"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study2'",
                                             p("", style = "font-size:14px"),
                                             p("Cotton et al. lymphoblast & fibroblast (n=2 tissues)", style = "font-size:18px", 
                                               downloadLink('download_states_study2','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/gb-2013-14-11-r122", href="https://doi.org/10.1186/gb-2013-14-11-r122", target="_blank",)), style = "font-size:14px"),
                                             p("Escape states obtained from Suppl.Table.1 of ", span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             # checkboxInput("states_filter_study2", # come back here
                                             #               "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                             #               value = TRUE),
                                             fluidRow(
                                                 column(4, offset = 0,
                                                        radioButtons("filter_study2", "Filter results",
                                                                     choices = c("Filter by searched genes (if no genes/diseases are selected, returns all genes)" = 1,
                                                                                 "Filter by displayed genes (zoom in plot)" = 2, 
                                                                                 "No filters (return all genes)" = 3),
                                                                     selected = 1),
                                                 ),
                                                 column(4, offset = 2,
                                                        br(),
                                                        p(paste(meta_stat_dt$NUM_GENES[meta_stat_dt$NUMBER == 2], "genes"), style = "font-size:16px"),
                                                        p(paste(meta_stat_dt$NUM_CALLS[meta_stat_dt$NUMBER == 2], "total calls"), style = "font-size:16px"),
                                                        p(paste(meta_stat_dt$NUM_TISS[meta_stat_dt$NUMBER == 2], "tissue(s)"), style = "font-size:16px"),
                                             ),
                                             ),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study2"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study3'",
                                             p("", style = "font-size:14px"),
                                             p("Carrel and Willard: hybrid fibroblast", style = "font-size:18px", 
                                               downloadLink('download_states_study3','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1038/nature03479", href="https://doi.org/10.1038/nature03479", target="_blank",)), style = "font-size:14px"),
                                             p("escape states obtained from Suppl.Table.1 of ", span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study3", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study3"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study4'",
                                             p("", style = "font-size:14px"),
                                             p("Katsir + Linial: scRNA-seq lymphoblast (n = 25)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study4','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s12864-019-5507-6", href="https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-5507-6", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study4", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study4"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study5'",
                                             p("", style = "font-size:14px"),
                                             p("Katsir + Linial: scRNA-seq fibroblast (n = 104)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study5','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s12864-019-5507-6", href="https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-5507-6", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study5", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study5"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study7'",
                                             p("", style = "font-size:14px"),
                                             p("Cotton et. al: mDNA multi-tissue (n = 27 tissues)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study7','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1093/hmg/ddu564", href="https://academic.oup.com/hmg/article/24/6/1528/682766#supplementary-data", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study7", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             checkboxInput("tissues_filter_study7", 
                                                           "View states within each tissue", 
                                                           value = FALSE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study7"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study8'",
                                             p("", style = "font-size:14px"),
                                             p("Balaton + Brown: Promoter DNA methylation", style = "font-size:18px",
                                               p("Center for Epigenome Mapping Technologies (CEMT) Cancer Cells", style ="font-size:14px"),
                                               p("Tissues: tonsils, thyroid, blood, breast, colon, brain", style ="font-size:14px"),
                                               downloadLink('download_states_study8','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s13072-021-00404-9", href="https://epigeneticsandchromatin.biomedcentral.com/articles/10.1186/s13072-021-00404-9", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study8", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study8"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study9'",
                                             p("", style = "font-size:14px"),
                                             p("Balaton + Brown: random forest epigenetic predictor model", style = "font-size:18px",
                                               p("Core Research for Evolutional Science and Technology (CREST)", style = "font-size:14px"), 
                                               p("Tissues: hepatocytes, normal human colon absorptive epithelial cells", style = "font-size:14px"), 
                                               downloadLink('download_states_study9','[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1186/s13072-021-00404-9", href="https://epigeneticsandchromatin.biomedcentral.com/articles/10.1186/s13072-021-00404-9", 
                                                      target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study9", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study9"))
                                         ),
                                         conditionalPanel( # conditional panel within conditional panel
                                             condition = "input.addStudies == 'study10'",
                                             p("", style = "font-size:14px"),
                                             p("Differential Gene Expression Analysis Tukiainen et al. (n=29 tissues)  ", style = "font-size:18px", 
                                               downloadLink('download_states_study10', '[download table]', style = "font-size:14px")),
                                             p(span(a("doi.org/10.1038/nature24265", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:14px"),
                                             checkboxInput("states_filter_study10", 
                                                           "Filter by selected genes (if no genes/diseases are selected, returns all genes)", 
                                                           value = TRUE),
                                             br(),
                                             (dataTableOutput(outputId = "status_table_study10"))
                                         )
                                ),
                                tabPanel("Association Data",
                                         # Only show this message if no genes or diseases are inputted
                                         p("", style="font-size:10px"),
                                         span(textOutput("pleaseInput1"), style="font-size:18px;font-style:italic"),
                                         br(),
                                         # Only show this panel if the we're looking at genes or diseases 
                                         conditionalPanel(
                                             condition = "output.geneTableStatus1",
                                             strong("GWAS Catalog Search", style = "font-size:16px",
                                                    a('(?)', href = "Tutorial-GWAS.pdf", target="_blank", style = "font-size:14px")
                                                    ),
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
                                width = NULL, side = "left"
                         )
                     )
                 )
)