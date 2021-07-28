# Invidivual Gene Search
TAB2 <- tabPanel(title = "Individual Gene Search",
                 # Create a layout with a sidebar and main area
                 sidebarLayout(
                     sidebarPanel(
                         h3("Observing XCI escape calls across studies"),
                         selectizeInput("geneofinterest2", "Gene of Interest:", unique(c(study1_genes,
                                study2_genes, study3_genes, study4_genes, study5_genes, 
                                study6_genes, study7_genes, study8_genes, study9_genes)), multiple = TRUE),
                         actionButton("resetButton2", "Clear Genes"),
                         br(),
                         br(),
                         strong("Directions for Use", style = "font-size:12px"),br(),
                         em("---Input an X-gene of interest", style = "font-size:12px"),br(),
                         em("---Examples: XIST, ZBED1, ASMTL", style = "font-size:12px"),br(),
                         br(),
                         strong("Studies:", style = "font-size:12px"),br(),
                         p("GEUVADIS (lymphoblast) - manuscript accepted 2021", style = "font-size:10px"),
                         p(span(a("Cotton et al. (lymphoblast & fibroblast)", href="https://doi.org/10.1186/gb-2013-14-11-r122", target="_blank",)), style = "font-size:10px"),
                         p(span(a("Carrel/Willard (hybrid fibroblast)", href="https://doi.org/10.1038/nature03479", target="_blank",)), style = "font-size:10px"),
                         p(span(a("Katsir + Linial (lymphoblast & fibroblast)", href="https://doi.org/10.1186/s12864-019-5507-6", target="_blank",)), style = "font-size:10px"),
                         p(span(a("GTEx (multi-tissue)", href="https://doi.org/10.1038/nature24265", target="_blank",)), style = "font-size:10px"),
                         p(span(a("Cotton et al. (multi-tissue)", href="https://doi.org/10.1093/hmg/ddu564", target="_blank",)), style = "font-size:10px"),
                         p(span(a("Baraton + Brown (DNA methylation in cancer and non-cancer cells)", href="https://doi.org/10.1186/s13072-021-00404-9", target="_blank",)), style = "font-size:10px"),
                         br(),
                         em(paste("Last published:", publication_date), style = "font-size:12px;color:grey"),
                     ),
                     mainPanel(
                         # Show message if no genes are displayed
                         conditionalPanel(
                             condition = "!output.ready1",
                             span(
                                 "Please wait until app is fully loaded before selecting a gene...",
                                 style = "font-size:18px;font-style:italic;color:purple"
                             ),
                             br(),
                             span("(refresh page if >10 seconds)", style =
                                      "font-size:18px;font-style:italic;color:purple"),
                         ),
                         span(textOutput("pleaseInput3"), style = "font-size:18px;font-style:italic"),
                         p("Escape States", style = "font-size:16px;font-weight:bold"),
                         (downloadButton("individual_gene_escape_download", "Download Table")),
                         br(),
                         br(),
                         withSpinner(dataTableOutput(outputId = "ind_escape_states_table"), type = 1, size = 1),
                     )
                 )
)
