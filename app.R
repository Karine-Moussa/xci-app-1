# Karine Moussa
# XCI-app 
# App Settings
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Libraries
library(shiny, warn.conflicts = FALSE)
library(dqshiny, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(png, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(magick, warn.conflicts = FALSE)
library(cowplot, warn.conflicts = FALSE)

### Source Data and Functions ###
source("utilities/format_input_data.R", local = TRUE)
source("utilities/create_global_variables.R", local = TRUE)
source("utilities/x_expr_mods.R", local = TRUE)
source("utilities/format_additional_studies.R", local = TRUE)
source("utilities/create_association_df.R", local = TRUE)
source("utilities/create_trait_objects.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/format_plot_aesthetics.R", local = TRUE)

### Load files and pre-processed data
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

### Save publication date
publication_date <- "2021-01-13 12:45:14 EST" # Sys.time()

### Options for Loading Spinner #####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

### Global variables ####
plot1_xmin = 0
plot1_xmax = max(x_expr$start) + 1.3e7

# shinyapp
ui <- fluidPage(title = "XCI Data",
                tabsetPanel(
                    ## TAB 1
                    tabPanel(title = "All Escape Expressions",
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
                                    selectizeInput("diseaseofinterest1", "Disease/Trait of Interest:", c("",LIST_OF_TRAITS_GWAS$GWAS_NAME)),
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
                                    condition = "input.searchType == 'gene' && input.geneofinterest1 != ''",
                                    strong("GWAS Catalog Search (Gene)", style = "font-size:16px"),
                                    p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                    (dataTableOutput(outputId = "gene_gwas_data")),
                                    strong("<additional> Catalog Search (Gene)", style = "font-size:16px"),
                                    p(span(a("Searching \"some source\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                    (dataTableOutput(outputId = "gene_nelson_data"))
                                ),
                                conditionalPanel(
                                    condition = "input.searchType == 'disease' && input.diseaseofinterest1 != ''",
                                    strong("GWAS Catalog Search (Disease/Trait)", style = "font-size:16px"),
                                    p(span(a("Searching \"All Assocations v1.02\"", href="https://www.ebi.ac.uk/gwas/docs/file-downloads", target="_blank",)), style = "font-size:14px"),
                                    (dataTableOutput(outputId = "gene_disease_data"))
                                )
                            )
                        )
                    ),
                    ## TAB 2
                    tabPanel(title = "Individual Gene Search",
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
                    ),
                    ## TAB 3
                    tabPanel(title = "Terminology",
                        p("Will update..."),
                        p("XCI:"),
                        p("Tau:"),
                        p("Skew:"),
                    ),
                    ## TAB 4 
                    tabPanel(title = "How To",
                        br(),
                        em("This is a concept page, all footage will be redone when product is finalized", style = "font-size:14px"),
                        br(),br(),
                        strong("Search for genes:", style = "font-size:18px"),br(),
                        tags$img(type="mp4",src="xcidemo1.mp4", height="400px"),br(),
                        br(),br(),br(),
                        strong("Search for diseases/traits:", style = "font-size:18px"),br(),
                        tags$img(type="mp4",src="xcidemo2.mp4", height="400px")
                    )
                )
)

server <- function(input, output, session) {
    # Reactive values
    rv <- reactiveValues(
        geneofinterest1 = "",
        geneofinterest2 = "",
        diseaseofinterest1 = "",
        searchType = "gene",
        addStudies = "",
        checkbox_input1 = "",
        myclick = "",
        plot1_coord_x = c(),
        plot1_coord_y = c(),
        mapped_gene = "",
        closest_expr_index = "",
        returned_genes_list = ""
        )
    # ObserveEvents Tab 1 
    observeEvent(input$geneofinterest1, { 
        rv$geneofinterest1 <- unique(c(input$geneofinterest1, rv$mapped_gene))
        rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
        rv$searchType <- input$searchType
        })
    observeEvent(input$diseaseofinterest1, { 
        rv$diseaseofinterest1 <- input$diseaseofinterest1
        rv$searchType <- input$searchType
    })
    observeEvent(input$searchType, {
        previous_searchtype <- rv$searchType
        rv$searchType <- input$searchType
        if(previous_searchtype != rv$searchType) {
            rv$mapped_gene = ""
            rv$geneofinterest1 = ""
            rv$diseaseofinterest1 = ""
            rv$plot1_coord_x = c()
            rv$plot1_coord_y = c()
            rv$closest_expr_index = "" 
            rv$returned_genes_list = ""
        }
    })
    observeEvent(input$checkbox_input1, {
        rv$checkbox_input1 <- input$checkbox_input1
        ifelse(rv$checkbox_input1 == "TRUE", 
               rv$test1 <- "PikaTRUE",
               rv$test1 <- "LuFALSio")
        ifelse(rv$checkbox_input1 == "TRUE", 
               rv$geneofinterest1 <- unique(x_expr_mod[x_expr_mod$status == "E","GENE"]),
               rv$geneofinterest1 <- "")
    })
    observeEvent(input$addStudies, { 
        rv$addStudies <- input$addStudies
    })
    observeEvent(input$myclick, {
        if(rv$searchType == 'gene'){
            rv$plot1_coord_x = c(rv$plot1_coord_x, input$myclick$x)
            rv$plot1_coord_y = c(rv$plot1_coord_y, input$myclick$y)
            for(i in 1:length(rv$plot1_coord_x)){
                index <- which.min(abs(x_expr$start - rv$plot1_coord_x[i]))
                rv$closest_expr_index[i] <- index
            }
            rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
            rv$mapped_gene = x_expr$GENE[as.numeric(rv$closest_expr_index)]
            if(rv$geneofinterest1 == ""){
                rv$geneofinterest1 = rv$mapped_gene
            } else {
                rv$geneofinterest1 = unique(c(rv$geneofinterest1, rv$mapped_gene))
            }
        }
    })
    observeEvent(input$resetButton, {
        rv$mapped_gene = ""
        rv$geneofinterest1 = ""
        rv$diseaseofinterest1 = ""
        rv$plot1_coord_x = c()
        rv$plot1_coord_y = c()
        rv$closest_expr_index = ""
        rv$returned_genes_list = ""
    })
    # ObserveEvents Tab2
    observeEvent(input$geneofinterest2, { 
        rv$geneofinterest2 <- input$geneofinterest2 
    })
    ##############################
    ## FOR TESTING ###############
    ##############################
    ## for testing:
    output$test <- renderPrint({ 
        str(rv$mapped_gene)
        str(rv$geneofinterest1)
        str(rv$plot1_coord_x)
    })
    ##############################
    ## OUTPUT TEXT ###############
    ##############################
    output$displayedGenes <- renderPrint({
        to_display = ""
        if(rv$searchType == "gene") { 
               to_display <- rv$geneofinterest1
        } else {
               to_display <- rv$returned_genes_list
        } 
        print(to_display)
    })
    ##############################
    ##############################
    ## CHECKBOXES ################
    ##############################
    # output$checkbox_text1 <- renderPrint({ rv$checkbox_input1 })
    ##############################
    ## DOWNLOAD HANDLERS #########
    ##############################
    ### TAB 2
    # Tau Download button for geneofinterest
    output$table1_download <- downloadHandler(
        filename =  function(){
            # Name of created file
            paste(rv$geneofinterest1, "_tau_table.csv", sep = "")
        },
        content = function(file){
            # Get the data source
            mydata <- readRDS('data_output/geneofinterest_tau_table.rds')
            write.csv(mydata, file)
        }
    )
    ##############################
    ## DATA TABLES ###############
    ##############################
    ### TAB 1
    # Gene GWAS table 
    output$gene_gwas_data <- renderDataTable({
        validate(need(rv$geneofinterest1,""))
        geneofinterest <- rv$geneofinterest1
        df <- data.frame()
        for(gene in geneofinterest){
            df <- rbind(df, create_gwas_association_df(gene))
        }
        ### only perform this section if the assocation_df isn't empty ###
        ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df$Link <- paste0('<a href="https://',df$Link,'" target="_blank">', df$Link, '</a>')
            to_remove <- "" # if all rows in a column are blank, then remove the column
            for(i in 1:ncol(df)){
                print(i)
                if(sum((df[,i]) == "") == nrow(df)){
                    ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
                }
            }
            # make sure to_remove actually exists before removing it from df
            ifelse(to_remove != "", df <- select(df, -c(all_of(to_remove))),"")
        } 
        #### done ########################################################
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2))
        ),
        escape = FALSE
    )
    # Gene NELSON table
    output$gene_nelson_data <- renderDataTable({
        validate(need(rv$geneofinterest1,""))
        geneofinterest <- rv$geneofinterest1
        df <- data.frame()
        for(gene in geneofinterest){
            df <- rbind(df, create_nelson_association_df(gene))
        }
        ifelse(nrow(df) == 0,"", # if df$Link has no entry do nothing, otherwise reformat for html
               df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>'))
    ### only perform this section if the assocation_df isn't empty ###
    ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df <- select(df, -"Hyperlink") # remove Hyperlink column
            to_remove <- "" # if all rows in a column are blank, then remove the column
            for(i in 1:ncol(df)){
                if(sum((df[,i]) == "") == nrow(df)){
                    ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
                }
            }
            # make sure to_remove actually exists before removing it from df
            ifelse(to_remove != "", df <- select(df, -c(all_of(to_remove))),"")
        } 
    #### done ########################################################
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2))
        ),
        escape = FALSE
    )
    # Disease GWAS table
    output$gene_disease_data <- renderDataTable({
        validate(need(rv$diseaseofinterest1,""))
        diseaseofinterest <- rv$diseaseofinterest1
        # Get list of matching genes
        mapped_genes <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == diseaseofinterest,'MAPPED_GENE']
        returned_genes_list <- c()
        returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
            ifelse(TRUE %in% grepl(gene, mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
        }
        # for event conditioning syntax, returned_genes would need to be "" if empty
        ifelse(returned_genes_list != c(), rv$returned_genes_list <- returned_genes_list, rv$returned_genes_list <- "")
        df <- data.frame()
        for(gene in returned_genes_list){
            assign(("gene_stats"), create_single_gene_stats(gene, x_expr)) # may not need this
            temp_df <- create_gwas_association_df(gene)
            df <- rbind(df, temp_df[temp_df$`Disease/Trait` == diseaseofinterest,])
            # ^subsets the GWAS table only for the disease of interest
        }
        ifelse(nrow(df) == 0,"", # if df$Link has no entry do nothing, otherwise reformat for html
               df$Link <- paste0('<a href="https://',df$Link,'" target="_blank">', df$Link, '</a>'))
        df},
        escape = FALSE
    )
    ### TAB 2
    # TAU Table
    output$gene_detail_table <- renderDataTable({
        validate(need(input$geneofinterest2,""))
        geneofinterest <- rv$geneofinterest2
        assign(("gene_stats"), create_single_gene_stats(geneofinterest, x_expr))
        genestatdf <- data.frame(sample = gene_stats$parent_sample,
                                 cell = gene_stats$cell_type,
                                 state = gene_stats$status,
                                 tau = gene_stats$tau,
                                 skew = gene_stats$skew_values,
                                 p = gene_stats$p_values,
                                 tau_plus = ifelse(gene_stats$skew_values < 0.25, "yes","no"))
        saveRDS(genestatdf,'data_output/geneofinterest_tau_table.rds')
        genestatdf
    })
    ##############################
    ## PLOTS/IMAGES ##############
    ##############################
    ### TAB 1
    ## Main Plot
    output$gene_pvalue <- renderPlot({
        # Save geneofinterest
        geneofinterest <- rv$geneofinterest1
        # Save disease of interest datapoints
        diseaseofinterest <- rv$diseaseofinterest1
        # Erase either geneofinterest or diseaseofinterest depending on the search engine
        searchType <- rv$searchType
    #    ifelse(searchType == 'gene', diseaseofinterest <- "", "")
    #    ifelse(searchType == 'disease', geneofinterest <- "", "")
        # Create gene of interest data frame
        geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% geneofinterest,]
        geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start),]
        # Create disease of interest data frame
        mapped_genes <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == diseaseofinterest,'MAPPED_GENE']
        returned_genes_list <- c()
        returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
            ifelse(TRUE %in% grepl(gene, mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
        }
        rv$returned_genes_list <- returned_genes_list
        returned_genes_list_length <- (length(returned_genes_list)) # use this to set up graph dimensions
        disease_geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% returned_genes_list,]
        disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
        ##### Create the y-positions for the disease/gene annotations:
        unique_disease_x_positions <- unique(disease_geneofinterest_df$start)
        unique_gene_x_positions <- unique(geneofinterest_df$start)
        ##### First get a vector of single unique positions
       y_disease_annot_unique <- c(rep(0,returned_genes_list_length))
       y_gene_annot_unique <- c(rep(0,length(geneofinterest)))
        for(i in 2:(length(unique_disease_x_positions))){
            current_pos <- (unique_disease_x_positions[i])
            previous_pos <- (unique_disease_x_positions[i-1])
            ifelse(current_pos - previous_pos > 15*10^6, 
                   y_disease_annot_unique[i] <- 0, 
                   y_disease_annot_unique[i] <- y_disease_annot_unique[i-1]-1/2)
        }
       for(i in 2:(length(unique_gene_x_positions))){
           current_pos <- (unique_gene_x_positions[i])
           previous_pos <- (unique_gene_x_positions[i-1])
           ifelse(current_pos - previous_pos > 15*10^6, 
                  y_gene_annot_unique[i] <- 0, 
                  y_gene_annot_unique[i] <- y_gene_annot_unique[i-1]-1/2)
       }
            # Then map each position to its gene occurrence 
        y_disease_annot <- c()
        y_gene_annot <- c()
        for(i in 1:length(y_disease_annot_unique)){
            gene_var <- unique(disease_geneofinterest_df$GENE)[i]
            rep_length <- sum(disease_geneofinterest_df$GENE == gene_var)
            y_disease_annot <- c(y_disease_annot, rep(y_disease_annot_unique[i], rep_length))
        }
        for(i in 1:length(y_gene_annot_unique)){
            gene_var <- unique(geneofinterest_df$GENE)[i]
            rep_length <- sum(geneofinterest_df$GENE == gene_var)
            y_gene_annot <- c(y_gene_annot, rep(y_gene_annot_unique[i], rep_length))
        }
            # If there were no disease returns, then set y_disease_annot to 0
        ifelse(returned_genes_list_length == 0, y_disease_annot <- 0, '')
        ifelse(length(geneofinterest) == 0, y_gene_annot <- 0, '')
        ###
        # Include supplementary information if user specifies it
        supp_study <- data.frame()
        ifelse(rv$addStudies == 'study1',
               supp_study <- x_expr_mod, "")
        ifelse(rv$addStudies == 'study2',
               supp_study <- p_cott_carr_will, "")
        #cott_carr_will_df = data.frame()
        # Split data by -10log(p) > or < 300
        p_less_300 <- x_expr_mod[x_expr_mod$p_mod_flag == FALSE,]
        p_more_300 <- x_expr_mod[x_expr_mod$p_mod_flag == TRUE,]
        # Range of plot
        ymin = 0
        ifelse(returned_genes_list_length > 1, ymin <- min(y_disease_annot),'')
        ifelse(length(geneofinterest) > 1, ymin <- min(y_gene_annot),'')
        ymax = 330
        # Get axis breaks
        x_breaks <- seq(0, max(x_expr$start), 10000000)
        first_label <- x_breaks[1]
        rest_of_labels <- x_breaks[2:length(x_breaks)]
        x_labels <- c(paste(first_label, "bp"),
                      paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (20), hjust = 0, vjust = -1), 
                         legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         #legend.position = "right", # removing legend
                         legend.position = "none",
                         axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                         #axis.title.x = element_text(family = "Helvetica", size = (18), colour = "steelblue4", face = "bold"),
                         axis.title.x = element_blank(), # Removing X-title
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10), 
                                                    face = "bold", angle=0, hjust=0.5),
                         panel.background = element_rect(fill = "white"))
        # Create plot
        genepvalue <- ggplot(data = p_less_300, aes(x=start, y=-log10(p_value_mod),
                                                    shape=p_mod_flag, label=GENE, label2=end, 
                                                    group=1)) +
            mytheme + ggtitle("X-Chromosome Escape Profile") + 
            xlab("X-Chromosome Position") + ylab("-log10(p)") + 
            # Add PAR and CENTROMERE shading
            geom_rect(data=NULL, aes(xmin=par1_boundaries[1], xmax=par1_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=par2_boundaries[1], xmax=par2_boundaries[2], ymin=0, ymax=330), 
                      fill="lightblue", alpha=0.25) + 
            geom_rect(data=NULL, aes(xmin=centre_boundaries[1], xmax=centre_boundaries[2], ymin=0, ymax=330), 
                      fill="pink", alpha=0.25)
        if(rv$addStudies != 'empty'){
            genepvalue <- genepvalue + 
                # Add Supplementary Study (escape states) information
                geom_point(supp_study, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                           fill=supp_study$color, colour=supp_study$color, 
                           alpha = 1, size=5, group=2)
            }
        genepvalue <- genepvalue + 
            # Main Data Points
            geom_point(fill = p_less_300$BandColor, size = 2) + 
            geom_point(p_more_300, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill=p_more_300$BandColor, size=2, group=3) +
            # Data points below significance
            geom_point(p_less_300[p_less_300$p_value_mod > P_SIG,],  
                       mapping=aes(x=p_less_300[p_less_300$p_value_mod > P_SIG, 'start'], 
                                   y=-log10(p_less_300[p_less_300$p_value_mod > P_SIG, 'p_value_mod']), 
                                   shape=p_less_300[p_less_300$p_value_mod > P_SIG, 'p_mod_flag']), 
                       fill=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       color=p_less_300[p_less_300$p_value_mod > P_SIG, 'BandColor'], 
                       size=2, group=4) + 
            # Scaling and Legends
            scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) + 
            scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(1,5,20,100,300), limits = c(ymin,ymax)) + 
            # Annotations
            geom_hline(yintercept = -log10(P_SIG), linetype='dotted') + 
            annotate("text", x = max(x_expr$start), y = -log10(P_SIG)+.40, hjust=-0.1, family = "Times New Roman",
                     label = paste0("-log10(p) = ", format(-log10(P_SIG), digits = 3)), size = (4)) + 
            # Data points added by user reactive values: Gene of Interest
            geom_point(geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill='red', size=3, group=2) + 
            annotate("text", label = geneofinterest_df$GENE, x = geneofinterest_df$start, y = y_gene_annot, 
                     color = "red", vjust = 2, group = 5) +
            # Data points added by user reactive values: Disease of Interest
            geom_point(disease_geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                       fill='green', size=3, group=2) + 
            annotate("text", label = disease_geneofinterest_df$GENE, x = disease_geneofinterest_df$start, y = y_disease_annot, 
                     color = "forestgreen", vjust = 2, group = 5) +
            # Scale shape manual (though right now this is disabled)
            scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300")) 
        p <- ggdraw() + 
            draw_plot(genepvalue) + 
            draw_image("images/mainplot_legend.png", x = .44, y = 0.30, scale = 0.10)
            # to shift x left, x -> -1
            # to shift y up, y -> +1
        # Add supplementary legend if user specified
        ifelse(rv$addStudies != 'empty', 
               (p <- p + draw_image("images/mainplot_additional_studies_legend_inactive.png", x = .43, y = 0.10, scale = 0.12)), 
               "")
        p <- genepvalue # for testing
        p
    })
    ## X chromosome "image"
    output$gene_pvalue_xchromosome <- renderPlot({
        # Create theme for plot
        mytheme <- theme(plot.title = element_text(family = "Courier", face = "bold", size = (20), hjust = 0.0), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         legend.position = "none",
                         axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                         axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                         axis.title.x = element_text(family = "Helvetica", size = (18), 
                                                     colour = "steelblue4", face = "bold", hjust = .45, vjust = -1),
                         axis.text.x = element_text(family = "Helvetica", 
                                                    colour = "steelblue4", size = (10), face = "bold", angle=90, hjust=1, vjust = 1),
                         axis.ticks.y = element_blank(),
                         panel.background = element_rect(fill = "white"))
        xchromosome_plot <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod), group=1)) +
            mytheme + 
            xlab("X-Chromosome Position") + ylab(" ") + 
            # Scaling and Legends
            scale_x_continuous(breaks=x_region_breaks, labels = x_region_labels, limits=c(plot1_xmin, plot1_xmax)) + 
            scale_y_continuous(breaks = c(0,1), labels= c("  ","  "), limits = c(-2,0)) + 
            # Add annotions
            annotate("text", x=0, y=-0.5, label="PAR1", size=4, color = "steelblue", hjust=0) + 
            annotate("text", x=max(x_expr$start), y=-0.5, label="PAR2", size=4, color = "steelblue", hjust=.9) +
            annotate("text", x=mean(centre_boundaries[1],centre_boundaries[2])+3e6, y=-0.5, 
                     label="CENTROMERE", size=4, color = "red", alpha = 0.5) +
            # Add chromosome map (yes the ordering is important)
            chrom_segments_colored[c('start','end')] + 
            chrom_segments + 
            chrom_segments_colored[c('par1','par2','centre')]
            # ^these objects contains geom_segment() layers for each band.
            #  Created in utilities/format_plot_aesthetics.R
        xchromosome_plot
    })
    ### TAB 2 
    ## Violin Plots - Gene of Interest
    output$individual_gene_tau_plot <- renderPlot({
        validate(
            need(input$geneofinterest2 !="", "Please input a gene of interest")
        )
        geneofinterest <- rv$geneofinterest2
        #geneofinterest <- "XIST" # For testing
        assign("geneofinterest_stats", create_single_gene_stats(geneofinterest, x_expr))
        # Assign object attributes to variables
        avg_p_value <- geneofinterest_stats$avg_p_value
        min_p_value <- geneofinterest_stats$min_p_value
        max_p_value <- geneofinterest_stats$max_p_value
        avg_tau_value <- geneofinterest_stats$avg_tau_value
        min_tau_value <- geneofinterest_stats$min_tau_value
        max_tau_value <- geneofinterest_stats$max_tau_value
        skew_values <- geneofinterest_stats$skew_values
        avg_skew_value <- geneofinterest_stats$avg_skew
        min_skew_value <- geneofinterest_stats$min_skew
        max_skew_value <- geneofinterest_stats$max_skew
        perc_samples_esc <- geneofinterest_stats$perc_samples_esc
        perc_samples_esc_tauplus <- geneofinterest_stats$perc_samples_esc_tauplus
        # Assign object attributes to variables with skew < 25%
        assign("geneofinterest_tauplus_stats", create_single_gene_stats(geneofinterest, x_expr_tauplus))
        skew_tauplus <- geneofinterest_tauplus_stats$skew_values
        avg_skew_tauplus <- geneofinterest_tauplus_stats$avg_skew
        min_skew_tauplus <- geneofinterest_tauplus_stats$min_skew
        max_skew_tauplus <- geneofinterest_tauplus_stats$max_skew
        tau_tauplus <- geneofinterest_tauplus_stats$tau
        avg_tauplus <- geneofinterest_tauplus_stats$avg_tau_value
        min_tauplus <- geneofinterest_tauplus_stats$min_tau_value
        max_tauplus <- geneofinterest_tauplus_stats$max_tau_value
        # Create tautable
        tautable <- data.frame("tau__tauplus" = c(rep("TAU",length(geneofinterest_stats$tau)),
                                                  rep("TAU+",length(skew_tauplus))),
                               "tau__tauskewvalues25" = c(geneofinterest_stats$tau,tau_tauplus))
        # If there are two or less entries for tau+, remove the
        # tau+ entries from the table (it's not enough info to display)
        if(sum(tautable$tau__tauplus == "TAU+") <= 2) 
        {tautable <- tautable[tautable$tau__tauplus == "TAU",]}
        # My Theme
        mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (22), hjust = 0.5), 
                         legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)), 
                         legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                         legend.position = "right",
                         axis.title = element_text(family = "Courier", size = (22), colour = "steelblue4", face = "bold"),
                         axis.text.x = element_text(family = "Helvetica", colour = "black", size = (18), face = "bold"),
                         axis.title.y = element_blank())
        # Locations for annotations. Some locations will vary depending on:
        # ---if we display the TAU+ violin plot
        # ---if the violin plot is top-heavy or bottom-heavy
        x_center = ifelse(sum(tautable$tau__tauplus == "TAU+") >= 3, 1.5, 1)
        labelx = 1.5
        labely_tau = ifelse(avg_tau_value < 0.25, 0.48, 0.1)
        labely_tauplus = ifelse(avg_tauplus < 0.25, 0.48, 0.1)
        # Create Plot
        geneofinterest_tauplot <- ggplot(tautable, 
              aes(x = tau__tauplus, y = tau__tauskewvalues25, fill = tau__tauplus)) +
            geom_violin(alpha = 0.5) + 
            mytheme + 
            ylim(-0.05,.5) + 
            xlab(geneofinterest) + 
            # Add boxplots
            geom_boxplot(width = 0.03, fill = "white") + 
            # Annotate with tau values and p-values
            annotate("text", x=0.45, y=c(labely_tau,labely_tau-0.025,labely_tau-0.05), 
                     label=c(paste0('avg tau: ', sprintf("%1.2f", avg_tau_value)), 
                             paste0('min tau: ', sprintf("%1.2f", min_tau_value)),
                             paste0('max tau: ', sprintf("%1.2f", max_tau_value))),
                     family = 'Courier', color = "steelblue", size = 4, hjust = 0) + 
            annotate("text", x=1,y=-.03,
                     label=paste0('escapes in ',sprintf("%3.1f",perc_samples_esc*100),'% of samples'),
                     family = 'Courier', color = "black", size = 6, hjust = 0.5, alpha = 0.5) + 
            annotate("text", x=2,y=-.03,
                     label=paste0('escapes in ',sprintf("%3.1f",perc_samples_esc_tauplus*100),'% of samples'),
                     family = 'Courier', color = "black", size = 6, hjust = 0.5, alpha = 0.5) + 
            scale_fill_manual("", values = c("cornflowerblue","purple"))
        # Remove legend if only one violin plot will be displayed
        if(sum(tautable$tau__tauplus == "TAU+") < 2){
            geneofinterest_tauplot <- geneofinterest_tauplot +
                theme(legend.position = "none")
        }
        # If there is enough information to display tau+ data,
        # display it. Criteria: TAU+ needs at least 2 entries.
        if(sum(tautable$tau__tauplus == "TAU+") >= 2){
            geneofinterest_tauplot <- geneofinterest_tauplot + 
                annotate("text", x=2.55, y=c(labely_tauplus,labely_tauplus-0.025,labely_tauplus-0.05), 
                         label=c(paste0('avg tau+: ', sprintf("%1.2f", avg_tauplus)), 
                                 paste0('min tau+: ', sprintf("%1.2f", min_tauplus)),
                                 paste0('max tau+: ', sprintf("%1.2f", max_tauplus))),
                         family = 'Courier', color = "purple", size = 4, hjust = 1) + 
                scale_fill_manual("TAU vs TAU+", values = c("cornflowerblue","purple"), 
                                  labels=c("For all skew values", "Skew < 25%")) + 
                scale_x_discrete(limits = c("TAU","TAU+"))
        }
        geneofinterest_tauplot
    })
    ##################
    ## TAB 3 OUTPUT
    ##################
    
    ##################
    ## TAB 4 OUTPUT
    ##################
    
}

#This lets script know its a shiny app
shinyApp(server = server, ui = ui)