# Karine Moussa
# XCI-app 
# App Settings
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Libraries
library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(dqshiny, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(png, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(magick, warn.conflicts = FALSE)
library(cowplot, warn.conflicts = FALSE)
library(rlist, warn.conflicts = FALSE)

### Source Data and Functions ###
source("utilities/format_input_data.R", local = TRUE)
source("utilities/create_global_variables.R", local = TRUE)
source("utilities/x_expr_mods.R", local = TRUE)
source("utilities/format_additional_studies.R", local = TRUE)
source("utilities/create_association_df.R", local = TRUE)
source("utilities/create_trait_objects.R", local = TRUE)
source("utilities/create_gene_objects.R", local = TRUE)
source("utilities/create_escape_df.R", local = TRUE)
source("utilities/format_plot_aesthetics.R", local = TRUE)

### Load files and pre-processed data
gene_stat_table <- readRDS(file = "data_intermediate/gene_stat_table.rds")

### Save publication date
publication_date <- "2021-02-10 15:58:41 EST" # Sys.time()

### Options for Loading Spinner (for TAB1 main plot) #####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

#############################################################
######### MAIN SHINYAPP STARTS HERE #########################
#############################################################
#source("myUI.R", local = TRUE) # view myUI.R for code
#ui <- myUI
source("myUI_Tab1.R", local = TRUE)
source("myUI_Tab2.R", local = TRUE)
ui <- fluidPage(title = "XCI Data",
                  tabsetPanel(
                    ## TAB 1: Main Plot, disease/trait searches, association tables
                    ## (see myUI_Tab1.R for code)
                    TAB1,
                    ## TAB 2: Individual gene search, tau data
                    ## (see myUI_Tab2.R for code)
                    TAB2,
                    ## TAB 3: Glossary of Terms
                    tabPanel(title = "Terminology",
                             p("Will update..."),
                             p("XCI:"),
                             p("Tau:"),
                             p("Skew:"),
                    ),
                    ## TAB 4: How To Page
                    tabPanel(title = "How To",
                             br(),br(),
                             strong("Search for genes:", style = "font-size:18px"),br(),
                             em("Type or click to add genes", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo1.mp4", height="500px"),br(),
                             br(),br(),br(),
                             strong("Search for diseases/traits:", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo2.mp4", height="500px"),
                             br(),br(),br(),
                             strong("Clear all displayed genes:", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo3.mp4", height="500px"),
                             br(),br(),br(),
                             strong("See escape states (graphically):", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo4.mp4", height="500px"),
                             br(),br(),br(),
                             strong("See escape states (data table):", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo5.mp4", height="500px"),
                             br(),br(),br(),
                             strong("Observe GWAS catalog information:", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo6.mp4", height="500px"),
                             br(),br(),br(),
                             strong("Change escape threshold for main study:", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo7.mp4", height="500px"),
                             br(),br(),br(),
                             strong("Search individual gene characteristics:", style = "font-size:18px"),br(),
                             tags$img(type="mp4",src="xcidemo8.mp4", height="500px")
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
    addStudies = "empty",
    checkbox_input1 = "",
    myclick = "",
    myhover= "",
    plot1_coord_x = c(),
    plot1_coord_y = c(),
    mapped_gene = "",
    closest_expr_index = "",
    returned_genes_list = "",
    slider1 = SV_threshold,
    slider2 = VE_threshold,
    previous_val1 = "",
    previous_val2 = "",
    SV_threshold = SV_threshold,
    VE_threshold = VE_threshold,
    states_filter_study1 = "on",
    states_filter_study2 = "on",
    states_filter_study3 = "on"
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
      if(rv$geneofinterest1[1] == ""){
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
  observeEvent(input$slider1, {
    rv$slider1 <- input$slider1
    # it cant be higher than the VE_threshold
    if(rv$slider1 >= rv$slider2){
      rv$SV_threshold <- SV_threshold # value unchanged
    }
    else{
      rv$SV_threshold <- input$slider1 # value updated
    }
  })
  observeEvent(input$slider2, {
    rv$slider2 <- input$slider2
    # it can't be lower than the SV_threshold
    if(rv$slider1 >= rv$slider2){
      rv$VE_threshold <- rv$VE_threshold # value unchanged
    } else {
      rv$VE_threshold <- input$slider2 # value updated
    }
  })
  # If "filter" check box is updated in study1 escape states table
  observeEvent(input$states_filter_study1, {
    rv$states_filter_study1 <- input$states_filter_study1
  })
  # If "filter" check box is updated in study2 escape states table
  observeEvent(input$states_filter_study2, {
    rv$states_filter_study2 <- input$states_filter_study2
  })
  observeEvent(input$states_filter_study3, {
    rv$states_filter_study3 <- input$states_filter_study3
  })
  # ObserveEvents Tab2
  observeEvent(input$geneofinterest2, { 
    rv$geneofinterest2 <- input$geneofinterest2 
  })
  ##############################
  ## FOR TESTING ###############
  ##############################
  ## for testing (disable/enable this in myUI.R)
  output$test <- renderPrint({ 
    print(paste0("rv$slider1:", rv$slider1))
    print(paste0("rv$SV_threshold:", rv$SV_threshold))
    print(paste0("rv$slider2:", rv$slider2))
    print(paste0("rv$VE_threshold:", rv$VE_threshold))
  })
  ##########################################
  ## CONDITIONAL PANEL STATUS ##############
  ##########################################
  # The logic for whether the tables are displayed 
  output$geneTableStatus <- reactive({
    rv$geneofinterest1 != "" 
  })
  outputOptions(output, "geneTableStatus", suspendWhenHidden = FALSE)
  
  output$diseaseTableStatus <- reactive({
    rv$diseaseofinterest1 != ""
  })
  outputOptions(output, "diseaseTableStatus", suspendWhenHidden = FALSE)
  
  # The logic for the slider warning message
  output$sliderWarning <- reactive({
    rv$slider1 >= rv$slider2
  })
  outputOptions(output, "sliderWarning", suspendWhenHidden = FALSE)
  ##############################
  ## OUTPUT TEXT ###############
  ##############################
  ### TAB 1
  # Genes displayed
  output$displayedGenes <- renderPrint({
    to_display = ""
    if(rv$searchType == "gene") { 
      to_display <- rv$geneofinterest1
    } else {
      to_display <- rv$returned_genes_list
    }
    print(to_display)
  })
  # "please enter" message
  output$pleaseInput <- renderText({
    text <- ""
    if(rv$geneofinterest1[1] == "" & rv$diseaseofinterest1[1] == ""){
      text <- "Input gene or disease of interest for association data"
    }
    text
  })
  ##############################
  ## DOWNLOAD HANDLERS #########
  ##############################
  ### TAB 1
  ## Download escape states 
  output$download_states_study1 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "geuvadis_lcl_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/geuvadis_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study2 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "cott_carr_will_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/cott_carr_will_xstates.rds')
      write.csv(mydata, file)
    }
  )
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
  # Download button for individual gene escape table
  output$individual_gene_escape_download <- downloadHandler(
    filename =  function(){
      # Name of created file
      paste(rv$geneofinterest1, "_individual_escape_table.csv", sep = "")
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/individual_escape_table.rds')
      write.csv(mydata, file)
    }
  )
  ##############################
  ## DATA TABLES ###############
  ##############################
  ### TAB 1
  source("myServer_Tab1_assocTables.R", local = TRUE) # Source for association tables
  ## Gene GWAS table 
  output$gene_gwas_data <- getAssocObjGene("gwas")
  ## Gene NELSON table (currently commented out in myUI_Tab1.R)
  output$gene_nelson_data <- getAssocObjGene("nels")
  ## Disease GWAS table
  output$gene_disease_gwas_data <- getAssocObjDisease("gwas")
  ## Disease Nelson table (currently commented out in myUI_Tab1.R)
  output$gene_disease_nelson_data <- getAssocObjDisease("nels")
  ## Status Table (Study1)
  output$status_table_study1 <- renderDataTable({
    df <- data.frame("Gene" = distinct(x_expr_mod, GENE),
                     "Start (bp) [hg38]" = distinct(x_expr_mod, GENE, start)[,'start'],
                     "Escape Freq" = distinct(x_expr_mod, GENE, perc_samples_esc)[,'perc_samples_esc'],
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$GENE
    if (isTruthy(rv$states_filter_study1)){
      # b. If the study search is 'gene' use 'geneofinterest' reactive value
      # c. If the study search is 'disease' use the 'returned_genes_list' reactive value
      if (isTruthy(rv$searchType == "gene" & rv$geneofinterest1 != "")) { 
        to_display <- rv$geneofinterest1
      }
      if (isTruthy(rv$searchType == "disease" & rv$returned_genes_list != "")) {
        to_display <- rv$returned_genes_list
      }
    }
    df <- df[df$GENE %in% to_display,]
    # State is going to be a little complex because it now
    # depends on the slider inputs.
    # If mean(status == 'E' <= SV_threshold), 'inactive'
    # If mean(status == 'E < SV_threshold <= VE_threshold), 'variable'
    # If mean(status == 'E' > VE-threshold), 'escape'
    for(i in 1:nrow(df)) {
      if(df$`Escape Freq`[i] <= rv$SV_threshold){
        df$State[i] <- 'inactive'
      }
      if(df$`Escape Freq`[i] > rv$SV_threshold & df$`Escape Freq`[i] <= rv$VE_threshold){
        df$State[i] <- 'variable'
      }
      if(df$`Escape Freq`[i] > rv$VE_threshold){
        df$State[i] <- 'escape'
      }
    }
    # Also update format of frequency column
    df$`Escape Freq` <- sprintf("%1.3f", as.numeric(df$`Escape Freq`))
    saveRDS(df,'data_output/geuvadis_xstates.rds')
    df
  })
  ## Status Table (Study2)
  output$status_table_study2 <- renderDataTable({
    df <- data.frame(Gene = cott_carr_will_df$gene,
                     "Start (bp) [hg38]" = cott_carr_will_df$start_mapped,
                     State = cott_carr_will_df$status,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study2)){
    # b. If the study search is 'gene' use 'geneofinterest' reactive value
    # c. If the study search is 'disease' use the 'returned_genes_list' reactive value
      if (isTruthy(rv$searchType == "gene" & rv$geneofinterest1 != "")) { 
        to_display <- rv$geneofinterest1
      }
      if (isTruthy(rv$searchType == "disease" & rv$returned_genes_list != "")) {
        to_display <- rv$returned_genes_list
      }
    }
    df <- df[df$Gene %in% to_display,]
    saveRDS(df,'data_output/cott_carr_will_xstates.rds')
    df
  })
  ## Status Table (Study2)
  output$status_table_study3 <- renderDataTable({
    df <- data.frame(Gene = kat_lin_df$gene,
                     "Start (bp) [hg38]" = kat_lin_df$start_mapped,
                     State = kat_lin_df$status_lb,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study3)){
      # b. If the study search is 'gene' use 'geneofinterest' reactive value
      # c. If the study search is 'disease' use the 'returned_genes_list' reactive value
      if (isTruthy(rv$searchType == "gene" & rv$geneofinterest1 != "")) { 
        to_display <- rv$geneofinterest1
      }
      if (isTruthy(rv$searchType == "disease" & rv$returned_genes_list != "")) {
        to_display <- rv$returned_genes_list
      }
    }
    df <- df[df$Gene %in% to_display,]
    saveRDS(df,'data_output/katsir_linial_lymphoblast_xstates.rds')
    df
  })
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
                             p = sprintf("%1.3f", gene_stats$p_values),
                             tau_plus = ifelse(gene_stats$skew_values < 0.25, "yes","no"))
    saveRDS(genestatdf,'data_output/geneofinterest_tau_table.rds')
    genestatdf
  })
  output$ind_escape_states_table <- renderDataTable({
    validate(need(input$geneofinterest2,""))
    geneofinterest <- rv$geneofinterest2
    df <- create_escape_df(geneofinterest)
    saveRDS(df,'data_output/individual_escape_table.rds')
    df
  })
  ##############################
  ## PLOTS/IMAGES ##############
  ##############################
  ### TAB 1
  ##### Main Plot
  # Local variables and source materials
  plot1_xmin = 0
  plot1_xmax = max(x_expr$start) + 1.3e7
  # Output object
  output$gene_pvalue <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Erase either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start),]
    # Create disease of interest data frame subsetting x_expr_mod data
    mapped_genes_gwas <- c()
    mapped_genes_nels <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      mg_nels <- NELSON_ASSOCIATIONS_2[tolower(NELSON_ASSOCIATIONS_2$MSH) == d,'Gene']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
      if(!identical(mg_nels, character(0))){
        ifelse(is.null(mapped_genes_nels), mapped_genes_nels <- mg_nels, mapped_genes_nels <- c(mapped_genes_nels, mg_nels))
      } 
    }
    mapped_genes <- unique(c(mapped_genes_gwas, mapped_genes_nels))
    returned_genes_list <- c()
    returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
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
    # Finally, if we're observing the Cotton/Carrel study, lower all annotations
    # to make room for the color bar
    if(rv$addStudies == 'study2'){
      y_disease_annot <- y_disease_annot - 0.6
      y_gene_annot <- y_gene_annot - 0.6
    }
    ###
    ##### Include supplementary information if user specifies it
    # Determine the "variable" state of genes in our data set
    SV_threshold <- rv$SV_threshold
    VE_threshold <- rv$VE_threshold
    supp_study <- data.frame()
    ifelse(rv$addStudies == 'study1',
           supp_study <- x_expr_mod, "")
    ifelse(rv$addStudies == 'study2',
           supp_study <- p_cott_carr_will, "")
    # Split data by -10log(p) > or < 300
    p_less_300 <- x_expr_mod[x_expr_mod$p_mod_flag == FALSE,]
    p_more_300 <- x_expr_mod[x_expr_mod$p_mod_flag == TRUE,]
    # Range of plot
    ymin = 0
    ifelse(rv$addStudies == 'study2', ymin <- -1, '')
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
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1), 
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
      annotate("text", x = max(x_expr$start), y = -log10(P_SIG)+.40, hjust=-0.1, family = "serif",
              # label = paste0("-log10(p) = ", format(-log10(P_SIG), digits = 3)), size = (4)) + 
              label = paste0("    p = ", P_SIG), size = (4)) + 
      # Data points added by user reactive values: Gene of Interest
      geom_point(geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                 fill='red', size=3, group=2) + 
      # Change annotation color based on the displayed study
      annotate("text", label = geneofinterest_df$GENE, x = geneofinterest_df$start, y = y_gene_annot, 
               color = "red",
               vjust = 2, group = 5) +
      # Data points added by user reactive values: Disease of Interest
      geom_point(disease_geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                 fill='green', size=3, group=2) + 
      annotate("text", label = disease_geneofinterest_df$GENE, x = disease_geneofinterest_df$start, y = y_disease_annot, 
               color = "forestgreen", vjust = 2, group = 5) +
      # Scale shape manual (though right now this is disabled)
      scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300")) 
    if(rv$addStudies == 'study1'){
      genepvalue <- genepvalue + 
        # Add Supplementary Study (escape states) information after Main Data Points
        geom_point(supp_study, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag), 
                   fill=ifelse(supp_study$perc_samples_esc <= SV_threshold, "lightsteelblue3", 
                               ifelse(supp_study$perc_samples_esc > SV_threshold & supp_study$perc_samples_esc <= VE_threshold, "turquoise3", "purple")),
                   size=3, group=2)
    }
    if(rv$addStudies == 'study2'){
      genepvalue <- genepvalue + 
        annotate("segment", x=cott_carr_will_df[, "start_mapped"], 
                 xend=cott_carr_will_df[, "start_mapped"],
                 y=-.6, yend=-.2, size=1, alpha=0.8,
                 color=cott_carr_will_df[, "color"])
    }
    genepvalue
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