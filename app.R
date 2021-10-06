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
library(dplyr, warn.conflicts = FALSE)
library(png, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(rlist, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(magrittr)

### Source Data/Functions ###
source("utilities/source_all_scripts.R", local = TRUE)

### Save publication date
publication_date <- Sys.time()

### Options for Loading Spinner (for TAB1 main plot) #####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

#############################################################
######### MAIN SHINYAPP STARTS HERE #########################
#############################################################
source("myUI_Tab1.R", local = TRUE)
source("myUI_Tab2.R", local = TRUE)
source("myUI_Tab3.R", local = TRUE)
ui <- fluidPage(title = "x-Viz",
                  tabsetPanel(
                    ## TAB 1: Main Plot, disease/trait searches, association tables
                    ## (see myUI_Tab1.R for code)
                    TAB1,
                    ## TAB 2: Individual gene search, tau data
                    ## (see myUI_Tab2.R for code)
                    TAB2,
                    ## TAB 3: Glossary of Terms
                    TAB3,
                    ## TAB 4: How To Page
                    tabPanel(title = "How To",
                             br(),br(),
                             strong("TAB 1: All Escape Expressions", style = "font-size:20px"),br(),
                             strong("Select a study:", style = "font-size:18px"),br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502584?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='1'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("Search for genes:", style = "font-size:18px"),br(),
                             em("Type to add genes and click ENTER", style = "font-size:18px"),br(),br(),
                             tags$img(type="png",src="HT-gene-select-type.png", height="400px"),br(),br(),
                             em("Or click to add genes", style = "font-size:18px"),br(),
                             tags$img(type="png",src="HT-gene-select-click.png", height="300px"),br(),
                             br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502613?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='2'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("Observe GWAS catalog information:", style = "font-size:18px"),br(),
                             tags$img(type="png",src="HT-gwas.png", height="500px"),br(),br(),
                             a('Link: Additional GWAS information (PDF)', href = "Tutorial-GWAS.pdf", target="_blank", style = "font-size:18px"),
                             br(),br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502668?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='3'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("Search for diseases/traits:", style = "font-size:18px"),br(),
                             tags$img(type="png",src="HT-disease-type-1.png", height="300px"), br(),br(),
                             tags$img(type="png",src="HT-disease-type-2.png", height="400px"),
                             br(),br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502702?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='4'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("Modify Escape Frequency Thresholds:", style = "font-size:18px"),br(),
                             a('Tutorial: Escape Frequency Tresholds (PDF)', href = "Tutorial-Sliders.pdf", target="_blank", style = "font-size:18px"),
                             br(),br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502810?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='5'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("Manually upload studies:", style = "font-size:18px"),br(),
                             a('Tutorial: Manual Upload (PDF)', href = "Tutorial-Upload.pdf", target="_blank", style = "font-size:18px"),
                             br(),br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502846?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='6'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                             strong("TAB 2: Individual Gene Search", style = "font-size:20px"),br(),
                             strong("Search individual gene characteristics:", style = "font-size:18px"),br(),
                             tags$img(type="png",src="HT-individual-search.png", height="500px"),
                             br(),
                             tags$div(
                               HTML("<div style='padding:56.25% 0 0 0;position:relative;'><iframe src='https://player.vimeo.com/video/578502928?badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479' frameborder='0' allow='autoplay; fullscreen; picture-in-picture' allowfullscreen style='position:absolute;top:0;left:0;width:60%;height:60%;' title='tab 2'></iframe></div><script src='https://player.vimeo.com/api/player.js'></script>")
                             ),
                    )
                  )
)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    geneofinterest1 = "",
    geneofinterest1b = "",
    geneofinterest2 = "",
    geneofinterest3 = "",
    diseaseofinterest1 = "",
    searchType = "gene",
    addStudies = "empty",
    checkbox_input1 = "",
    myclick = "",
    myhover= "",
    plot_coord_x = c(),
    plot_coord_y = c(),
    mapped_gene = "",
    closest_expr_index = "",
    returned_genes_list = "",
    slider1 = SV_threshold,
    slider2 = VE_threshold,
    previous_val1 = "",
    previous_val2 = "",
    SV_threshold = SV_threshold,
    VE_threshold = VE_threshold,
    states_filter_study0 = "on",
    states_filter_study1 = "on",
    tissues_filter_study1 = "on",
    states_filter_study6 = "on",
    tissues_filter_study6 = "on",
    #states_filter_study2 = "on",
    filter_study2 = 1,
    states_filter_study3 = "on",
    states_filter_study4 = "on",
    states_filter_study5 = "on",
    study0_df = data.frame(),
    study0_genes = c(),
    study0_flag = FALSE,
    includes_start = TRUE,
    ready1 = FALSE
  )
  # zoom in out
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # ObserveEvents Tab 1
  observeEvent(input$file1, {
    # need to reset settings if input$file1 changes
    rv$study0_flag = FALSE
    rv$study0_df = data.frame()
    rv$study0_genes = c()
  })
  observeEvent(input$template, {
    # need to reset settings if template type changes
    rv$study0_flag = FALSE
    rv$study0_df = data.frame()
    rv$study0_genes = c()
    create_template(input$template, rv$includes_start)
  })
  observeEvent(input$includes_start, {
    rv$includes_start <- input$includes_start
    create_template(input$template, rv$includes_start)
  })
  observeEvent(input$submitButton1, { # submit study 0
    rv$study0_df <- read.csv(input$file1$datapath,
                             sep = ",",
                             #sep = input$sep, # uncomment in myUI_Tab1.R
                             check.names = FALSE)
    rv$study0_genes <- toupper(rv$study0_df[,1]) # first column of input df
    rv$study0_flag <- TRUE
    updateSelectizeInput(session,
                         inputId = "geneofinterest1_0",
                         choices = rv$study0_genes,
                         selected = NULL)
    updateSelectizeInput(session,
                         inputId = "geneofinterest2",
                         server = TRUE,
                         choices = unique(c(rv$study0_genes, study1_genes, study2_genes,
                                     study3_genes, study4_genes, study5_genes,
                                     study6_genes)),
                         selected = NULL)
  })
  observeEvent(input$geneofinterest1_0, { # conditional panel for study 0
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_0, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_1, { # conditional panel for study 1
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_1, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_2, { # conditional panel for study 2
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_2, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_3, { # conditional panel for study 3
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_3, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_4, { # conditional panel for study 4
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_4, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_5, { # conditional panel for study 5
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_5, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_6, { # conditional panel for study 6
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_6, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_7, { # conditional panel for study 7
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_7, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_8, { # conditional panel for study 8
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_8, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_9, { # conditional panel for study 9
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_9, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$geneofinterest1_10, { # conditional panel for study 10
    rv$geneofinterest1 <- unique(c(input$geneofinterest1_10, rv$mapped_gene))
    rv$geneofinterest1 <- rv$geneofinterest1[rv$geneofinterest1 != ""]
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$diseaseofinterest1, { 
    rv$diseaseofinterest1 <- input$diseaseofinterest1
    if (rv$diseaseofinterest1 == "ALL FEMALE BIAS TRAITS"){
      rv$diseaseofinterest1 <- readRDS("data_intermediate/fbias_traits.rds")
    }
    rv$searchType <- input$searchType
    rv$addStudies <- input$addStudies
  })
  observeEvent(input$searchType, {
    previous_searchtype <- rv$searchType
    rv$searchType <- input$searchType
    if(previous_searchtype != rv$searchType) {
      rv$mapped_gene = ""
      rv$geneofinterest1 = ""
      rv$diseaseofinterest1 = ""
      rv$plot_coord_x = c()
      rv$plot_coord_y = c()
      rv$closest_expr_index = ""
      rv$returned_genes_list = ""
    }
    rv$addStudies <- input$addStudies
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
    previous_study <- rv$addStudies
    rv$addStudies <- input$addStudies
    rv$diseaseofinterest1 <- input$diseaseofinterest1
    if (rv$diseaseofinterest1 == "ALL FEMALE BIAS TRAITS"){
      rv$diseaseofinterest1 <- readRDS("data_intermediate/fbias_traits.rds")
    }
    if(previous_study != rv$addStudies) {
      rv$mapped_gene <- ""
      rv$geneofinterest1 <- ""
      rv$plot_coord_x <- c()
      rv$plot_coord_y <- c()
      rv$closest_expr_index <- ""
      rv$returned_genes_list <- ""
    }
    if(rv$addStudies != "study0"){
      rv$study0_flag = FALSE
    }
  })
  observeEvent(input$study2_dblclick, {
    # Zoom in out
    brush <- input$study2_brush # will need to change if more studies
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  observeEvent(input$myclick, {
    if(rv$searchType == 'gene'){
      rv$plot_coord_x = c(rv$plot_coord_x, input$myclick$x)
      rv$plot_coord_y = c(rv$plot_coord_y, input$myclick$y)
      # Query study 0
      study0_tempdf <- readRDS("rds/study0_df.rds") # make sure there's start column
      if(rv$addStudies == "study0" & "start" %in% colnames(study0_tempdf)){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(rv$study0_df$start - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = rv$study0_df$gene[as.numeric(rv$closest_expr_index)]
      }
      # Query study 1
      if(rv$addStudies == "study1"){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(x_expr$start - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = x_expr$GENE[as.numeric(rv$closest_expr_index)]
      }
      # Query study 2
      # zoom in out
      # if(rv$addStudies == "study2"){
      #   for(i in 1:length(rv$plot_coord_x)){
      #     cott_df <- cott_carr_will_df[cott_carr_will_df$status_cott != "NA",]
      #     index <- which.min(abs(cott_df$start_mapped - rv$plot_coord_x[i]))
      #     rv$closest_expr_index[i] <- index
      #   }
      #   rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
      #   rv$mapped_gene = cott_df$gene[as.numeric(rv$closest_expr_index)]
      # }
      # Query study 3
      if(rv$addStudies == "study3"){
        for(i in 1:length(rv$plot_coord_x)){
          carrwill_df <- cott_carr_will_df[cott_carr_will_df$status_carrwill != "NA",]
          index <- which.min(abs(carrwill_df$start_mapped - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = carrwill_df$gene[as.numeric(rv$closest_expr_index)]
      }
      # Query study 4
      if(rv$addStudies == "study4"){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(kat_lin_df_lb$start_mapped - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = kat_lin_df_lb$gene[as.numeric(rv$closest_expr_index)]
      }
      # Query study 5
      if(rv$addStudies == "study5"){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(kat_lin_df_fb$start_mapped - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = kat_lin_df_fb$gene[as.numeric(rv$closest_expr_index)]
      }
      # Query study 6
      if(rv$addStudies == "study6"){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(TukGTExMod$`start_mapped` - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = TukGTExMod$`Gene name`[as.numeric(rv$closest_expr_index)]
      }
      # Query study 7
      if(rv$addStudies == "study7"){
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(cotton_mDNA$POS - rv$plot_coord_x[i]))
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = cotton_mDNA$GENE[as.numeric(rv$closest_expr_index)]
      }
      # Query study 8
      if(rv$addStudies == "study8"){ # change here
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(balbrown_mCEMT$START - rv$plot_coord_x[i])) # change here
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = balbrown_mCEMT$GENE[as.numeric(rv$closest_expr_index)] # change here
      }
      # Query study 9
      if(rv$addStudies == "study9"){ # change here
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(balbrown_CREST$START - rv$plot_coord_x[i])) # change here
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = balbrown_CREST$GENE[as.numeric(rv$closest_expr_index)] # change here
      }
      # Query study 10
      if(rv$addStudies == "study10"){ # change here
        for(i in 1:length(rv$plot_coord_x)){
          index <- which.min(abs(TukDEG$START - rv$plot_coord_x[i])) # change here
          rv$closest_expr_index[i] <- index
        }
        rv$closest_expr_index <- unique(rv$closest_expr_index) # remove duplicates
        rv$mapped_gene = TukDEG$GENE[as.numeric(rv$closest_expr_index)] # change here
      }
      ###
      if(rv$geneofinterest1[1] == ""){
        rv$geneofinterest1 = rv$mapped_gene
      } else {
        rv$geneofinterest1 = unique(c(rv$geneofinterest1, rv$mapped_gene))
      }
    }
  })
  observeEvent(input$resetButton1, {
    rv$mapped_gene = ""
    rv$geneofinterest1 = ""
    rv$diseaseofinterest1 = ""
    rv$plot_coord_x = c()
    rv$plot_coord_y = c()
    rv$closest_expr_index = ""
    rv$returned_genes_list = ""
    #rv$addStudies = ""
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
  # If "filter" check box is changed, updated escape states table
  observeEvent(input$states_filter_study0, {
    rv$states_filter_study0 <- input$states_filter_study0
  })
  observeEvent(input$states_filter_study1, {
    rv$states_filter_study1 <- input$states_filter_study1
  })
  observeEvent(input$tissues_filter_study1, {
    rv$tissues_filter_study1 <- input$tissues_filter_study1
  })
  observeEvent(input$states_filter_study6, {
    rv$states_filter_study6 <- input$states_filter_study6
  })
  observeEvent(input$tissues_filter_study6, {
    rv$tissues_filter_study6 <- input$tissues_filter_study6
  })
  observeEvent(input$filter_study2, {
    rv$filter_study2 <- input$filter_study2
  })
  observeEvent(input$states_filter_study3, {
    rv$states_filter_study3 <- input$states_filter_study3
  })
  observeEvent(input$states_filter_study4, {
    rv$states_filter_study4 <- input$states_filter_study4
  })
  observeEvent(input$states_filter_study5, {
    rv$states_filter_study5 <- input$states_filter_study5
  })
  observeEvent(input$states_filter_study7, {
    rv$states_filter_study7 <- input$states_filter_study7
  })
  observeEvent(input$tissues_filter_study7, {
    rv$tissues_filter_study7 <- input$tissues_filter_study7
  })
  observeEvent(input$states_filter_study8, {
    rv$states_filter_study8 <- input$states_filter_study8
  })
  observeEvent(input$states_filter_study9, {
    rv$states_filter_study9 <- input$states_filter_study9
  })
  observeEvent(input$states_filter_study10, {
    rv$states_filter_study10 <- input$states_filter_study10
  })
  # ObserveEvents Tab2
  observeEvent(input$geneofinterest2, {
    rv$geneofinterest2 <- input$geneofinterest2
  })
  observeEvent(input$resetButton2, {
    rv$geneofinterest2 <- ""
    rv$tauStudy <- ""
  })
  observeEvent(input$tauStudy, {
    rv$tauStudy <- input$tauStudy
  })
  # ObserveEvents Tab3
  observeEvent(input$geneofinterest3, {
    rv$geneofinterest3 <- input$geneofinterest3
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
  # The logic for whether tables or messages are displayed
  output$ready1 <- reactive({
    rv$ready1 != FALSE
  })
  outputOptions(output, "ready1", suspendWhenHidden = FALSE)
  
  output$geneTableStatus1 <- reactive({
    rv$geneofinterest1 != ""
  })
  outputOptions(output, "geneTableStatus1", suspendWhenHidden = FALSE)

  output$diseaseTableStatus <- reactive({
    rv$diseaseofinterest1 != ""
  })
  outputOptions(output, "diseaseTableStatus", suspendWhenHidden = FALSE)

  output$geneTableStatus2 <- reactive({
    rv$geneofinterest2 != ""
  })
  outputOptions(output, "geneTableStatus2", suspendWhenHidden = FALSE)
  
  output$geneTableStatus3 <- reactive({
    rv$geneofinterest3 != ""
  })
  outputOptions(output, "geneTableStatus3", suspendWhenHidden = FALSE)
  
  output$plotStudy0 <- reactive({
    rv$study0_flag == TRUE
  })
  outputOptions(output, "plotStudy0", suspendWhenHidden = FALSE)
  
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
  output$pleaseInput1 <- renderText({
    text <- ""
    if(rv$geneofinterest1[1] == "" & rv$diseaseofinterest1[1] == ""){
      text <- "Select a study, then input gene or disease of interest for association data"
    }
    text
  })
  output$pleaseInput2 <- renderText({
    text <- ""
    if(rv$addStudies == "empty"){
      text <- "READY. Select a study."
      rv$ready1 <- TRUE
    }
    text
  })
  output$pleaseInput3 <- renderText({
    text <- ""
    if(rv$geneofinterest2[1] == ""){
      text <- "READY. Select a gene"
    }
    text
  })
  output$pleaseInput4 <- renderText({
    text <- ""
    if(rv$geneofinterest3[1] == ""){
      text <- "Select a gene"
    }
    text
  })
  ##############################
  ## DOWNLOAD HANDLERS #########
  ##############################
  ### TAB 1
  ## Download manual study templates
  output$template_download <- downloadHandler(
    filename = function(){
      "TEMPLATE.csv"
    },
    content = function(file){
      mydata <- readRDS('rds/usr_temp.rds')
      write.csv(mydata, file, row.names = FALSE)
    }
  )
  ## Download escape states
  output$download_states_study0 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "uploaded_study_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/uploaded_study_escape_states.rds')
      write.csv(mydata, file)
    }
  )
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
  output$download_states_study6 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "gtex_vp6_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/gtex_v6p_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study2 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "cott_fibr_lymph_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/cott_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study3 <- downloadHandler( 
    filename =  function(){
      # Name of created file
      "carr_will_escape_states.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/carr_will_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study4 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "katsir_linial_lymphoblast_xstates.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/katsir_linial_lymphoblast_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study5 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "katsir_linial_fibroblast_xstates.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/katsir_linial_fibroblast_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study7 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "cott_mDNA_multi-tiss_xstates.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/cott_mDNA_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study8 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "balbrown_DNAm_CEMT.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/balbrown_CEMT_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study9 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "balbrown_DNAme_CREST.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/balbrown_mCREST_xstates.rds')
      write.csv(mydata, file)
    }
  )
  output$download_states_study10 <- downloadHandler(
    filename =  function(){
      # Name of created file
      "tuk_DGEA.csv"
    },
    content = function(file){
      # Get the data source
      mydata <- readRDS('data_output/tuk_DGEA_xstates.rds') # come back here
      write.csv(mydata, file)
    }
  )
  ### TAB 2
  # Tau Download button for geneofinterest
  output$table1_download <- downloadHandler(
    filename =  function(){
      # Name of created file
      paste(rv$geneofinterest3, "_tau_table.csv", sep = "")
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
      paste(rv$geneofinterest1, "gene_escape_table.csv", sep = "")
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
  output$gene_gwas_data <- getAssocObjGene()
  ## Disease GWAS table
  output$gene_disease_gwas_data <- getAssocObjDisease()
  ## Status Table (Study0)
  output$status_table_study0 <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    input_df <- read.csv(input$file1$datapath,
                   sep = ",",
                   #sep = input$sep, # uncomment in myUI_Tab1.R
                   check.names = FALSE)
    # Before the submit button is hit, df will just be
    # the input df
    if(rv$study0_flag == FALSE){
      df <- input_df
      colnames(df) <- c("gene", colnames(df)[2:length(colnames(df))]) # fix first col name
      saveRDS(df, "data_output/uploaded_study_escape_states.rds")
      df
    } else {
      # Calculate escape freq 
      df <- input_df
      colnames(df) <- c("gene", colnames(df)[2:length(colnames(df))]) # fix first col name
      df[,"samp_state"] <- tolower(df[,"samp_state"])
      for(row in 1:nrow(df)){
        subset_df <- subset(df, gene == df[row,1])
        df$escape_freq[row] <- mean(subset_df$`samp_state` == "escape")
      }
      # Filter out non-genes of interest
      to_display = df$gene
      if (isTruthy(rv$states_filter_study0)){
        # b. If the study search is 'gene' use 'geneofinterest' reactive value
        # c. If the study search is 'disease' use the 'returned_genes_list' reactive value
        if (isTruthy(rv$searchType == "gene" & rv$geneofinterest1 != "")) {
          to_display <- rv$geneofinterest1
        }
        if (isTruthy(rv$searchType == "disease" & rv$returned_genes_list != "")) {
          to_display <- rv$returned_genes_list
        }
      }
      # Add final escape states and color
      for(i in 1:nrow(df)) {
        if(df$`escape_freq`[i] <= rv$SV_threshold){
          df$state[i] <- 'inactive'
          df$color[i] <- 'lightsteelblue'
        }
        if(df$`escape_freq`[i] > rv$SV_threshold & df$`escape_freq`[i] <= rv$VE_threshold){
          df$state[i] <- 'variable'
          df$color[i] <- 'turquoise3'
        }
        if(df$`escape_freq`[i] > rv$VE_threshold){
          df$state[i] <- 'escape'
          df$color[i] <- 'purple'
        }
      }
      last_column <- length(colnames(df)) - 1
      rv$study0_df <- df # save the reactive value for plotting later
      df <- df[df$gene %in% to_display,]
      saveRDS(df[,1:last_column], "data_output/uploaded_study_escape_states.rds") # save for downloading
      saveRDS(df[,1:last_column], "rds/study0_df.rds") # save for tab 2
      df[,1:last_column] # display table, exclude the color # commented out for testing
    } # end of "if rv$study0_flag == TRUE"
  })
  ## Status Table (Study1)
  output$status_table_study1 <- renderDataTable({
    if (!isTruthy(rv$tissues_filter_study1)){
      df <- data.frame("Gene" = distinct(x_expr_mod, GENE),
                       "Start (bp) [hg38]" = distinct(x_expr_mod, GENE, start)[,'start'],
                       "Escape Freq" = distinct(x_expr_mod, GENE, perc_samples_esc)[,'perc_samples_esc'],
                       check.names = FALSE)
    } else {
      df <- data.frame("Gene" = x_expr_mod$GENE[order(x_expr_mod$start)],
                       "Start (bp) [hg38]" = x_expr_mod$start[order(x_expr_mod$start)],
                       "Sample" = x_expr_mod$sample[order(x_expr_mod$start)],
                       "Sample State" = x_expr_mod$status[order(x_expr_mod$start)],
                       "Escape Freq" = x_expr_mod$perc_samples_esc[order(x_expr_mod$start)],
                       check.names = FALSE)
    }
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df[,1]
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
    df <- df[df[,1] %in% to_display,]
    # The rest can only be performed if the data table is populated
    if(nrow(df) != 0){
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
      # If all of the samples are being displayed, make sure the sample state 
      # is correctly formatted:
      if(isTruthy(rv$tissues_filter_study1)){
        df$`Sample State` <- ifelse(df$`Sample State` == "S", "inactive",
                                    ifelse(df$`Sample State` == "E", "escape",""))
      }
      # Also update format of frequency column
      df$`Escape Freq` <- sprintf("%1.3f", as.numeric(df$`Escape Freq`))
    }
    saveRDS(df,'data_output/geuvadis_xstates.rds')
    df
  })
  ## Status Table (Study2)
  output$status_table_study2 <- renderDataTable({
    df <- data.frame(Gene = cott_carr_will_df$gene,
                     "Start (bp) [hg38]" = cott_carr_will_df$start_mapped,
                     "End (bp) [hg38]" = cott_carr_will_df$end_mapped,
                     State = cott_carr_will_df$status_cott,
                     "Gene Link" = cott_carr_will_df$gene_link,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (rv$filter_study2 == 1){
    # b. If the study search is 'gene' use 'geneofinterest' reactive value
    # c. If the study search is 'disease' use the 'returned_genes_list' reactive value
      if (isTruthy(rv$searchType == "gene" & rv$geneofinterest1 != "")) {
        to_display <- rv$geneofinterest1
      }
      if (isTruthy(rv$searchType == "disease" & rv$returned_genes_list != "")) {
        to_display <- rv$returned_genes_list
      }
    } 
    if (rv$filter_study2 == 2){ # zoom in out
      # Return only the genes that are between the min and max of the plot
      if(!is.null(ranges$x)){ # make sure we have ranges
        to_display <- df$Gene[(as.numeric(df$`End (bp) [hg38]`) > ranges$x[1] & as.numeric(df$`Start (bp) [hg38]`) < ranges$x[2])]
        to_display <- to_display[!is.na(to_display)]
      }
    }
    df <- df[df$Gene %in% to_display,]
    df <- df[df$State != "NA",]
    saveRDS(df,'data_output/cott_xstates.rds')
    # If df isn't empty, make hyperlinks for genes
    if(nrow(df) != 0){
      df$Gene <- paste0('<a href="', df$`Gene Link`,'" target="_blank">', df$`Gene`, '</a>')
      df <- df[, -5] # remove Hyperlink column 
    }
    df
  }, escape = FALSE # this allows hyperlinks to be active
  )
  ## Status Table (Study3) (similar to Study2)
  output$status_table_study3 <- renderDataTable({
    df <- data.frame(Gene = cott_carr_will_df$gene,
                     "Start (bp) [hg38]" = cott_carr_will_df$start_mapped,
                     "End (bp) [hg38]" = cott_carr_will_df$end_mapped,
                     State = cott_carr_will_df$status_carrwill,
                     "Gene Link" = cott_carr_will_df$gene_link,
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
    df <- df[df$State != "NA",]
    saveRDS(df,'data_output/carr_will_xstates.rds')
    # If df isn't empty, make hyperlinks for genes
    if(nrow(df) != 0){
      df$Gene <- paste0('<a href="', df$`Gene Link`,'" target="_blank">', df$`Gene`, '</a>')
      df <- df[, -5] # remove Hyperlink column 
    }
    df
  }, escape = FALSE # this allows hyperlinks to be active
  )
  ## Status Table (Study4)
  output$status_table_study4 <- renderDataTable({
    df <- data.frame(Gene = kat_lin_df_lb$gene,
                     "Start (bp) [hg38]" = kat_lin_df_lb$start_mapped,
                     State = kat_lin_df_lb$status_lb,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study4)){
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
    df <- df[!grepl("TCON", df$Gene),]
    saveRDS(df,'data_output/katsir_linial_lymphoblast_xstates.rds')
    df
  })
  ## Status Table (Study5) (similar to Study4)
  output$status_table_study5 <- renderDataTable({
    df <- data.frame(Gene = kat_lin_df_fb$gene,
                     "Start (bp) [hg38]" = kat_lin_df_fb$start_mapped,
                     State = kat_lin_df_fb$status_fb,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study5)){
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
    df <- df[!grepl("TCON", df$Gene),]
    saveRDS(df,'data_output/katsir_linial_fibroblast_xstates.rds')
    df
  })
  ## Status Table (Study6) (similar to Study1)
  output$status_table_study6 <- renderDataTable({
    df <- data.frame("Gene" = TukGTExMod$`Gene name`,
                     "Start (bp) [hg38]" = TukGTExMod$`start_mapped`,
                     "Tissue" = TukGTExMod$Tissue,
                     "Tissue State" = TukGTExMod$`Incomplete XCI`,
                     "Escape Freq" = TukGTExMod$perc_tissues_esc,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study6)){
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
    # Filter out the Tissue column if only a summary is needed
    if (!isTruthy(rv$tissues_filter_study6)){
      df <- df[,-c(3,4)]
      df <- unique(df)
    }
    # The rest can only be performed if the data table is populated
    if(nrow(df) != 0){
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
        if(isTruthy(rv$tissues_filter_study6)){ # only do this if the column still exists
          if(df$`Tissue State`[i] == "FALSE"){
            df$`Tissue State`[i] = "inactive"
          }
          if(df$`Tissue State`[i] == "TRUE"){
            df$`Tissue State`[i] = "escape"
          }
        }
      }
      # Update format of frequency column
      df$`Escape Freq` <- sprintf("%1.3f", as.numeric(df$`Escape Freq`))
      # Update name of state column
      colnames(df) <- c(colnames(df)[-ncol(df)], "State (based on esc freq)")
    }
    saveRDS(df,'data_output/gtex_v6p_xstates.rds')
    df
  })
  ## Status Table (Study7) (similar to Study6)
  output$status_table_study7 <- renderDataTable({
    df <- data.frame("Gene" = cotton_mDNA$GENE,
                     "Start (bp) [hg38]" = cotton_mDNA$POS,
                     "Tissue" = cotton_mDNA$FULL_TISS,
                     "Tissue State" = cotton_mDNA$TISS_STATE,
                     "Escape Status" = cotton_mDNA$STATUS,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study7)){
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
    # Filter out the Tissue column if only a summary is needed
    if (!isTruthy(rv$tissues_filter_study7)){
      df <- df[,-c(3,4)]
      df <- unique(df)
    }
    saveRDS(df,'data_output/cott_mDNA_xstates.rds')
    df
  })
  ## Status Table (Study8) 
  output$status_table_study8 <- renderDataTable({
    df <- data.frame("Gene" = balbrown_mCEMT$GENE,
                     "Start (bp) [hg38]" = balbrown_mCEMT$START,
                     "Escape Status" = balbrown_mCEMT$STATUS,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study8)){
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
    saveRDS(df,'data_output/balbrown_mCEMT_xstates.rds')
    df
  })
  ## Status Table (Study9) 
  output$status_table_study9 <- renderDataTable({
    df <- data.frame("Gene" = balbrown_CREST$GENE,
                     "Start (bp) [hg38]" = balbrown_CREST$START,
                     "Escape Status" = balbrown_CREST$STATUS,
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study9)){
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
    saveRDS(df,'data_output/balbrown_CREST_xstates.rds')
    df
  })
  ## Status Table (Study10) 
  output$status_table_study10 <- renderDataTable({
    df <- data.frame("Gene" = TukDEG$GENE, # change here
                     "Start (bp) [hg38]" = TukDEG$START, # change here
                     "Bias Status" = TukDEG$BIAS, # change here
                     check.names = FALSE
    )
    # Filter the df based on what genes are being displayed
    # (only filter if the "filter" check box is true)
    # a. By default, it displays ALL genes
    to_display = df$Gene
    if (isTruthy(rv$states_filter_study10)){ # don't forget to change here
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
    saveRDS(df,'data_output/tuk_DGEA_xstates.rds')
    df
  })
  ### TAB 2
  # Individual Escape Table
  output$ind_escape_states_table <- renderDataTable({
    #validate(need(input$geneofinterest2,""))
    geneofinterest <- rv$geneofinterest2
    dt <- data.table()
    if (length(geneofinterest) != 0) {
      dt <- create_escape_df(geneofinterest)
    }
    saveRDS(dt,'data_output/individual_escape_table.rds')
    dt
  })
  # TAU detail table
  output$gene_detail_table <- renderDataTable({
    validate(need(input$geneofinterest3,""))
    geneofinterest <- rv$geneofinterest3
    for (gene in geneofinterest){
      assign(("gene_stats"), create_single_gene_stats(geneofinterest, x_expr))
      tmp_genestatdf <- data.frame(sample = gene_stats$parent_sample,
                                   cell = gene_stats$cell_type,
                                   state = gene_stats$status,
                                   tau = gene_stats$tau,
                                   skew = gene_stats$skew_values,
                                   p = sprintf("%1.3f", gene_stats$p_values),
                                   tau_plus = ifelse(gene_stats$skew_values < 0.25, "yes","no"))
      # Don't bind a new df if this was the first gene on the list
      if(gene == geneofinterest[1]){
        genestatdf <- tmp_genestatdf
      } else {
        genestatdf <- rbind(genestatdf, tmp_genestatdf)
      }
    }
    saveRDS(genestatdf,'data_output/geneofinterest_tau_table.rds')
    genestatdf
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
  output$plot_study0 <- renderPlot({
    validate(need(rv$study0_flag,""))
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Study0 genes / df
    study0_genes <- rv$study0_genes
    study0_df <- rv$study0_df
    # Create gene of interest data frame
    geneofinterest_df <- study0_df[study0_genes %in% geneofinterest,]
    if("start" %in% colnames(study0_df)){
      geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start),]
    }
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study0_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- study0_df[study0_genes %in% returned_genes_list,]
    if("start" %in% colnames(study0_df)){
      disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    }
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot (only if 'start' information is included)
    if("start" %in% colnames(study0_df)){
      p0 <- ggplot(data = study0_df, aes(x=start, y=start)) +
        mytheme + ggtitle("X-Chromosome Escape Profile") +
        xlab("X-Chromosome") + ylab("") + 
        # Add points
        geom_segment(data = study0_df, 
                     aes(x=study0_df[, "start"], y=0,
                         xend=study0_df[, "start"], yend=ymax-1),
                     color=study0_df[,"color"]
        ) + 
        # Scaling and Legends
        scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
        scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
      # Data points added by user reactive values: Gene of Interest
      if(nrow(geneofinterest_df) != 0){
        p0 <- p0 + geom_segment(data = geneofinterest_df, 
                                aes(x=geneofinterest_df[, "start"], y=ymin,
                                    xend=geneofinterest_df[, "start"], yend=ymax-1),
                                color='red')
      }
      # Data points added by user reactive values: Disease of Interest
      #if(nrow(disease_geneofinterest_df) != 0){
      #  p0 <- p0 + geom_segment(data = disease_geneofinterest_df, 
      #                          aes(x=disease_geneofinterest_df[, "start"], y=ymin,
      #                              xend=disease_geneofinterest_df[, "start"], yend=ymax-1),
      #                          color='red')
      #}
      p0
    }
  })
  output$plot_study1 <- renderCachedPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start),]
    # Create disease of interest data frame subsetting x_expr_mod data
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study1_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    returned_genes_list_length <- (length(returned_genes_list)) # use this to set up graph dimensions
    disease_geneofinterest_df <- x_expr_mod[x_expr_mod$GENE %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    ##### Create the x-positions for the disease/gene annotations:
    unique_disease_x_positions <- unique(disease_geneofinterest_df$start)
    unique_gene_x_positions <- unique(geneofinterest_df$start)
    ##### Create the x-positions for the disease/gene annotations:
    # First get a vector of single unique positions
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
    # Finally, if we're observing the a supplementary study, lower all annotations
    # to make room for the color bar
    if(rv$addStudies != 'empty' & rv$addStudies != 'study1'){
      y_disease_annot <- y_disease_annot - 0.6
      y_gene_annot <- y_gene_annot - 0.6
    }
    ###
    ##### Include supplementary information if user specifies it
    # Determine the "variable" state of genes in our data set
    SV_threshold <- rv$SV_threshold
    VE_threshold <- rv$VE_threshold
    escape_states <- data.frame()
    ifelse(rv$addStudies == 'study1',
           escape_states <- x_expr_mod, "")
    # Split data by -10log(p) > or < 300
    p_less_300 <- x_expr_mod[x_expr_mod$p_mod_flag == FALSE,]
    p_more_300 <- x_expr_mod[x_expr_mod$p_mod_flag == TRUE,]
    # Range of plot
    ymin = 0
    ifelse(rv$addStudies != 'empty' & rv$addStudies != 'study1', ymin <- -1, '')
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
    genepvalue_1 <- ggplot(data = p_less_300, aes(x=start, y=-log10(p_value_mod),
                                                shape=p_mod_flag,
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
    # Scaling and Legends
    genepvalue_1 <- genepvalue_1 +
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=c(1,5,20,100,300), limits = c(ymin,ymax)) +
      # Annotations
      geom_hline(yintercept = -log10(P_SIG), linetype='dotted') +
      annotate("text", x = max(x_expr$start), y = -log10(P_SIG)+.40, hjust=-0.1, family = "serif",
              # label = paste0("-log10(p) = ", format(-log10(P_SIG), digits = 3)), size = (4)) +
              label = paste0("    p = ", P_SIG), size = (4))
    # Add escape colors  
    if(rv$addStudies == 'study1'){
        genepvalue_1 <- genepvalue_1 +
          # Add Escape State information after Main Data Points
          geom_point(escape_states, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag),
                     fill=ifelse(escape_states$perc_samples_esc <= SV_threshold, "lightsteelblue3",
                                 ifelse(escape_states$perc_samples_esc > SV_threshold & escape_states$perc_samples_esc <= VE_threshold, "turquoise3", "purple")),
                     size=3, group=2)
      }
      # Data points added by user reactive values: Gene of Interest
      genepvalue_1 <- genepvalue_1 +
        geom_point(geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag),
                 fill='red', size=3, group=2) +
      # Change annotation color based on the displayed study
      annotate("text", label = geneofinterest_df$GENE, x = geneofinterest_df$start, y = y_gene_annot,
               color = "red",
               vjust = 2, group = 5) +
      # Data points added by user reactive values: Disease of Interest
      geom_point(disease_geneofinterest_df, mapping=aes(x=start, y=-log10(p_value_mod), shape=p_mod_flag),
                 fill='red', size=3, group=2) +
      annotate("text", label = disease_geneofinterest_df$GENE, x = disease_geneofinterest_df$start, y = y_disease_annot,
               color = "red", vjust = 2, group = 5) + 
      # Scale shape manual (though right now this is disabled)
      scale_shape_manual("-log10(p)", values=c(21,24), labels=c("< 300", ">= 300"))
    genepvalue_1
  }, 
  cacheKeyExpr = { list(input$addStudies, input$diseaseofinterest1, 
                        rv$mapped_gene, rv$geneofinterest1, 
                        input$searchType,
                        rv$SV_threshold, rv$VE_threshold) }, 
  cache = "app",
  )
  output$plot_study2 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Get Range of plot (zoom in out)
    ymin = 0
    ymax = 10
    if (is.null(ranges$x)){
      xmin <- plot1_xmin
      xmax <- plot1_xmax
    } else {
      xmin <- ranges$x[1]
      xmax <- ranges$x[2]
    }
    # Create this study's data frame
    cott_df <- cott_carr_will_df[cott_carr_will_df$status_cott != "NA",]
    cott_df$end_mapped <- as.numeric(cott_df$end_mapped)
    # Account for if a gene is cut off (due to zoom in zoom out):
    cott_df$start_mapped <- ifelse(cott_df$start_mapped < xmin, xmin, cott_df$start_mapped)
    cott_df$end_mapped <- ifelse(cott_df$end_mapped > xmax, xmax, cott_df$end_mapped)
    # Create gene of interest data frame
    geneofinterest_df <- cott_df[cott_df$gene %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start_mapped),]
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study2_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- cott_df[cott_df$gene %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p2 <- ggplot(data = cott_df, aes(x=0, y=0)) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome") + ylab("") + 
      # Add points
      geom_rect(data = NULL, 
                   aes(xmin=cott_df[, "start_mapped"], ymin=0,
                       xmax=cott_df[, "end_mapped"], ymax=ymax-1),
                   fill=cott_df[, "color_cott"]) + 
      # Scaling and Legends
      # zoom in out
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(xmin, xmax)) +
      # scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p2 <- p2 + geom_rect(data = geneofinterest_df, 
                              aes(xmin=geneofinterest_df[, "start_mapped"], ymin=ymin,
                                  xmax=geneofinterest_df[, "end_mapped"], ymax=ymax-1),
                              fill='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p2 <- p2 + geom_rect(data = disease_geneofinterest_df, 
                           aes(xmin=disease_geneofinterest_df[, "start_mapped"], ymin=ymin,
                               xmax=disease_geneofinterest_df[, "end_mapped"], ymax=ymax-1),
                           fill='red')
    }
    p2
  })
  output$plot_study3 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    carrwill_df <- cott_carr_will_df[cott_carr_will_df$status_cott != "NA",]
    geneofinterest_df <- carrwill_df[carrwill_df$gene %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start_mapped),]
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study3_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- carrwill_df[carrwill_df$gene %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p3 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = carrwill_df, 
                   aes(x=carrwill_df[, "start_mapped"], y=0,
                       xend=carrwill_df[, "start_mapped"], yend=ymax-1),
                   color=carrwill_df[, "color_carrwill"]) + 
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p3 <- p3 + geom_segment(data = geneofinterest_df, 
                              aes(x=geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p3 <- p3 + geom_segment(data = disease_geneofinterest_df, 
                              aes(x=disease_geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=disease_geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    p3
  })
  output$plot_study4 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- kat_lin_df_lb[kat_lin_df_lb$gene %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start_mapped),]
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study4_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- kat_lin_df_lb[kat_lin_df_lb$gene %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p4 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = kat_lin_df_lb, 
                   aes(x=kat_lin_df_lb[, "start_mapped"], y=0,
                       xend=kat_lin_df_lb[, "start_mapped"], yend=ymax-1),
                   color=kat_lin_df_lb[, "color_lb"]) + 
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p4 <- p4 + geom_segment(data = geneofinterest_df, 
                              aes(x=geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p4 <- p4 + geom_segment(data = disease_geneofinterest_df, 
                              aes(x=disease_geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=disease_geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    p4
  })
  output$plot_study5 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- kat_lin_df_fb[kat_lin_df_fb$gene %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$start_mapped),]
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study5_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- kat_lin_df_fb[kat_lin_df_fb$gene %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$start),]
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p5 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = kat_lin_df_fb, 
                   aes(x=kat_lin_df_fb[, "start_mapped"], y=0,
                       xend=kat_lin_df_fb[, "start_mapped"], yend=ymax-1),
                   color=kat_lin_df_fb[, "color_fb"]) + 
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p5 <- p5 + geom_segment(data = geneofinterest_df, 
                              aes(x=geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p5 <- p5 + geom_segment(data = disease_geneofinterest_df, 
                              aes(x=disease_geneofinterest_df[, "start_mapped"], y=ymin,
                                  xend=disease_geneofinterest_df[, "start_mapped"], yend=ymax-1),
                              color='red')
    }
    p5
  })
  output$plot_study6 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Save threshold information for colorizing
    SV_threshold <- rv$SV_threshold
    VE_threshold <- rv$VE_threshold
    # Create gene of interest data frame
    geneofinterest_df <- TukGTExMod[TukGTExMod$`Gene name` %in% geneofinterest,]
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$`start_mapped`),]
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study6_genes){
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- TukGTExMod[TukGTExMod$`Gene name` %in% returned_genes_list,]
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$`start_mapped`),]
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p6 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome") + ylab("") + 
      # Add points
      geom_segment(data = TukGTExMod, 
                   aes(x=as.numeric(unlist(TukGTExMod[, "start_mapped"])), y=0,
                       xend=as.numeric(unlist(TukGTExMod[, "start_mapped"])), yend=ymax-1),
                   color=ifelse(as.numeric(unlist(TukGTExMod[, "perc_tissues_esc"])) <= SV_threshold, 
                                "lightsteelblue3",
                                ifelse(as.numeric(unlist(TukGTExMod[, "perc_tissues_esc"])) > SV_threshold & as.numeric(unlist(TukGTExMod[, "perc_tissues_esc"])) <= VE_threshold, 
                                       "turquoise3", "purple"))) + 
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p6 <- p6 + geom_segment(data = geneofinterest_df, 
                              aes(x=as.numeric(unlist(geneofinterest_df[, "start_mapped"])), y=ymin,
                                  xend=as.numeric(unlist(geneofinterest_df[, "start_mapped"])), yend=ymax-1),
                              color='red')
      
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p6 <- p6 + geom_segment(data = disease_geneofinterest_df, 
                              aes(x=as.numeric(unlist(disease_geneofinterest_df[, "start_mapped"])), y=ymin,
                                  xend=as.numeric(unlist(disease_geneofinterest_df[, "start_mapped"])), yend=ymax-1),
                              color='red')
    }
    p6
  })
  output$plot_study7 <- renderPlot({
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- cotton_mDNA[cotton_mDNA$GENE %in% geneofinterest,] # change here
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$POS),] # change here
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study7_genes){ # changed here
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- cotton_mDNA[cotton_mDNA$GENE %in% returned_genes_list,] # changed here
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$POS),] # changed here
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p7 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) +
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = cotton_mDNA, # changed here
                   aes(x=cotton_mDNA[, "POS"], y=0, # chnaged here
                       xend=cotton_mDNA[, "POS"], yend=ymax-1), # changed here
                   color=cotton_mDNA[, "COLOR"]) + # changed here
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p7 <- p7 + geom_segment(data = geneofinterest_df, # changed here
                              aes(x=geneofinterest_df[, "POS"], y=ymin, # changed here
                                  xend=geneofinterest_df[, "POS"], yend=ymax-1), # changed here
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p7 <- p7 + geom_segment(data = disease_geneofinterest_df, # changed here
                              aes(x=disease_geneofinterest_df[, "POS"], y=ymin, # changed here
                                  xend=disease_geneofinterest_df[, "POS"], yend=ymax-1), # changed here
                              color='red')
    }
    p7
  })
  output$plot_study8 <- renderPlot({ # changed here
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- balbrown_mCEMT[balbrown_mCEMT$GENE %in% geneofinterest,] # change here
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$START),] # change here
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study8_genes){ # changed here
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- balbrown_mCEMT[balbrown_mCEMT$GENE %in% returned_genes_list,] # changed here
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$START),] # changed here
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p8 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) + # changed here
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = balbrown_mCEMT, # changed here
                   aes(x=balbrown_mCEMT[, "START"], y=0, # chnaged here
                       xend=balbrown_mCEMT[, "START"], yend=ymax-1), # changed here
                   color=balbrown_mCEMT[, "COLOR"]) + # changed here
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p8 <- p8 + geom_segment(data = geneofinterest_df, # changed here
                              aes(x=geneofinterest_df[, "START"], y=ymin, # changed here
                                  xend=geneofinterest_df[, "START"], yend=ymax-1), # changed here
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p8 <- p8 + geom_segment(data = disease_geneofinterest_df, # changed here
                              aes(x=disease_geneofinterest_df[, "START"], y=ymin, # changed here
                                  xend=disease_geneofinterest_df[, "START"], yend=ymax-1), # changed here
                              color='red')
    }
    p8
  })
  output$plot_study9 <- renderPlot({ # changed here
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- balbrown_CREST[balbrown_CREST$GENE %in% geneofinterest,] # change here
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$START),] # change here
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study9_genes){ # changed here
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- balbrown_CREST[balbrown_CREST$GENE %in% returned_genes_list,] # changed here
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$START),] # changed here
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p9 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) + # changed here
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = balbrown_CREST, # changed here
                   aes(x=balbrown_CREST[, "START"], y=0, # chnaged here
                       xend=balbrown_CREST[, "START"], yend=ymax-1), # changed here
                   color=balbrown_CREST[, "COLOR"]) + # changed here
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p9 <- p9 + geom_segment(data = geneofinterest_df, # changed here
                              aes(x=geneofinterest_df[, "START"], y=ymin, # changed here
                                  xend=geneofinterest_df[, "START"], yend=ymax-1), # changed here
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p9 <- p9 + geom_segment(data = disease_geneofinterest_df, # changed here
                              aes(x=disease_geneofinterest_df[, "START"], y=ymin, # changed here
                                  xend=disease_geneofinterest_df[, "START"], yend=ymax-1), # changed here
                              color='red')
    }
    p9 # changed here
  })
  output$plot_study10 <- renderPlot({ # changed here
    # Save geneofinterest
    geneofinterest <- rv$geneofinterest1
    # Save disease of interest datapoints
    diseaseofinterest <- rv$diseaseofinterest1
    # Select either geneofinterest or diseaseofinterest depending on the search engine
    searchType <- rv$searchType
    # Create gene of interest data frame
    geneofinterest_df <- TukDEG[TukDEG$GENE %in% geneofinterest,] # change here
    geneofinterest_df <- geneofinterest_df[order(geneofinterest_df$START),] # change here (sometimes)
    # Create disease of interest data frame
    mapped_genes_gwas <- c()
    for(d in diseaseofinterest){
      mg_gwas <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
      if(!identical(mg_gwas, character(0))){
        ifelse(is.null(mapped_genes_gwas), mapped_genes_gwas <- mg_gwas, mapped_genes_gwas <- c(mapped_genes_gwas, mg_gwas))
      }
    }
    mapped_genes <- unique(mapped_genes_gwas)
    returned_genes_list <- c()
    returned_genes <- for(gene in study10_genes){ # changed here
      ifelse(TRUE %in% grepl(paste0("\\b",gene,"\\b"), mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
    }
    rv$returned_genes_list <- returned_genes_list
    disease_geneofinterest_df <- TukDEG[TukDEG$GENE %in% returned_genes_list,] # changed here
    disease_geneofinterest_df <- disease_geneofinterest_df[order(disease_geneofinterest_df$START),] # changed here (sometimes)
    # Get Range of plot
    ymin = 0
    ymax = 10
    # Get axis breaks
    x_breaks <- seq(0, max(x_expr_mod$start), 10000000)
    first_label <- x_breaks[1]
    rest_of_labels <- x_breaks[2:length(x_breaks)]
    x_labels <- c(paste(first_label, "bp"),
                  paste(formatC(rest_of_labels/(10^6), format = "f", big.mark = ",", digits = 0),"Mbp"))
    # Create theme
    mytheme <- theme(plot.title = element_text(family = "serif", face = "bold", size = (20), hjust = 0, vjust = -1),
                     legend.title = element_text(face = "bold", colour = "steelblue", family = "Helvetica", size = (15)),
                     legend.text = element_text(face = "bold", colour="steelblue4",family = "Helvetica", size = (12)),
                     legend.position = "none",
                     axis.title.y = element_text(family = "Helvetica", size = (14), colour = "steelblue4", face = "bold"),
                     axis.text.y = element_text(family = "Courier", colour = "steelblue4", size = (10), face = "bold", angle=0),
                     axis.title.x = element_blank(), # Removing X-title
                     axis.text.x = element_text(family = "Helvetica", colour = "steelblue4", size = (10),
                                                face = "bold", angle=0, hjust=0.5),
                     axis.ticks = element_blank(),
                     panel.background = element_rect(fill = "white"))
    # Create plot
    p10 <- ggplot(data = x_expr_mod, aes(x=start, y=-log10(p_value_mod))) + # changed here
      mytheme + ggtitle("X-Chromosome Escape Profile") +
      xlab("X-Chromosome Position") + ylab("") + 
      # Add points
      geom_segment(data = TukDEG, # changed here
                   aes(x=TukDEG[, "START"], y=0, # changed here
                       xend=TukDEG[, "START"], yend=ymax-1), # changed here
                   color=TukDEG[, "COLOR"]) + # changed here
      # Scaling and Legends
      scale_x_continuous(breaks=x_breaks, labels = x_labels, limits = c(plot1_xmin, plot1_xmax)) +
      scale_y_continuous(limits = c(ymin,ymax), breaks = c(ymin, ymax), labels= c("  ","  "))
    # Data points added by user reactive values: Gene of Interest
    if(nrow(geneofinterest_df) != 0){
      p10 <- p10 + geom_segment(data = geneofinterest_df, # changed here
                              aes(x=geneofinterest_df[, "START"], y=ymin, # changed here (sometimes)
                                  xend=geneofinterest_df[, "START"], yend=ymax-1), # changed here (sometimes)
                              color='red')
    }
    # Data points added by user reactive values: Disease of Interest
    if(nrow(disease_geneofinterest_df) != 0){
      p10 <- p10 + geom_segment(data = disease_geneofinterest_df, # changed here
                              aes(x=disease_geneofinterest_df[, "START"], y=ymin, # changed here (sometimes)
                                  xend=disease_geneofinterest_df[, "START"], yend=ymax-1), # changed here (sometimes)
                              color='red')
    }
    p10 # changed here
  })
  ## X chromosome "image"
  ## commented out for testing
  output$xchromosome <- renderCachedPlot({
    # Scaling zoom in out
    chrom_segments_mod <- chrom_segments
    chrom_segments_colored_mod <- chrom_segments_colored
    if (is.null(ranges$x)){
      xmin <- plot1_xmin
      xmax <- plot1_xmax
      chrom_segments_mod <- chrom_segments
      chrom_segments_colored_mod <- chrom_segments_colored
    } else {
      xmin <- ranges$x[1]
      xmax <- ranges$x[2]
      # Update the chrom segment objects
      chrom_segments_colored_mod <- chrom_segments_colored # for now
      for ( i in 3:39 ){
        # if start position of chrom segment is less than xmin
        if ( chrom_segments[[paste0("seg",i)]]$mapping$x < xmin ){
          chrom_segments_mod[[paste0("seg",i)]]$mapping$x <- xmin
        } else {
          chrom_segments_mod[[paste0("seg",i)]]$mapping$x <- chrom_segments[[paste0("seg",i)]]$mapping$x
        }
        # if end position of chrom segment is greater than xmax
        if ( chrom_segments[[paste0("seg",i)]]$mapping$xend > xmax ){
          chrom_segments_mod[[paste0("seg",i)]]$mapping$xend <- xmax
        } else {
          chrom_segments_mod[[paste0("seg",i)]]$mapping$xend <- chrom_segments[[paste0("seg",i)]]$mapping$xend
        }
      }
    } # come back here
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
      xlab("X Chromosome") + ylab(" ") +
      # Scaling and Legends
      scale_x_continuous(breaks=x_region_breaks, labels = x_region_labels, limits=c(xmin, xmax)) + # zoom in out
      scale_y_continuous(breaks = c(0,1), labels= c("  ","  "), limits = c(-2,0)) +
      # Add annotions
      annotate("text", x=0, y=-0.5, label="PAR1", size=4, color = "steelblue", hjust=0) +
      annotate("text", x=max(x_expr$start), y=-0.5, label="PAR2", size=4, color = "steelblue", hjust=.9) +
      annotate("text", x=mean(centre_boundaries[1],centre_boundaries[2])+3e6, y=-0.5,
               label="CENTROMERE", size=4, color = "red", alpha = 0.5) +
      # Add chromosome map (yes the ordering is important)
      chrom_segments_colored_mod[c('start','end')] +
      chrom_segments_mod +
      chrom_segments_colored_mod[c('par1','par2','centre')]
    # ^these objects contains geom_segment() layers for each band.
    #  Created in utilities/format_plot_aesthetics.R
    xchromosome_plot
  }, cacheKeyExpr = { c(input$geneofinterest1, ranges$x) }
  )
  ############
  ### TAB 3
  ############
  ## Violin Plots - Gene of Interest
  output$individual_gene_tau_plot <- renderPlot({
    validate(
      need(input$geneofinterest3 !="", "Please input a gene of interest")
    )
    geneofinterest <- rv$geneofinterest3
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
}

#This lets script know its a shiny app
shinyApp(server = server, ui = ui)
