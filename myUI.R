# myUI: Where each tab layout is designed and objects are enabled/disabled
source("myUI_Tab1.R", local = TRUE)
source("myUI_Tab2.R", local = TRUE)
myUI <- fluidPage(title = "XCI Data",
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