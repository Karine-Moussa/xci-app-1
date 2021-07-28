# This script takes in Balaton + Brown 2021 supplementary 
# table 4 and formats it for use in the application.
# Output is an rds file: balbrown_mod.rds
library(readxl)
setwd("/Users/karinemoussa/Documents/DrLiu_Lab/Shiny_Apps/xci-app-1")
file_path <- "resources_studies/BalBrown2021/supp/S4_mod.xlsx"

balbrown_xl <- read_xlsx(file_path)

options_1 <- as.character(unique(unlist(balbrown_xl[,5:10])))
options_1 <- rbind(options_1,
                   map = c("escape", "NA",
                           "inactive", "variable",
                           "variable", "escape",
                           "NA", "escape, variable",
                           "inactive, variable", "escape, inactive",
                           "escape, inactive", "variable, escape",
                           "variable, inactive", "inactive, escape",
                           "inactive, variable", "variable", 
                           "inactive", "escape", 
                           "inactive, variable", "variable",
                           "uncalled", "variable")
)

# Update the names of all escape calls
balbrown_mod <- balbrown_xl
for (row_num in 1:nrow(balbrown_xl)){
    for (col_num in 5:10){
        new_name <- options_1[2,][options_1[1,] %in% balbrown_xl[row_num, col_num]]
        balbrown_mod[row_num, col_num] <- new_name
    }
}

saveRDS(balbrown_mod, "resources_studies/BalBrown2021/balbrown_mod.rds")