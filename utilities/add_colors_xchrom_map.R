######################################
### Adding colors to X-bands:
### The colors will be added to the xchrom_map
### and stored in the data_intermediate folder
# Import x-map
xchrom_map <- read.delim("data_sources/xchrom_map_2012-09-27", sep = "\t")
# Add a column for color based on density
xcolors <- c("gray80","gray60","gray40","gray20","gray0")
BandColor <- rep(xcolors[1],nrow(xchrom_map))
BandColor[xchrom_map$density == "NA"] = xcolors[1]
BandColor[xchrom_map$density == "25"] = xcolors[2]
BandColor[xchrom_map$density == "50"] = xcolors[3]
BandColor[xchrom_map$density == "75"] = xcolors[4]
BandColor[xchrom_map$density == "100"] = xcolors[5]
xchrom_map_colored <- xchrom_map
xchrom_map_colored[["BandColor"]] <- BandColor
rm(xchrom_map)

# Create output tsv
write.table(xchrom_map_colored, file="data_intermediate/xchrom_map_colored",sep='\t')