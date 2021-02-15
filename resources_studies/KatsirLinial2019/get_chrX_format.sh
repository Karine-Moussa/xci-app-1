# Set variables
TABLE="/Users/karinemoussa/Documents/DrLiu_Lab/Shiny_Apps/xci-app-1/resources_studies/KatsirLinial2019/table_s3_mod.csv"

OUTPUT="katsir_linial_chrX_positions_orig.txt"

# Main code starts here
cat $TABLE | awk 'BEGIN { FS = "," } ; {print $2":"$3"-"$4}' | grep -v "^chrom" > $OUTPUT
