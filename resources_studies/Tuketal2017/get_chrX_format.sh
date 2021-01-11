# Set variables
TABLE=/Users/karinemoussa/Documents/DrLiu_Lab/Shiny_Apps/xci-app-1/resources_studies/Tuketal2017/Suppl.Table.1.csv

OUTPUT="cott_car_will_chrX_positions_hg19.txt"

# Main code starts here
cat $TABLE | awk 'BEGIN { FS = "," } ; {print "chr"$3":"$4"-"$5}' | awk 'NR>2{print}' | sed '/^chr:-/d' > $OUTPUT
