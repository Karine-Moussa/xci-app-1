# Set variables
TABLE="Suppl.Table.1.csv"

OUTPUT="cott_car_will_chrX_positions_hg19.txt"

# Main code starts here
cat $TABLE | awk 'BEGIN { FS = "," } ; {print "chr"$3":"$4"-"$5}' | awk 'NR>2{print}' | sed '/^chr:-/d' > $OUTPUT
