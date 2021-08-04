# Set variables
TABLE="TukSupTables/Suppl.Table.13.csv"

OUTPUT="tuk_DEG_chrX_positions_hg19.txt"

# Main code starts here
cat $TABLE | awk 'BEGIN { FS = "," } ; {print "chr"$3":"$4"-"$5}' | awk 'NR>1{print}' | sed '/^chr:-/d' > $OUTPUT
