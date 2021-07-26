TABLE="ddu564supp_table5_mod.csv"

OUTPUT="cotton_mDNA_chrX_pos.txt"

cat $TABLE | awk 'BEGIN { FS = "," } ; NR > 3 {print "chrX:"$1}' > $OUTPUT 
