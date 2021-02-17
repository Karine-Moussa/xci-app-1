TABLE=table_s2.csv

cat $TABLE | awk 'BEGIN { FS = "," } ; {print $1":"$2}' | grep "^chrX" > snp_positions.txt
