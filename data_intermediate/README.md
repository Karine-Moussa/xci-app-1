### /data_intermediate
Files that are created as outputs of manipulating /data_sources.
These files are used as inputs for further processing or visualizing.

- **gwas_assoc_v1_02_xonly.csv**: GWAS associations from [GWAS Catalog v1.02](https://www.ebi.ac.uk/gwas/docs/file-downloads)\
The applications returns genes from the "MAPPED GENE" column of GWAS association data.
- **additional findings**: prototype for adding disease/traits that are not in GWAS catalogue. 
- **fbias_traits**: list of female-biased traits from the UK Biobank. 
- **gene_list**: list of genes in the GEUVADIS data set.
- **gwas_ukbio_mapping_xchrom.rds**: RDS version of UK Biobank to GWAS conversions from\
[Mapping UK Biobank to the Experimental Factor Ontology (EFO)](https://github.com/EBISPOT/EFO-UKB-mappings)
- **xchrom_map_colored**: used to create bank colors for the  X-chromosome graphic.\
Original file from (NCBI/ideogram_10090_GCF_000000045.1_NA_V2 )[https://ftp.ncbi.nlm.nih.gov/pub/gdp/]