create_association_df <- function(gene){
association_df <- data.frame(Date = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DATE.ADDED.TO.CATALOG"],
                     "Mapped.Gene" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"MAPPED_GENE"],
                     "Disease/Trait" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DISEASE.TRAIT"],
                     Link = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"LINK"])
return(association_df)
}