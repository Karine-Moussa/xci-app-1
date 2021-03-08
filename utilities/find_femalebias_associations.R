# This script returns a list of all of the traits/diseases that 
# are female biased. 
##################################################################
# Create trait object for each trait in GWAS trait list that has
# an equivalent UK BioBank name
fbias_traits <- list()
for(i in 1:nrow(LIST_OF_TRAITS_GWAS)){
    trait <- LIST_OF_TRAITS_GWAS$GWAS_NAME[i]
    assign("trait_stats", create_single_trait_stats(trait))
    bias <- trait_stats$ukbio_stats$bias
    if (bias == "Female" | bias == "Female specific"){
        fbias_traits <- list.append(fbias_traits, trait)
    }
}
fbias_traits <- unlist(fbias_traits)
saveRDS(fbias_traits, "data_intermediate/fbias_traits.RData")
