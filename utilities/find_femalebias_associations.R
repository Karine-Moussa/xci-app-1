# This script returns a list of all of the traits/diseases that 
# are female biased. 
# Then it returns the list of all X-genes associated with these
# traits/diseases. 
##################################################################
# Create trait object for each trait in GWAS trait list that has
# an equivalent UK BioBank name
trait <- ""
fbias_traits <- list()
count = 0 # for testing
for(i in 1:nrow(LIST_OF_TRAITS_GWAS)){
    if (!is.na(LIST_OF_TRAITS_GWAS$UKBIO_NAME[i])){
        trait <- LIST_OF_TRAITS_GWAS$GWAS_NAME[i]
    }
    if (trait != ""){
        assign(paste0("trait_stats"), create_single_trait_stats(trait))
        bias <- trait_stats$bias
        if(bias == "Female" | bias == "Female specific"){
            count = count + 1 # for testing
            fbias_traits <- list.append(fbias_traits, trait)
        }
    }
}
fbias_traits <- unlist(fbias_traits)
saveRDS(fbias_traits, "data_intermediate/fbias_traits.RData")

# Part 2 here
