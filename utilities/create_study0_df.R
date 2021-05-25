# Create blank study0_df
column_names = c("gene", "start", "tiss_samp", "state")
study0_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(study0_df) <- column_names
saveRDS(study0_df, "rds/study0_df.rds")