# Create template rds file
create_template <- function(template_num){
    if(template_num == 1){
        df <- data.frame(matrix(nrow=0, ncol=4, data = ""))
        x <- c("GENE", "STATE", "START", "END")
        colnames(df) <- x
    }
    if(template_num == 2){
        df <- data.frame(matrix(nrow=0, ncol=5, data = ""))
        x <- c("GENE", "STATE", "SAMPLE", "START", "END")
        colnames(df) <- x
    }
    saveRDS(df, "rds/usr_temp.rds")
}
