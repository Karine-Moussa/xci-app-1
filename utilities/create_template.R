# Create template rds file
create_template <- function(template_num, includes_start){
    if(template_num == 1 & includes_start == FALSE){
        df <- data.frame(matrix(nrow=0, ncol=2, data = ""))
        x <- c("gene", "samp_state")
        colnames(df) <- x
    }
    if(template_num == 1 & includes_start == TRUE){
        df <- data.frame(matrix(nrow=0, ncol=3, data = ""))
        x <- c("gene", "samp_state", "start")
        colnames(df) <- x
    }
    if(template_num == 2 & includes_start == FALSE){
        df <- data.frame(matrix(nrow=0, ncol=3, data = ""))
        x <- c("gene", "samp_state", "sample")
        colnames(df) <- x 
    }
    if(template_num == 2 & includes_start == TRUE){
        df <- data.frame(matrix(nrow=0, ncol=4, data = ""))
        x <- c("gene", "samp_state", "sample", "start")
        colnames(df) <- x
    }
    saveRDS(df, "rds/usr_temp.rds")
}
