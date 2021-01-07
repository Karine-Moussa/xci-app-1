# Create global variables
STATUS <- as.factor(x_expr$status)
GENE <- as.factor(x_expr$GENE)

# List of samples
SAMPLE_NAMES <- c(unique(x_expr[,"sample"]))
LIST_OF_GENES <- c(unique(x_expr[,"GENE"]))

# P-value significance
P_SIG <- 0.05/(length(unique(GENE)))
P_SIG <- 1.87e-4
P_MIN <- 2.845412e-304