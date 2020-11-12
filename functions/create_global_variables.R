# Create global variables
STATUS <- as.factor(x_expr$status)
GENE <- as.factor(x_expr$GENE)

# List of samples
SAMPLE_NAMES <- c(unique(x_expr[,"sample"]))

# P-value significance
P_SIG <- 0.05/(length(unique(GENE)))

MIN_P <- 2.845412e-304