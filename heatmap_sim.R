heatmap_sim <- function(
    nrows,
    nbins,
    p_geom = 0.1,
    se_norm = 10,
    jitter_factor = 3,
    jitter_amount = 5
){
    require(ComplexHeatmap)
    
    # init df
    sim.df <- data.frame(matrix(NA, nrow = nrows, ncol = nbins))
    
    # simulating data
    for (i in 1:nrow(sim.df)) {
        m = rgeom(1, p_geom)
        left = sort(rnorm((nbins/2), m, se_norm), decreasing = FALSE)
        right = sort(rnorm((nbins/2), m, se_norm), decreasing = TRUE)
        left[left < 0] = 0
        right[right < 0] = 0
        sim.df[i,] <- 
            c(
                jitter(left, jitter_factor, jitter_amount), 
                jitter(right, jitter_factor, jitter_amount)
            )
    }
    
    # Sorting by row sums
    sim.df <- sim.df[order(rowSums(sim.df), decreasing = TRUE),]
    
    # Plotting Heatmap
    hm <- Heatmap(
        as.matrix(sim.df),
        cluster_columns = F,
        cluster_rows = F,
        show_row_names = FALSE,
        show_column_names = FALSE
    )
    return(hm)
}