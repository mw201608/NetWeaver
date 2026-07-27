.ACAT_internal <- function(Pvals, weights = NULL, is.check = TRUE) {
  #Modified from https://github.com/yaowuliu/ACAT
  Pvals <- as.matrix(Pvals)
  n_row <- nrow(Pvals)
  n_col <- ncol(Pvals)
  
  if (is.check) {
    if (anyNA(Pvals)) {
      stop("Cannot have NAs in the p-values!")
    }
    if (any(Pvals < 0 | Pvals > 1)) {
      stop("P-values must be between 0 and 1!")
    }
    
    is_zero <- colSums(Pvals == 0) > 0
    is_one  <- colSums(Pvals == 1) > 0
    
    if (any(is_zero & is_one)) {
      stop("Cannot have both 0 and 1 p-values in the same column!")
    }
    if (any(is_zero)) {
      warning("There are p-values that are exactly 0!")
    }
    if (any(is_one)) {
      warning("There are p-values that are exactly 1!")
    }
  }
  
  if (is.null(weights)) {
    # Default equal weights: normalize by row count per column
    w_mat <- NULL
  } else {
    weights <- as.matrix(weights)
    
    if (!identical(dim(weights), dim(Pvals))) {
      stop("The dimensions of weights and Pvals must be the same!")
    }
    if (is.check && any(weights < 0)) {
      stop("All the weights must be nonnegative!")
    }
    
    w_sum <- colSums(weights)
    if (any(w_sum <= 0)) {
      stop("At least one weight should be positive in each column!")
    }
    
    # Vectorized column normalization (replaces the R loop)
    w_mat <- sweep(weights, 2, w_sum, FUN = "/")
  }
  
  # Calculate Cauchy statistics using linear indexing
  is_small <- Pvals < 1e-15
  stat_mat <- matrix(0, nrow = n_row, ncol = n_col)
  
  # Approximation for ultra-small p-values: tan((0.5 - p)*pi) = 1 / (p * pi)
  if (any(is_small)) {
    stat_mat[is_small] <- 1 / (Pvals[is_small] * pi)
  }
  
  not_small <- !is_small
  if (any(not_small)) {
    stat_mat[not_small] <- tan((0.5 - Pvals[not_small]) * pi)
  }
  
  # Apply weights & summarize
  if (is.null(w_mat)) {
    cct_stat <- colMeans(stat_mat)
  } else {
    cct_stat <- colSums(stat_mat * w_mat)
  }
  
  return(pcauchy(cct_stat, lower.tail = FALSE))
}
ACAT.2 <- function(Pvals, na.action = c('na.omit', 'na.to1'), tol = 1.0E-300){
	#Customized ACAT function call with special handling of 1s and 0s.
  #Pvals, a vector of P values
	#tol is an arbitrary value close to the machine precision limit of smallest non-zeiro numeric value. Zero(s) in x will be replaced by the smaller of tol or the minimum non-zero value in x.
	na.action <- match.arg(na.action)
  x = Pvals
	if(any(is.na(x))){
		if(na.action == 'na.omit'){
			x <- x[!is.na(x)]
		}else{
			x[is.na(x)] <- 1
		}
		if(length(x) == 0) return(NA)
	}
    if(all(x == 1)){
        return(1)
    }
	if(any(x == 0)){
		a <- x[x > 0]
		if(length(a) > 1) a <- min(a, na.rm = TRUE)
		if(length(a) == 0 || a > tol){
			x[x == 0] <- tol
		}else{
			x[x == 0] <- a
		}
	}
    if(any(x==1)){
        x[x==1] <- (max(x[x < 1]) / 2) + 0.5
    }
	.ACAT_internal(x) #devtools::install_github("yaowuliu/ACAT")
}
PMatrixACAT <- function(Pmatrix, na.action = c('na.omit', 'na.to1')){
	na.action <- match.arg(na.action)
	cat('na.action:', na.action, '\n')
  Pvalue = apply(as.matrix(Pmatrix), 1, ACAT.2, na.action = na.action)
	Pvalue
}
