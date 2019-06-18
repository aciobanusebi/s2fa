checkMatrix <- function(X_t) {
  # checking "matrix" %in% ... also handles the NA/NULL case
  if(!("matrix" %in% class(X_t))) {
    stop("An argument must be a matrix and it is not")
  }
  if(any(is.na(X_t))) {
    stop("An argument must not contain NAs and it does")
  }
}