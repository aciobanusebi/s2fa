faPredict_ <- function(p,X_t_test,
                         checkArgs,checkPositiveDefinite) {
  result <- faPredict(p,X_t_test,checkArgs,checkPositiveDefinite)
  list(
    E_z=result$values,
    E_zz=t(result$values) %*% result$values + nrow(X_t_test)*result$Sigma_t
  )
}
