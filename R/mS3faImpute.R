#' Impute missing data via MS3FA
#'
#' @param params parameters of a fitted MS3FA
#' @param testInput test input data; can contain NAs; error if a full row (input+output) is NA
#' @param testOutput train output data; can contain NAs; error if a full row (input+output) is NA
#'
#' @return a list with two elements: testInput with NAs imputed, testOutput with NAs imputed
#'
#' @export
#' @examples
#' houseCopy <- house
#' houseCopy[1:10,2:3] <- NA
#' houseCopy[11:20,1] <- NA
#' params <- mS3faFit(trainInput=houseCopy[,2:3,drop=FALSE],
#'                    trainOutput=houseCopy[,1,drop=FALSE],
#'                    params=NULL,
#'                    type="ppca",
#'                    lambdaRidge=0,
#'                    epsilon=1e-10,
#'                    maxIterations=100,
#'                    stopType="objfn",
#'                    turboEmMethods=NULL)
#' result <- mS3faImpute(params=params,
#'                       testInput=houseCopy[,2:3,drop=FALSE],
#'                       testOutput=houseCopy[,1,drop=FALSE])
#' result
mS3faImpute <- function(params,testInput,testOutput) {
  nDimX <- ncol(testInput)
  nDimZ <- ncol(testOutput)
  xIndexes <- 1:nDimX
  zIndexes <- nDimX + (1:nDimZ)

  nTest <- nrow(testInput)

  v1 <- params$mu + params$lambda %*% params$mu_z
  v2 <- params$mu_z
  v <- rbind(v1,v2)

  M1 <- params$lambda %*% params$Sigma_z %*% t(params$lambda) + params$psi
  M2 <- params$lambda %*% params$Sigma_z
  M3 <- t(M2)
  M4 <- params$Sigma_z
  M <- rbind(cbind(M1,M2),cbind(M3,M4))

  data <- cbind(testInput,testOutput)

  values <- list()
  Sigmas <- list()
  for(i in 1:nTest) {
    s_i <- convertToColumnVector(data[i,,drop=FALSE])
    E_s_i <- s_i
    isNaIndexes <- which(is.na(s_i))

    if(length(isNaIndexes)) {
      distr <- getConditionalDistribution(s_i,v,M,isNaIndexes)
      s_i[isNaIndexes] <- distr$mu
    }

    testInput[i,] <- s_i[xIndexes]
    testOutput[i,] <- s_i[zIndexes]
  }

  list(
    testInput=testInput,
    testOutput=testOutput
  )
}
