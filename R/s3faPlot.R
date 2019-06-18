#' Plot the resulted S3FA hyperplane
#'
#' Works only if input is 1- or 2-dimensional and output is 1-dimensional.
#'
#' @param X_t a matrix representing the input points to be plotted (if addPoints is TRUE); usually, the supervised training input
#' @param Z_t a matrix representing the output = values of function in input points; usually, the supervised training output
#' @param p parameters of a fitted S3FA
#' @param color color of the hyperplane given by S3FA
#' @param add add this plot to an already existing plot?
#' @param addPoints whether to plot the points X_t, Z_t
#' @param pointsColor color of points
#' @param addLr fit and add a linear regression model on plot?
#' @param lrColor color of linear regression hyperplane
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#' @param checkPositiveDefinite whether to check the covariance matrices are valid; it takes more time to execute
#'
#' @export
#' @examples
#' params <- s3faFit(X_t_supervised = house[1:10,2,drop=FALSE],
#'                   Z_t_supervised = house[1:10,1,drop=FALSE],
#'                   X_t_unsupervised = house[11:20,2,drop=FALSE],
#'                   params=NULL,
#'                   type="fa",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE,
#'                   checkPositiveDefinite=FALSE,
#'                   epsilon=1e-10,
#'                   maxIterations=100,
#'                   stopType="objfn",
#'                   turboEmMethods=NULL)
#' s3faPlot(X_t=house[1:10,2,drop=FALSE],
#'          Z_t=house[1:10,1,drop=FALSE],
#'          p=params,
#'          color="red",
#'          add=FALSE,
#'          addPoints=TRUE,
#'          pointsColor="black",
#'          addLr=TRUE,
#'          lrColor="blue",
#'          checkArgs=TRUE,
#'          checkPositiveDefinite=FALSE)
#'
#' params <- s3faFit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                   Z_t_supervised = house[1:10,1,drop=FALSE],
#'                   X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                   params=NULL,
#'                   type="fa",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE,
#'                   checkPositiveDefinite=FALSE,
#'                   epsilon=1e-10,
#'                   maxIterations=100,
#'                   stopType="objfn",
#'                   turboEmMethods=NULL)
#' s3faPlot(X_t=house[1:10,2:3,drop=FALSE],
#'          Z_t=house[1:10,1,drop=FALSE],
#'          p=params,
#'          color="red",
#'          add=FALSE,
#'          addPoints=TRUE,
#'          pointsColor="black",
#'          addLr=TRUE,
#'          lrColor="blue",
#'          checkArgs=TRUE,
#'          checkPositiveDefinite=FALSE)
#'
s3faPlot <- function(X_t,Z_t,p,
                     color="red",add=FALSE,
                     addPoints=FALSE,pointsColor="black",
                     addLr=FALSE,lrColor="blue",
                     checkArgs=TRUE,
                     checkPositiveDefinite=FALSE) {
  s2faPlot(X_t,Z_t,p,
                       color=color,add=add,
                       addPoints=addPoints,pointsColor=pointsColor,
                       addLr=addLr,lrColor=lrColor,
                       checkArgs=checkArgs,
                       checkPositiveDefinite=checkPositiveDefinite)
}
