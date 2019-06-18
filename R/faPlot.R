#' Plot the resulted FA hyperplane
#'
#' Works only if input is 1- or 2-dimensional and output is 1-dimensional.
#'
#' @param X_t a matrix representing the input points to be plotted (if addPoints is TRUE); usually, the training input
#' @param Z_t a matrix representing the output = values of function in input points; usually, the training output
#' @param p parameters of a fitted FA
#' @param color color of the hyperplane given by FA
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
#' params <- faFit(X_t_unsupervised = house[,2:3,drop=FALSE],
#'                 paramsNULL_nDimZ = 1,
#'                 params=NULL,
#'                 type="fa",
#'                 lambdaRidge=0,
#'                 checkArgs=FALSE,
#'                 checkPositiveDefinite=FALSE,
#'                 epsilon=1e-10,
#'                 maxIterations=100,
#'                 stopType="parameter",
#'                 turboEmMethods=c("em","pem"))
#' faPlot(X_t=house[,2:3,drop=FALSE],
#'        Z_t=house[,1,drop=FALSE],
#'        p=params,
#'        color="red",
#'        add=FALSE,
#'        addPoints=TRUE,
#'        pointsColor="black",
#'        addLr=TRUE,
#'        lrColor="blue",
#'        checkArgs=TRUE,
#'        checkPositiveDefinite=FALSE)
#'
#' params <- faFit(X_t_unsupervised = house[,2,drop=FALSE],
#'                 paramsNULL_nDimZ = 1,
#'                 params=NULL,
#'                 type="fa",
#'                 lambdaRidge=0,
#'                 checkArgs=FALSE,
#'                 checkPositiveDefinite=FALSE,
#'                 epsilon=1e-10,
#'                 maxIterations=100,
#'                 stopType="parameter",
#'                 turboEmMethods=c("em","pem"))
#' faPlot(X_t=house[,2,drop=FALSE],
#'        Z_t=house[,1,drop=FALSE],
#'        p=params,
#'        color="red",
#'        add=FALSE,
#'        addPoints=TRUE,
#'        pointsColor="black",
#'        addLr=TRUE,
#'        lrColor="blue",
#'        checkArgs=TRUE,
#'        checkPositiveDefinite=FALSE)
faPlot <- function(X_t,Z_t,p,
                     color="red",add=FALSE,
                     addPoints=FALSE,pointsColor="black",
                     addLr=FALSE,lrColor="blue",
                     checkArgs=TRUE,
                     checkPositiveDefinite=FALSE) {
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  if(checkArgs) {
    faPlotCheckArgs(X_t,Z_t,p,add,addPoints,addLr,
                      checkPositiveDefinite)
  }

  if(!(p$nDimX %in% 1:2)) {
    stop("X_t must have 1 or 2 columns")
  }
  if(p$nDimZ != 1) {
    stop("p$nDimZ must be 1")
  }

  if(p$nDimX == 1) {
    if(addPoints == TRUE) {
      plot(X_t,Z_t,
           xlab = "x",
           ylab = "y",
           col=pointsColor)
      add <- TRUE
    }
    x <- seq(min(X_t[,1],na.rm = T),max(X_t[,1],na.rm = T),length.out = 100)
    y <- faPredict(p,matrix(x))
    if(add) {
      points(x,y$values,col=color,type="l")
    } else {
      plot(x,y$values,
           col=color,
           xlab = "x",
           ylab = "y",
           type="l")
    }
    if(addLr) {
      Z_t <- as.data.frame(Z_t)
      X_t <- as.data.frame(X_t)
      colnames(Z_t) <- "V2"
      colnames(X_t) <- "V1"
      lrModel <- getLrModel(Z_t,X_t)
      testInput <- data.frame(x)
      colnames(testInput) <- colnames(X_t)
      predicted <- predict(lrModel,testInput)
      points(x,predicted,col=lrColor,type="l")
    }
  } else {
    if(addPoints == TRUE) {
      rgl::plot3d(X_t[,1],X_t[,2],Z_t,
             xlab = "x",
             ylab = "y",
             zlab = "z",
             col=pointsColor)
      add <- TRUE
    }
    x1 <- seq(min(X_t[,1],na.rm = T),max(X_t[,1],na.rm = T),length.out = 100)
    x2 <- seq(min(X_t[,2],na.rm = T),max(X_t[,2],na.rm = T),length.out = 100)
    testInput <- expand.grid(x1,x2)
    y <- faPredict(p,as.matrix(testInput))
    if(add) {
      rgl::points3d(testInput[[1]],testInput[[2]],y$values,col=color)
    } else {
      rgl::plot3d(testInput[[1]],testInput[[2]],y$values,
             col=color,
             xlab = "x",
             ylab = "y",
             zlab = "z")
    }
    if(addLr) {
      Z_t <- as.data.frame(Z_t)
      X_t <- as.data.frame(X_t)
      colnames(Z_t) <- "V3"
      colnames(X_t) <- c("V1","V2")
      lrModel <- getLrModel(Z_t,X_t)
      colnames(testInput) <- colnames(X_t)
      predicted <- predict(lrModel,testInput)
      rgl::points3d(testInput[[1]],testInput[[2]],predicted,col=lrColor)
    }
  }
}
