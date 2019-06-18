faLooperTurbo <- function(params,objfn,fixptfn,
                       localHasConvergedParameter,
                       localHasConvergedLogLike,
                       maxIterations,
                       stopType,
                       methods=c("em", "squarem", "pem")) {
  s3faLooperTurbo(params,objfn,fixptfn,
                  localHasConvergedParameter,
                  localHasConvergedLogLike,
                  maxIterations,
                  stopType,
                  methods)
}