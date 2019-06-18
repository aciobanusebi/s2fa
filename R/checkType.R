checkType <- function(type) {
  type <- tolower(type)
  if(!(type %in% c("fa","ppca","un","unc","unconstrained"))) {
    stop("type must be: 'unc','fa', or 'ppca'")
  }
  type
}