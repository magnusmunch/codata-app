# selected variables lists
selvar <- function(fit, features, response) {
  nm <- names(fit)
  rnms <- rownames(features)
  if(!is.null(response$unpen)) {
    rnms <- c(colnames(response$unpen), rnms)
  }
  l <- list()
  if("enet" %in% nm) {
    l <- c(l, enet=list(apply(
      coef(fit$enet, s="lambda.min")[-1, , drop=FALSE], 2, function(b) {
        rnms[as.numeric(b)!=0]})))
  }
  if("gren" %in% nm) {
    l <- c(l, gren=list(apply(
      coef(fit$gren, type="groupreg")[-1, , drop=FALSE], 2, function(b) {
        rnms[as.numeric(b)!=0]})))
  }
  if("ecpc" %in% nm) {
    l <- c(l, ecpc=list(apply(
      fit$ecpc$betaPost, 2, function(b) {
        rnms[as.numeric(b)!=0]})))
  }
  l[sapply(l, length)==0] <- "no variables selected"
  return(l)
}
