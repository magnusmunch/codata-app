# ROC plots
rocplots <- function(cv, response) {
  
  # the predictions
  pred <- cv$pred
  
  # the fitted models
  nm <- colnames(pred)
  
  # settings
  col <- c(1:length(nm))
  
  # roc curves
  # opar <- par(no.readonly=TRUE)
  # layout(matrix(1:length(nm), nrow=length(nm)))
  for(m in 1:length(nm)) {
    plot(pROC::roc(as.numeric(response$y), pred[, m], quiet=TRUE), 
         add=m!=1, lty=m, col=col[m]) 
  }
  legend("bottomright", lty=1:length(nm), col=col, legend=nm)
  # par(opar)
}

# load("/Users/magnusmunch/Downloads/fit-2021-03-18-15_13_31.Rdata")
# source("app/R/data.R")
# # codatatype <- c("categorical", "continuous", "categorical")
# codata <- codataFile(list(datapath="data/codata_cat_cont_hier.txt"))
# codatatype <- list(codatatypeV1="categorical", codatatypeV3="categorical",
#                    codatatypeV2="continuous")
# penalty weights plot
multplots <- function(fit, codatatype, codata) {
  
  # the fitted models
  nm <- names(fit)[!(names(fit) %in% "enet")]
  
  # reorder codatatypes to match codata
  codatatype <- codatatype[
    match(sapply(strsplit(names(codatatype), "codatatype"), "[", 2),
          colnames(codata))]
  
  # add ecpc and gren multipliers
  mults.ecpc <- mults.gren <- NULL
  if("ecpc" %in% nm) {
    mults.ecpc <- split(fit$ecpc$gamma, 
                        sapply(strsplit(names(fit$ecpc$gamma), ".", 
                                        fixed=TRUE), "[", 1))
    if(any(codatatype=="continuous")) {
      mults.ecpc[codatatype=="continuous"] <- 
        lapply(which(codatatype=="continuous"), function(d) {
          grp <- fit$ecpc$groupsets[[d]]
          Z <- as.matrix(sapply(grp, function(g) {
            replace(rep(0, nrow(codata)), g, rep(1, length(g)))}))
          Z <- Z/rowSums(Z)
          as.numeric(Z %*% mults.ecpc[[d]])})
    }
  }
  if("gren" %in% nm) {
    mults.gren <- fit$gren$lambdag
    if(any(codatatype=="continuous")) {
      mults.gren[codatatype=="continuous"] <- 
        lapply(which(codatatype=="continuous"), function(d) {
          # fit$gren$lambdag[[d]][fit$gren$codata[, d]]
          fit$gren$lambdag[[d]][fit$gren$codata[[d]]]
          })
    }
  }
  
  # create multiplier object
  mults <- lapply(1:ncol(codata), function(d) {
    if((!is.null(mults.gren[[d]]) & !is.null(mults.gren[[d]])) &
       (length(mults.gren[[d]])!=length(mults.ecpc[[d]]))) {
      list(gren=mults.gren[[d]], ecpc=mults.ecpc[[d]])  
    } else {
      cbind(gren=mults.gren[[d]], ecpc=mults.ecpc[[d]])
    }})
    
  
  # multiplier plots
  mat <- matrix(nrow=length(mults), ncol=1 + 
                  (any(codatatype=="continuous") | any(sapply(mults, is.list))))
  count <- 1
  for(d in 1:nrow(mat)) {
    if(codatatype[d]=="categorical" & !is.list(mults[[d]])) {
      mat[d, ] <- rep(count, ncol(mat))
      count <- count + 1
    } else {
      mat[d, ] <- c(count:(count + ncol(mat) - 1))
      count <- count + ncol(mat)
    }
  }
  opar <- par(no.readonly=TRUE)
  layout(mat)
  for(d in 1:length(mults)) {
    if(codatatype[d]=="categorical" & !is.list(mults[[d]])) {
      barplot(mults[[d]] ~ c(1:nrow(mults[[d]])), beside=TRUE,
              xlab=colnames(codata)[d], ylab="Penalty weight", 
              legend.text=TRUE)
      abline(h=1, lty=2)
    } else if(codatatype[d]=="categorical") {
      col <- gray.colors(length(mults[[d]]))
      for(m in 1:length(mults[[d]])) {
        barplot(mults[[d]][[m]] ~ c(1:length(mults[[d]][[m]])), beside=TRUE,
                xlab=colnames(codata)[d], ylab="Penalty weight",
                main=names(mults[[d]])[m], legend.text=FALSE, col=col[m])
        abline(h=1, lty=2)
      }
    } else if(codatatype[d]=="continuous") {
      for(m in 1:ncol(mults[[d]])) {
        plot(codata[, d], mults[[d]][, m], xlab=colnames(codata)[d],
             ylab="Penalty weight", main=colnames(mults[[d]])[m])  
        abline(h=1, lty=2)
      }
    }
  }
  par(opar)
}




