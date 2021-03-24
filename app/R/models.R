cv=FALSE
# codatatype=codatatype
fitFunction <- function(response, features, codata, methods, settings, 
                        codatatype, split, responsetype, cv=FALSE) {
  
  # if(!cv) {
  #   showModal(modalDialog("Fitting models"))
  # }
  out <- list()
  # fit the model depending on type
  if("enet" %in% methods) {
    
    # find glmnet settings
    sets <- settings[substr(names(settings), 9, 14)=="glmnet"]
    names(sets) <- substr(names(sets), 15, nchar(names(sets)))
    sets <- sets[names(sets)!="..."]
    
    # set ill-defined settings
    nobs <- ncol(features)
    nvars <- nrow(features)
    if(is.language(sets$dfmax)) {
      dfmax <- eval(sets$dfmax)
    } else {
      dfmax <- eval(parse(text=sets$dfmax))  
    }
    if(is.language(sets$penalty.factor)) {
      penalty.factor <- eval(sets$penalty.factor)
    } else {
      penalty.factor <- eval(parse(text=sets$penalty.factor))
    }
    if(!is.null(response$unpen)) {
      penalty.factor <- c(rep(0, times=ncol(response$unpen)), penalty.factor)
    }
    sets <- sets[names(sets)!="penalty.factor"]
    if(any(sapply(sets, is.language))) {
      sets <- sapply(sets, deparse)  
    }
    
    # create model object
    model <- sapply(sets, function(s) {gsub('\"', "'", s)})
    model["family"] <- paste0("'", responsetype, "'")
    if(responsetype=="cox") {
      model <- model[names(model)!="intercept"]
    }
    model <- sapply(1:length(model), function(i) {
      paste0(names(model)[i], "=", model[[i]])})
    model <- paste0(model, collapse=", ")
    model <- gsub("y=,", "", model)
    model <- gsub("y=)", "", model)
    model <- gsub("x=,", "", model)
    model <- gsub("x=)", "", model)
    
    # paste the model object and fit
    model <- paste0("cv.glmnet(", model, ", ",
                    ifelse(responsetype=="cox", "y=as.matrix(response$y)", 
                           "y=response$y"), 
                    ", x=cbind(response$unpen, t(features))", 
                    ", penalty.factor=penalty.factor)")
    modfit <- eval(parse(text=model))
    # modfit <- model
    out <- c(out, enet=list(modfit))
    
  }
  if("gren" %in% methods) {
    
    # find gren settings
    sets <- settings[substr(names(settings), 9, 12)=="gren"]
    names(sets) <- substr(names(sets), 13, nchar(names(sets)))
    sets <- sets[names(sets)!="..."]
    if(any(sapply(sets, is.language))) {
      sets <- sapply(sets, deparse)  
    }
    
    # adjust data
    y <- response$y
    if(is.null(ncol(y)) | ncol(y)==1) {
      y <- as.numeric(y)
    }
    x <- t(features)
    
    # check codata
    codatatypes <- unlist(codatatype)
    codatatypes <- codatatypes[match(substr(names(codatatypes), 11, 1000000L),
                                     colnames(codata))]
    
    #########################################################
    ### testings  
    # library(gdata)
    # source("app/R/data.R")
    # # codata <- codataFile(list(datapath="data/cdataV.txt"))
    # codata <- codataFile(file=list(datapath="data/codata_cat_cont_hier.txt"))
    # response <- responseFile(list(datapath="data/respV.xlsx"))
    # codatatypes <- c("categorical", "continuous", "categorical")
    # split <- c("both", "both", "both")
    # 
    # co <- 2
    codatagren <- vector("list", ncol(codata))
    # names(codatagren) <- colnames(codata)
    for(co in 1:ncol(codata)) {
      
      if(codatatypes[co]=="categorical") {
        codatagren[[co]] <- rep(NA, nrow(codata))
        for(gr in 1:length(unique(codata[, co]))) {
          codatagren[[co]][as.character(codata[, co]) %in% 
                             levels(as.factor(codata[, co]))[gr]] <- gr
        }
      } else if(codatatypes[co]=="continuous") {
        ct <- cut(codata[, co], 
                  unique(quantile(codata[, co], probs=seq(0, 1, length.out=6))),
                  include.lowest=TRUE)
        codatagren[[co]] <- rep(NA, nrow(codata))
        for(gr in 1:length(levels(ct))) {
          codatagren[[co]][as.character(ct) %in% levels(ct)[gr]] <- gr
        }
      }
    }
    #########################################################
    
    # codatagren <- codata
    # if(any(codatatypes=="continuous")) {
    #   codatagren[, codatatypes=="continuous"] <-
    #     apply(codatagren[, codatatypes=="continuous", drop=FALSE], 2,
    #           function(cd) {
    #             cut(cd, unique(quantile(cd, probs=seq(0, 1, length.out=6))),
    #                 include.lowest=TRUE)})  
    # }
    # 
    # # iscat <- codatatypes=="categorical"
    # # codatagren <- codata[, iscat, drop=FALSE]
    # codatagren <- apply(codatagren, 2, function(s) {
    #   if(suppressWarnings(any(is.na(as.numeric(s))))) {
    #     s <- as.numeric(as.factor(s))
    #   } else {
    #     s <- as.numeric(s)
    #   }
    #   return(s)})

    # create model object
    model <- sapply(sets, function(s) {gsub('\"', "'", s)})
    model <- sapply(1:length(model), function(i) {
      paste0(names(model)[i], "=", model[[i]])})
    model <- paste0(model, collapse=", ")
    model <- gsub("trace=TRUE", "trace=FALSE", model)
    model <- gsub("partitions=NULL",
                  # "partitions=split(codatagren, rep(1:ncol(codatagren), each=nrow(codatagren)))",
                  "partitions=codatagren",
                  model)
    model <- gsub("unpenalized=NULL", "", model)
    model <- gsub("unpenalized=,", "", model)
    model <- gsub("unpenalized=", "", model)
    model <- gsub("y=,", "", model)
    model <- gsub("y=", "", model)
    model <- gsub("x=,", "", model)
    model <- gsub("x=", "", model)

    # fit gren
    model <- paste0("gren(", model, ", y=y, x=x, unpenalized=response$unpen)")
    modfit <- eval(parse(text=model))
    modfit <- c(modfit, codata=list(codatagren))
    class(modfit) <- "gren"
    out <- c(out, gren=list(modfit))
  }
  if("ecpc" %in% methods) {
    
    # find ECPC settings
    sets <- settings[substr(names(settings), 9, 12)=="ecpc"]
    names(sets) <- substr(names(sets), 13, nchar(names(sets)))
    sets <- sets[names(sets)!="..."]
    sets[names(sets) %in% c("hypershrinkage", "postselection")] <-
      lapply(sets[names(sets) %in% c("hypershrinkage", "postselection")],
             function(s) {paste0("'", s, "'")})
    
    # if(suppressWarnings(any(is.na(as.numeric(sets[["lambda"]]))))) {
    #   sets["lambda"] <- paste0("'", sets["lambda"], "'")
    # }  
    
    # adjust codata
    codatatypes <- unlist(codatatype)
    codatatypes <- codatatypes[match(substr(names(codatatypes), 11, 1000000L),
                                     colnames(codata))]
    split <- unlist(split)
    split <- split[match(substr(names(split), 6, 1000000L),
                         colnames(codata))]
    

    #########################################################
    ### testings  
    # library(gdata)
    # source("app/R/data.R")
    # # codata <- codataFile(list(datapath="data/cdataV.txt"))
    # codata <- codataFile(file=list(datapath="data/codata_cat_cont_hier.txt"))
    # response <- responseFile(list(datapath="data/respV.xlsx"))
    # codatatypes <- c("categorical", "continuous", "categorical")
    # split <- c("both", "both", "both")
    
    # co <- 1
    groupsets <- groupsets.grouplvl <- vector("list", ncol(codata))
    # names(groupsets) <- names(groupsets.grouplvl) <- colnames(codata)
    for(co in 1:ncol(codata)) {
      
      if(codatatypes[co]=="categorical" &
         any(sapply(strsplit(as.character(codata[, co]), ";"), length) > 1)) {
        spl <- strsplit(as.character(codata[, co]), ";")
        groupsets[[co]] <- 
          apply(sapply(1:length(unique(unlist(spl))), function(g) {
            sapply(1:length(spl), function(j) {g %in% spl[[j]]})}), 2, 
            function(m) {
              which(m) + ifelse(is.null(ncol(response$unpen)), 0, 
                                ncol(response$unpen))})
        # names(groupsets[[co]]) <- unique(unlist(spl))
        groupsets.grouplvl[[co]] <- obtainHierarchy(groupsets[[co]])
      } else if(codatatypes[co]=="categorical") {
        groupsets[[co]] <- vector("list", length(unique(codata[, co])))
        # names(groupsets[[co]]) <- levels(as.factor(codata[, co]))
        for(gr in 1:length(groupsets[[co]])) {
          groupsets[[co]][[gr]] <- 
            which(as.character(codata[, co]) %in% 
                    levels(as.factor(codata[, co]))[gr]) +
            ifelse(is.null(ncol(response$unpen)), 0, ncol(response$unpen))
        }
        groupsets.grouplvl[[co]] <- NULL  
      } else if(codatatypes[co]=="continuous") {
        groupsets[[co]] <- splitMedian(
          as.numeric(codata[, co]), index=1:length(codata[, co]) +
            ifelse(is.null(ncol(response$unpen)), 0, ncol(response$unpen)),
          minGroupSize=10, split=split[co])
        groupsets.grouplvl[[co]] <- obtainHierarchy(groupsets[[co]])
      }
    }
    #########################################################
    
    # split <- split[codatatypes=="continuous"]
    # 
    # iscat <- codatatypes=="categorical"
    # isnum <- apply(codata, 2, function(x) {
    #   !suppressWarnings(any(is.na(as.numeric(x))))})
    # 
    # if(any(!isnum & iscat)) {
    #   hiergroupsets <- apply(
    #     codata[, !isnum & iscat, drop=FALSE], 2, function(col) {
    #       spl <- strsplit(as.character(col), ";");
    #       apply(sapply(1:length(unique(unlist(spl))), function(g) {
    #         sapply(1:length(spl), function(j) {g %in% spl[[j]]})}), 2, which)})
    # } else {
    #   hiergroupsets <- NULL
    # }
    # if(any(isnum & !iscat)) {
    #   contgroupsets <- sapply(1:sum(isnum & !iscat), function(j) {
    #     cd <- codata[, isnum & !iscat, drop=FALSE][, j]
    #     splitMedian(as.numeric(cd), index=1:length(cd) +
    #                   ifelse(is.null(ncol(response$unpen)), 0,
    #                          ncol(response$unpen)),
    #                 minGroupSize=10, split=split[j])}, simplify=FALSE)
    # } else {
    #   contgroupsets <- NULL
    # }
    # if(any(isnum & iscat)) {
    #   catgroupsets <- apply(
    #     codata[, isnum & iscat, drop=FALSE], 2, function(col) {
    #       lapply(1:length(unique(col)), function(g) {
    #         which(as.numeric(col)==g) +
    #           ifelse(is.null(ncol(response$unpen)), 0,
    #                  ncol(response$unpen))})})
    # } else {
    #   catgroupsets <- NULL
    # }
    # groupsets <- c(catgroupsets, contgroupsets, hiergroupsets)
    # groupsets.grouplvl <- c(vector("list", length(catgroupsets)),
    #                         lapply(contgroupsets, obtainHierarchy),
    #                         lapply(hiergroupsets, obtainHierarchy))
  
    # set default hypershrinkage
    if(sets$hypershrinkage=="''" | sets$hypershrinkage=="c('')") {
      sets$hypershrinkage <- 
        paste0("c(", paste0("'", c("hierLasso,ridge", "ridge")[
          as.numeric(codatatypes=="categorical") + 1], "'", collapse=","), ")")
    }

    # adjust data
    yecpc <- response$y
    family <- responsetype
    family <- paste0("'",
                     ifelse(family=="binomial", "logistic",
                            ifelse(family=="gaussian", "linear", family)),
                     "'")
    if(is.null(ncol(yecpc)) | ncol(yecpc)==1) {
      yecpc <- as.numeric(yecpc)
    }
    xecpc <- cbind(response$unpen, t(features))

    # create model object
    model <- lapply(sets, function(s) {gsub('\"', "'", s)})
    model$model <- family
    model$silent <- T
    model[sapply(model, length)==0] <- list(NULL)
    model <- paste0(names(model), "=", model, collapse=", ")
    model <- paste0(strsplit(model, ", ")[[1]][!sapply(
      strsplit(model, ", ")[[1]], function(s) {
        unlist(strsplit(s, "="))[2] %in% c("''", "NaN", "NA")})],
      collapse=", ")
    model <- gsub("groupsets=", "", model, fixed=TRUE)
    model <- gsub("groupsets=NULL", "", model, fixed=TRUE)
    model <- gsub("groupsets.grouplvl=NULL", "", model, fixed=TRUE)
    model <- gsub("groupsets.grouplvl=", "", model, fixed=TRUE)
    model <- gsub("unpen=NULL", "", model, fixed=TRUE)
    model <- gsub("unpen=", "", model, fixed=TRUE)
    model <- paste0(model, ", groupsets.grouplvl=groupsets.grouplvl",
                    ", groupsets=groupsets")
    if(!is.null(response$unpen)) {
      model <- paste0(model, ", unpen=c(1:ncol(response$unpen))")
    }
    model <- gsub("Y=,", "", model, fixed=TRUE)
    model <- gsub("Y=)", "", model, fixed=TRUE)
    model <- gsub("X=,", "", model, fixed=TRUE)
    model <- gsub("X=)", "", model, fixed=TRUE)

    # fit ECPC
    model <- paste0("ecpc(", model, ", Y=yecpc, X=xecpc)")
    modfit <- eval(parse(text=model))
    # modfit <- model
    # out <- list(ecpc=modfit)
    
    # restore groupsets and add for plots 
    groupsets <- lapply(groupsets, function(gr) {
      lapply(gr, function(g) {g - ifelse(is.null(response$unpen), 0,
                                         ncol(response$unpen))})})
    out <- c(out, ecpc=list(c(modfit, groupsets=list(groupsets),
                              groupsets.grouplvl=list(groupsets.grouplvl))))
    
  }
  # if(!cv) {
  #   removeModal()
  # }
  
  return(out)
}

# response=response(); features=features(); codata=codata();
# methods=methods(); settings=settings();
# codatatype=codatatype(); nfolds=nfolds();
# cvmeasures=cvmeasures()
cvFunction <- function(response, features, codata, methods, settings, 
                       codatatype, split, responsetype, nfolds, cvmeasures) {
  # showModal(modalDialog("Cross validating models, estimating remaining time..."))
  
  # create folds
  n <- ncol(features)
  if(responsetype=="gaussian") {
    foldid <- sample(rep(1:nfolds, times=round(c(rep(
      n %/% nfolds + as.numeric((n %% nfolds)!=0), times=n %% nfolds),
      rep(n %/% nfolds, times=nfolds - n %% nfolds)))))  
  } else if(responsetype=="binomial") {
    n0 <- sum(response$y==0)
    foldid0 <- sample(rep(1:nfolds, times=round(c(rep(
      n0 %/% nfolds + as.numeric((n0 %% nfolds)!=0), times=n0 %% nfolds),
      rep(n0 %/% nfolds, times=nfolds - n0 %% nfolds)))))
    n1 <- sum(response$y==1)
    foldid1 <- sample(rep(1:nfolds, times=round(c(rep(
      n1 %/% nfolds + as.numeric((n1 %% nfolds)!=0), times=n1 %% nfolds),
      rep(n1 %/% nfolds, times=nfolds - n1 %% nfolds)))))
    foldid <- numeric(n)
    foldid[response$y==0] <- foldid0
    foldid[response$y==1] <- foldid1
  } else if(responsetype=="cox") {
    n0 <- sum(response$y[, "status"]==0)
    foldid0 <- sample(rep(1:nfolds, times=round(c(rep(
      n0 %/% nfolds + as.numeric((n0 %% nfolds)!=0), times=n0 %% nfolds),
      rep(n0 %/% nfolds, times=nfolds - n0 %% nfolds)))))
    n1 <- sum(response$y[, "status"]==1)
    foldid1 <- sample(rep(1:nfolds, times=round(c(rep(
      n1 %/% nfolds + as.numeric((n1 %% nfolds)!=0), times=n1 %% nfolds),
      rep(n1 %/% nfolds, times=nfolds - n1 %% nfolds)))))
    foldid <- numeric(n)
    foldid[response$y[, "status"]==0] <- foldid0
    foldid[response$y[, "status"]==1] <- foldid1
  }
  
  # run iterations
  pred <- matrix(nrow=n, ncol=length(methods),
                 dimnames=list(NULL, methods))
  measures <- matrix(0, nrow=length(cvmeasures), ncol=length(methods),
                     dimnames=list(cvmeasures, methods))
  
  # if selection is done with ecpc
  if("ecpc" %in% methods) {
    if(settings[[which(names(settings)=="settingsecpcpostselection")]]!=FALSE) {
      pred <- cbind(pred, "ecpc+sel"=NA)
      measures <- cbind(measures, "ecpc+sel"=0)
    }  
  }
  time <- 0
  for(k in 1:nfolds) {
    
    rem <- round((nfolds - k + 1)*(time/(k - 1)))
    rem <- ifelse(is.nan(rem), "Inf", rem)
    incProgress((k - 1)/nfolds,
                detail=paste0("fold ", k, " of ", nfolds,
                              ", estimated remaining time: ", rem, " seconds"))
    
    start <- unname(proc.time()[3])
    
    # split into training and test
    if(is.null(response$unpen)) {
      trainunpen <- testunpen <- NULL
    } else {
      trainunpen <- response$unpen[foldid!=k, , drop=FALSE]
      testunpen <- response$unpen[foldid==k, , drop=FALSE]
    }
    trainresponse <- list(y=response$y[foldid!=k, , drop=FALSE],
                          family=responsetype, 
                          unpen=trainunpen)
    testresponse <- list(y=response$y[foldid==k, , drop=FALSE],
                         family=responsetype, 
                         unpen=testunpen)
    trainfeatures <- features[, foldid!=k, drop=FALSE]
    testfeatures <- features[, foldid==k, drop=FALSE]
    
    # remove constant features
    isconst <- apply(trainfeatures, 1, sd)==0
    traincodata <- codata[!isconst, , drop=FALSE]
    trainfeatures <- trainfeatures[!isconst, , drop=FALSE]
    testfeatures <- testfeatures[!isconst, , drop=FALSE]
    
    # fit the models
    fit <- fitFunction(response=trainresponse, features=trainfeatures, 
                       codata=traincodata, methods=methods, settings=settings, 
                       codatatype=codatatype, split=split, 
                       responsetype=responsetype, cv=TRUE)
    
    # retrieve predictions
    if("enet" %in% methods) {
      lpred <- predict(fit$enet, cbind(testunpen, t(testfeatures)),
                       type="link", s="lambda.min")
      if(responsetype=="gaussian") {
        pred[foldid==k, "enet"] <- lpred
      } else if(responsetype=="binomial") {
        pred[foldid==k, "enet"] <- 1/(1 + exp(-lpred))
      } else if(responsetype=="cox") {
        pred[foldid==k, "enet"] <- exp(lpred)
      }
    }
    if("gren" %in% methods) {
      pred[foldid==k, "gren"] <-
        predict(fit$gren, cbind(testunpen, t(testfeatures)), type="groupreg")
    }
    if("ecpc" %in% methods) {
      lpred <- cbind(testunpen, t(testfeatures)) %*% fit$ecpc$beta
      if(responsetype=="gaussian") {
        pred[foldid==k, "ecpc"] <- lpred
      } else if(responsetype=="binomial") {
        pred[foldid==k, "ecpc"] <- 1/(1 + exp(-lpred))
      } else if(responsetype=="cox") {
        pred[foldid==k, "ecpc"] <- exp(lpred)
      }
      if("betaPost" %in% names(fit$ecpc)) {
        lpred <- cbind(testunpen, t(testfeatures)) %*% fit$ecpc$betaPost
        if(responsetype=="gaussian") {
          pred[foldid==k, "ecpc+sel"] <- lpred
        } else if(responsetype=="binomial") {
          pred[foldid==k, "ecpc+sel"] <- 1/(1 + exp(-lpred))
        } else if(responsetype=="cox") {
          pred[foldid==k, "ecpc+sel"] <- exp(lpred)
        }
      }
    }
    time <- time + unname(proc.time()[3]) - start
    # removeModal()
    # showModal(modalDialog(paste0(
    #   "Cross validating models, estimating remaining time at ",
    #   round((nfolds - k)*(time/k)), " seconds")))
  }

  # calculate performance measures
  if("deviance" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["deviance", ] <- colMeans((pred - response$y[, , drop=TRUE])^2)
    } else if(responsetype=="binomial") {
      measures["deviance", ] <-
        colSums((response$y[, , drop=TRUE] - 1)*log(1 - pred) - 
                  response$y[, , drop=TRUE]*log(pred))
    } else if(responsetype=="cox") {
      id <- which(response$y[, "status"]==1)
      for(i in id) {
        measures["deviance", ] <- measures["deviance", ] -
          log(pred[i, ]) + log(colSums(pred[c(i:nrow(pred)), , drop=FALSE]))
      }
    }
  }
  if("mse" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["mse", ] <- colMeans((pred - response$y[, , drop=TRUE])^2)
    } else if(responsetype=="binomial") {
      measures["mse", ] <- colMeans((pred - response$y[, , drop=TRUE])^2)
    } else if(responsetype=="cox") {
      measures["mse", ] <- NA
    }
  }
  if("class" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["class", ] <- NA
    } else if(responsetype=="binomial") {
      measures["class", ] <- colMeans(
        (pred >= 0.5)!=response$y[, , drop=TRUE])
    } else if(responsetype=="cox") {
      measures["class", ] <- NA
    }
  }
  if("auc" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["auc", ] <- NA
    } else if(responsetype=="binomial") {
      measures["auc", ] <- apply(pred, 2, function(pr) {
        glmnet:::auc(response$y, pr)})
    } else if(responsetype=="cox") {
      measures["auc", ] <- NA
    }
  }
  if("mae" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["mae", ] <- colMeans(abs(pred - response$y[, , drop=TRUE]))
    } else if(responsetype=="binomial") {
      measures["mae", ] <- colMeans(abs(pred - response$y[, , drop=TRUE]))
    } else if(responsetype=="cox") {
      measures["mae", ] <- NA
    }
  }
  if("C" %in% cvmeasures) {
    if(responsetype=="gaussian") {
      measures["C", ] <- NA
    } else if(responsetype=="binomial") {
      measures["C", ] <- NA
    } else if(responsetype=="cox") {
      measures["C", ] <- apply(pred, 2, function(pr) {
        glmnet:::Cindex(pr, response$y)})
    }
  }
  # removeModal()
  return(list(measures=measures, pred=pred))
}

