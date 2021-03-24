# file=list(datapath="data/dataV.xlsx")
### response file input
responseFile <- function(file) {
  
  # read response and unpenalized data
  path <- file$datapath
  if(substr(path, nchar(path) - 4, nchar(path))==".xlsx") {
    # ydata <- read.xls(path, stringsAsFactors=FALSE)
    ydata <- read.xlsx(path)
    if(colnames(ydata)[1]=="") {
      rownames(ydata) <- ydata[, 1]
      ydata <- ydata[, -1]
    }
  } else if(substr(path, nchar(path) - 3, nchar(path)) %in%
            c(".csv", ".txt")) {
    # ydata <- read.table(path, stringsAsFactors=FALSE, sep="\t")
    
    ydata <- read.table(path, stringsAsFactors=FALSE, header=TRUE,
                        sep="\t")
    headrow <- read.table(path, stringsAsFactors=FALSE, nrows=2, sep="\t")[1, ]
    
    if(headrow[1]=="") {
      rown <- ydata[, 1]
      data <- ydata[, -1]
      rownames(ydata) <- rown
    } 
  }
  unpen <- NULL
  
  # check which to use as response
  issurv <- all(c("time", "status") %in% colnames(ydata))
  isresp <- "response" %in% colnames(ydata)
  
  # if we have survival data
  if(issurv & !isresp) {
    y <- Surv(ydata$time, ydata$status)
    dimnames(y)[[1]] <- rownames(ydata)
    family <- "cox"
    if(ncol(ydata) > 2) {
      unpen <- as.matrix(ydata[, !(colnames(ydata) %in% c("time", "status"))])
    }
  
  # if we have logistic or linear  
  } else if(!issurv & isresp) {
    y <- as.matrix(ydata[, "response", drop=FALSE])
    if(!is.numeric(ydata$response) | 
       all(unique(is.numeric(ydata$response)) %in% c(0, 1))) {
      if(!is.numeric(y)) {
        y <- matrix(as.numeric(y==y[1, 1]), 
                    dimnames=list(rownames(ydata), "response"))  
      }
      family <- "binomial"
    } else {
      family <- "gaussian"
    }
    if(ncol(ydata) > 1) {
      unpen <- as.matrix(ydata[, !(colnames(ydata) %in% c("response"))])
    }
  
  # if it is neither explicit survival, linear, or logistic
  } else if(!issurv & !isresp) {
    y <- as.matrix(ydata[, 1])
    if(!is.numeric(ydata$response) | 
       all(unique(is.numeric(ydata$response)) %in% c(0, 1))) {
      if(!is.numeric(y)) {
        y <- matrix(as.numeric(y==y[1, 1]), 
                    dimnames=list(rownames(ydata), "response"))  
      }
      family <- "binomial"
    } else {
      family <- "gaussian"
    }
    if(ncol(ydata) > 1) {
      unpen <- as.matrix(ydata[, -1])
    }
    
  # if both survival as well as linear/logistic are given
  } else if(issurv & isresp) {
    first <- colnames(ydata)[
      which(colnames(ydata) %in% c("response", "time", "status"))[1]]
    if(first %in% c("time", "response")) {
      y <- Surv(ydata$time, ydata$status)
      dimnames(y)[[1]] <- rownames(ydata)
      family <- "cox"
      unpen <- as.matrix(ydata[, !(colnames(ydata) %in% c("time", "status")),
                               drop=FALSE])
    } else if(!is.numeric(ydata[, first]) | 
              all(unique(is.numeric(ydata[, first])) %in% c(0, 1))) {
      y <- as.matrix(ydata[, "response", drop=FALSE])
      if(!is.numeric(y)) {
        y <- matrix(as.numeric(y==y[1, 1]), 
                    dimnames=list(rownames(ydata), "response"))  
      }
      family <- "binomial"
      unpen <- as.matrix(ydata[, !(colnames(ydata) %in% c("response")), 
                               drop=FALSE])
    } else {
      y <- as.matrix(ydata[, "response", drop=FALSE])
      family <- "gaussian"
      unpen <- as.matrix(ydata[, !(colnames(ydata) %in% c("time", "status")), 
                               drop=FALSE])
    }
  }
  
  data <- list(y=y, family="gaussian", unpen=unpen)
  return(data)
}

### feature data input
featuresFile <- function(file) {
  
  
  # read feature data
  path <- file$datapath
  if(substr(path, nchar(path) - 4, nchar(path))==".xlsx") {
    # data <- read.xls(path, stringsAsFactors=FALSE)
    data <- read.xlsx(path)
    if(colnames(data)[1]=="") {
      rownames(data) <- data[, 1]
      data <- data[, -1]
    }
  } else if(substr(path, nchar(path) - 3, nchar(path)) %in% 
            c(".csv", ".txt")) {
    data <- read.table(path, stringsAsFactors=FALSE, header=TRUE,
                       sep="\t")
    headrow <- read.table(path, stringsAsFactors=FALSE, nrows=2, sep="\t")[1, ]
    
    if(headrow[1]=="") {
      rown <- data[, 1]
      data <- data[, -1]
      rownames(data) <- rown
      
    } 
  } 
  data <- as.matrix(data)
  return(data)
}

### codata file input
codataFile <- function(file) {
  # read codata
  path <- file$datapath
  if(substr(path, nchar(path) - 4, nchar(path))==".xlsx") {
    # data <- read.xls(path, stringsAsFactors=FALSE)
    data <- read.xlsx(path)
    if(colnames(data)[1]=="") {
      rownames(data) <- data[, 1]
      data <- data[, -1]
    }
  } else if(substr(path, nchar(path) - 3, nchar(path)) %in% 
            c(".csv", ".txt")) {
    data <- read.table(path, stringsAsFactors=FALSE, header=TRUE,
                       sep="\t")
    headrow <- read.table(path, stringsAsFactors=FALSE, nrows=2, sep="\t")[1, ]
    
    if(headrow[1]=="") {
      rown <- data[, 1]
      data <- data[, -1]
      rownames(data) <- rown
      
    } 
    
  }
  
  return(data)
}






### checks
# checks <- function(input, output, session, response, features, codata) {
#   
#   output$samplenumber <- renderPrint({
#     validate(need(response, message=FALSE))
#     validate(need(features, message=FALSE))
#     if(ncol(features())!=nrow(response()$y)) {
#       "number of samples do not match"
#     }
#   })
# 
#   output$samplenames <- renderPrint({
#     validate(need(response, message=FALSE))
#     validate(need(features, message=FALSE))
#     if(!isTRUE(all.equal(colnames(features()), rownames(response()$y)))) {
#       "sample names do not match"
#     }
#   })
# 
#   output$samplecats <- renderPrint({
#     validate(need(response, message=FALSE))
#     if(!response()$family=="binomial" & length(unique(response()$y)) > 2) {
#       "more than two categories found"
#     }
#   })
# 
#   output$featurenumber <- renderPrint({
#     validate(need(codata, message=FALSE))
#     validate(need(features, message=FALSE))
#     if(nrow(features())!=nrow(codata())) {
#       "number of features do not match"
#     }
#   })
# 
#   output$featurenames <- renderPrint({
#     validate(need(codata, message=FALSE))
#     validate(need(features, message=FALSE))
#     if(!isTRUE(all.equal(rownames(features()), rownames(codata())))) {
#       "feature names do not match"
#     }
#   })
#   
#   ####### add missing values checks ########
#   ####### add missing values checks ########
#   
# }
# 
# checksUI <- function(id) {
#   
#   # Create a namespace function using the provided id
#   ns <- NS(id)
#   
#   # create output
#   tagList(
#     verbatimTextOutput(ns("samplenumber")),
#     verbatimTextOutput(ns("samplenames")),
#     verbatimTextOutput(ns("samplecats")),
#     verbatimTextOutput(ns("featurenumber")),
#     verbatimTextOutput(ns("featurenames"))
#   )
# }




