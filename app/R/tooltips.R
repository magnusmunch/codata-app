# create tooltips
manual <- readLines("tooltips.txt")
pivs <- c(which(sapply(manual, substr, 1, 2)=="//"), length(manual) + 1)
tooltips <- vector("list", length(pivs) - 1)
toolnames <- character(length(pivs) - 1)
for(i in 1:(length(pivs) - 1)) {
  tooltips[i] <- trimws(paste0(manual[c((pivs[i] + 1):(pivs[i + 1] - 1))],
                               collapse="\n"))
  toolnames[i] <- trimws(substr(manual[pivs[i]], 3, nchar(manual[pivs[i]])))
}
names(tooltips) <- toolnames
