add_asteriod <- function(x, begin_number, end_number){
  substr(x,begin_number+1,nchar(x)-end_number-1) <-
    paste0(rep("*",nchar(x)-begin_number-end_number),collapse = "")
}

generateRandomPassword <- function(len = 26){
    paste0(letters[sample(26, len, replace = TRUE)],collapse = "")
}


verbosePrint<-function(verbose, ...){
    if(verbose)
        message(...)
}
is.empty <- function(x){
    is.null(x) || length(x)==0
}

loadPackage <- function(pkg){
  if(!requireNamespace(pkg)){
    stop("The package <",pkg,"> is not installed")
  }
  library(pkg, character.only = TRUE)
}


getObjectData <- function(x){
  refClass <- x$getRefClass()
  fields <- names(refClass$fields())
  data <- list()
  for(i in fields){
    data[[i]] <- x$field(i)
  }
  data
}
setObjectData <- function(x, staticData){
  for(i in seq_along(staticData)){
    name <- names(staticData)[i]
    x$field(name, staticData[[i]])
  }
}
