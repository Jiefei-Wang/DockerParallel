add_asteriod <- function(x, begin_number, end_number){
  substr(x,begin_number+1,nchar(x)-end_number-1) <-
    paste0(rep("*",nchar(x)-begin_number-end_number),collapse = "")
}

generateServerPassword <- function(len = 26){
    paste0(letters[sample(26, len, replace = TRUE)],collapse = "")
}



environmentToJSON <- function(x){
    x <- x[!vapply(x, is.null, logical(1))]
    result <- list()
    for(i in seq_along(x)){
        result[[i]] <- list(name = names(x)[i], value = as.character(x[[i]]))
    }
    result
}
