combineList <- function(x, newX){
    for(i in seq_along(newX)){
        x[[names(newX)[i]]] <- newX[[i]]
    }
    x
}

packPackages <- function(x){
    encodedX <- vapply(x, function(i) URLencode(i, reserved = TRUE), character(1))
    paste0(encodedX, collapse = ",")
}
