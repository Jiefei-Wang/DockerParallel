retry_on_error <- function(func, ..., n_try){
  for(i in seq_len(n_try)){
    response <- tryCatch(
      func(...),
        error = function(e) NULL
    )
    if(!is.null(response)){
      return(response)
    }
    print("REST request failed, retrying")
  }
}

GET_EX <- function(..., n_try = 3){
  retry_on_error(func = httr::GET, ..., n_try=n_try)
}
POST_EX <- function(..., n_try = 3){
  retry_on_error(func = httr::POST, ..., n_try=n_try)
}
