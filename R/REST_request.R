retry_on_error <- function(func, ..., n_try){
  for(i in seq_len(n_try)){
    response <- tryCatch(
      func(...),
        error = function(e) NULL
    )
    if(!is.null(response)){
      return(response)
    }
    if(package_setting$print_when_retrying)
      print("REST request failed, retrying")
  }
}

GET_EX <- function(..., n_try = 3){
  retry_on_error(func = httr::GET, ..., n_try=package_setting$retry_time,
                 timeout(package_setting$REST_timeout))
}
POST_EX <- function(..., n_try = 3){
  retry_on_error(func = httr::POST, ..., n_try=package_setting$retry_time,
                 timeout(package_setting$REST_timeout))
}
