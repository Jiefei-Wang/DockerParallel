add_asteriod <- function(x, begin_number, end_number){
  substr(x,begin_number+1,nchar(x)-end_number-1) <-
    paste0(rep("*",nchar(x)-begin_number-end_number),collapse = "")
}
