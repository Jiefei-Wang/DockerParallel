.AWS_Configuration <- setClass("AWS_Configuration", representation(config = "environment", validation = "environment"))

aws_configuration<-function(cpu = "256", memory = "512"){
  config <- new.env(parent = emptyenv())
  config$cpu <- cpu
  config$memory <- memory
  config$cluster_name <- "R-worker-cluster"
  config$task_definition_name <- "R-worker-task-definition"
  config$task_name <- "R-worker-task"
  config$security_group_name <- "R-worker-security-group"
  config$vpc_id <- "auto"
  config$subnet_id <- "auto"
  config$security_group_id <- "auto"
  config$internet_gateway_id <- "auto"
  config$route_table_id <- "auto"
  config$image <- "szwjf08/dockerparallel-worker-image"

  .AWS_Configuration(config = config, validation = new.env(parent = emptyenv()))
}
#' @export
setMethod(f = "show",signature = "AWS_Configuration",
          definition = function(object){
            var_list<- c("cpu","memory",
                         "cluster_name",
                         "task_definition_name",
                         "task_name",
                         "security_group_name",
                         "vpc_id","subnet_id","security_group_id","internet_gateway_id","route_table_id")
            for(i in var_list){
              cat(paste0(i,": \t", object@config[[i]]),"\n")
            }
            invisible(NULL)
          })


#' @export
setMethod(f = "names",signature = "AWS_Configuration",
          definition = function(x){
              names(x@config)
          })

#' @export
setMethod(f = "$",signature = "AWS_Configuration",
          definition = function(x, name){
            x@config[[name, exact = FALSE]]
          })
#' @export
setMethod(f = "$<-",signature = "AWS_Configuration",
          definition = function(x, name, value){
            x@config[[name]] <- value
            x
          })
#' @export
setMethod(f = "[[",signature = "AWS_Configuration",
          definition = function(x, i, j, ...){
            x@config[[i]]
          })
#' @export
setMethod(f = "[[<-",signature = "AWS_Configuration",
          definition = function(x, i, j, ...,value){
            x@config[[i]] <- value
            set_invalid(x, i)
            x
          })

is_valid <- function(x, name){
  !is.null(x@validation[[name]])
}
set_valid <- function(x,name){
  x@validation[[name]] <- TRUE
}
set_invalid <- function(x, name){
  x@validation[[name]] <- NULL

}
