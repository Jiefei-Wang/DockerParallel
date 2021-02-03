.ECS_configuration <- setClass("ECS_configuration", representation(config = "environment", validation = "environment"))

#' @export
ecs_configuration<-function(cpu = "256", memory = "512"){
  config <- new.env(parent = emptyenv())
  config$cpu <- cpu
  config$memory <- memory
  config$cluster_name <- "R-worker-cluster"
  config$task_definition_name <- "R-worker-task-definition"
  config$security_group_name <- "R-worker-security-group"
  config$vpc_id <- "auto"
  config$subnet_id <- "auto"
  config$security_group_id <- "auto"
  config$internet_gateway_id <- "auto"
  config$route_table_id <- "auto"
  config$image <- "szwjf08/dockerparallel-worker-image"

  .ECS_configuration(config = config, validation = new.env(parent = emptyenv()))
}
#' @export
setMethod(f = "show",signature = "ECS_configuration",
          definition = function(object){
            # var_list<- c("cluster_name",
            #              "cpu","memory",
            #              "image"
            #              )
            #"security_group_name", "task_definition_name",
            #"vpc_id","subnet_id","security_group_id","internet_gateway_id","route_table_id"
            # for(i in var_list){
            #     cat(paste0(i,": \t", object@config[[i]]),"\n")
            # }
            cat(paste0("cluster_name:\t", object@config[["cluster_name"]]),"\n")
            cat(paste0("cpu:\t\t", object@config[["cpu"]]),"\n")
            cat(paste0("memory:\t\t", object@config[["memory"]]),"\n")
            cat(paste0("image:\t", object@config[["image"]]),"\n")
            invisible(NULL)
          })


#' @export
setMethod(f = "names",signature = "ECS_configuration",
          definition = function(x){
              names(x@config)
          })

#' @export
setMethod(f = "$",signature = "ECS_configuration",
          definition = function(x, name){
            x@config[[name, exact = FALSE]]
          })
#' @export
setMethod(f = "$<-",signature = "ECS_configuration",
          definition = function(x, name, value){
            x@config[[name]] <- value
            x
          })
#' @export
setMethod(f = "[[",signature = "ECS_configuration",
          definition = function(x, i, j, ...){
            x@config[[i]]
          })
#' @export
setMethod(f = "[[<-",signature = "ECS_configuration",
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
