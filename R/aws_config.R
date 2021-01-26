package_configure<-new.env()
package_configure$aws_credentials <- NULL
package_configure$aws_configure <- NULL

key_file <- "../accessKeys.csv"
aws_credentials<-function(key_file=NULL,
                          access_key_id=NULL, secret_access_key=NULL){
  profile_name <- shQuote("DockerParallel")
  if(!is.null(key_file)||
     !is.null(access_key_id)||
     !is.null(secret_access_key)){
    if(!is.null(key_file)){
      key_file_content<- read.csv(key_file)
      if(!is.null(key_file_content$Access.key.ID)&&
         is.null(access_key_id)){
        access_key_id <- key_file_content$Access.key.ID
      }
      if(!is.null(key_file_content$Secret.access.key)&&
         is.null(secret_access_key)){
        secret_access_key <- key_file_content$Secret.access.key
      }
    }
    if(is.null(access_key_id)){
      access_key_id <- ""
    }
    if(is.null(secret_access_key)){
      secret_access_key <- ""
    }
    system2("aws",
            args = c("configure","--profile",profile_name),
            input=c(access_key_id,
                    secret_access_key,
                    "",
                    "json"),stdout=TRUE)
  }

  aws_credentials <- list(
    profile_name = profile_name,
    access_key_id = get_aws_account_info("aws_access_key_id",profile_name),
    secret_access_key = get_aws_account_info("aws_secret_access_key",profile_name)
  )
  package_configure$aws_credentials <- aws_credentials
  aws_credentials
}


get_aws_account_info<-function(type, profile_name){
  suppressWarnings(
    info <-
      system2("aws",
              args = c("configure","get",type,"--profile",profile_name),
              stdout=TRUE))
  if(is.null(attr(info,"status"))){
    info
  }else{
    "NULL"
  }
}


aws_configure<-function(region = NULL, cpu = "256", memory = "512"){
  if(is.null(region)){
    region <- "ap-southeast-1"
  }
  aws_configure <- new.env()
  aws_configure$region <- region
  aws_configure$cluster_name <- "R-worker-cluster"
  aws_configure$cpu <- cpu
  aws_configure$memory <- memory
  aws_configure$task_definition_name <- "R-worker-task-definition"
  aws_configure$task_name <- "R-worker-task"
  aws_configure$security_group_name <- "R-worker-security-group"
  aws_configure$vpc_id <- "auto"
  aws_configure$subnet_id <- "auto"
  aws_configure$security_group_id <- "auto"
  aws_configure$internet_gateway_id <- "auto"
  aws_configure$route_table_id <- "auto"

  aws_configure$cluster_name_valid <- FALSE
  aws_configure$task_definition_name_valid <- FALSE
  aws_configure$task_name_valid <- FALSE
  aws_configure$security_group_name_valid <- FALSE

  package_configure$aws_configure <-
    structure(aws_configure, class = "aws_configure")
  package_configure$aws_configure
}

print.aws_configure<-function(x,...){
  config_list <- as.list.environment(x)
  var_list<- c("region","cluster_name",
               "cpu","memory",
               "task_definition_name",
               "task_name",
               "security_group_name",
               "vpc_id","subnet_id","security_group_id","internet_gateway_id","route_table_id")
  print(config_list[var_list])
}
get_aws_configure<-function(name){
  get(name,envir = package_configure$aws_configure)
}
set_aws_configure<-function(name, value){
  assign(name,value, envir = package_configure$aws_configure)
  if(paste0(name,"_valid")%in%names(package_configure$aws_configure)){
  assign(paste0(name,"_valid"),TRUE, envir = package_configure$aws_configure)}
}
is_aws_configure_valid <- function(name){
  valid_flag_name <- c("cluster_name","task_definition_name","task_name","security_group_name")
  if(name%in%valid_flag_name){
    get(paste0(name,"_valid"),envir = package_configure$aws_configure)
  }else{
    get(name,envir = package_configure$aws_configure)!="auto"
  }
}
get_aws_credentials<-function(name){
  package_configure$aws_credentials[[name]]
}
