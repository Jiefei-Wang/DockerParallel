ecs_package_configure <- new.env()
ecs_package_configure$access_key_id <- NULL
ecs_package_configure$secret_access_key <- NULL
ecs_package_configure$region <- NULL

#' Setting up AWS credentials
#'
#' Setting up AWS credentials. For details, see `?aws.signature::locate_credentials`
#'
#' @param key_file The csv credential file that is downloaded from AWS
#' @param access_key_id An AWS Access Key ID
#' @param secret_access_key An AWS Secret Access Key
#' @param region A character string containing the AWS region for the request.
#' If missing, “us-east-1” is assumed.
#' @param profile A character string specifying which profile to use from the
#' file. By default, the profile named in `AWS_PROFILE` is used, otherwise the
#' “default” profile is used.
#' @return
#' A list containing credentials(with asterisk) and region.
#' @export
ecs_set_credentials<-function(key_file=NULL,
                              access_key_id=NULL, secret_access_key=NULL, region = NULL,
                              profile = NULL){
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
  credentials <- aws.signature::locate_credentials(key = access_key_id,
                                    secret = secret_access_key,
                                    region=region,
                                    profile=profile)
  if(!is.null(credentials$key)&&!is.null(credentials$secret)){
    ecs_package_configure$access_key_id <- credentials$key
    ecs_package_configure$secret_access_key <- credentials$secret
  }
  if(!is.null(credentials$region)){
    ecs_package_configure$region <- credentials$region
  }
  ecs_get_credentials()
}

ecs_get_credentials <- function(){
  out_access_key_id <- ecs_package_configure$access_key_id
  out_secret_access_key <- ecs_package_configure$secret_access_key
  if(!is.null(out_access_key_id)){
    substr(out_access_key_id,3,nchar(out_access_key_id)-4) <-
      paste0(rep("*",nchar(out_access_key_id)-6),collapse = "")
  }else{
    out_access_key_id <- "NULL"
  }
  if(!is.null(out_secret_access_key)){
    substr(out_secret_access_key,3,nchar(out_secret_access_key)-4) <-
      paste0(rep("*",nchar(out_secret_access_key)-6),collapse = "")
  }else{
    out_secret_access_key <- "NULL"
  }
  list(
    access_key_id = out_access_key_id,
    secret_access_key = out_secret_access_key,
    region = ecs_package_configure$region
  )
}

get_access_key_id <- function(){
  ecs_package_configure$access_key_id
}

get_secret_access_key <- function(){
  ecs_package_configure$secret_access_key
}

get_ecs_region<-function(){
  ecs_package_configure$region
}
