ecs_package_configure <- new.env()
ecs_package_configure$access_key_id <- NULL
ecs_package_configure$secret_access_key <- NULL
ecs_package_configure$region <- NULL

#' Setting up AWS credentials
#'
#' Setting up AWS credentials. For details, see `?aws.signature::locate_credentials`
#'
#' @param key_file The csv credential file that is downloaded from AWS
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

#
# ecs_package_configure$region<- "ap-southeast-1"
#
# key_file <- "../accessKeys.csv"
# ecs_set_credentials(key_file)
#
#
# library(aws.signature)
# service = "ecs"
# region="ap-southeast-1"
# host <- paste0("https://",service, ".", region, ".amazonaws.com")
# datetime <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
# request_body="{}"
# sig <- signature_v4_auth(
#   datetime = datetime,
#   region=region,
#   verb = "POST", service = service, action = "/",
#   request_body = request_body,
#   key=package_configure$ecs_credentials$access_key_id,
#   secret=package_configure$ecs_credentials$secret_access_key,
#   canonical_headers = c(
#     Host = "ecs.ap-southeast-1.amazonaws.com",
#     `Content-Type` = "application/x-amz-json-1.1",
#     `X-Amz-Target` = paste0(SERVICE_ID, ".", action),
#     `X-Amz-Date` = datetime
#   )
# )
#
#
# SERVICE_ID <- "AmazonEC2ContainerServiceV20141113"
# action <- "ListClusters"
# response <- POST(
#   host,
#   add_headers(
#     `Content-Type` = "application/x-amz-json-1.1",
#     `X-Amz-Date` = datetime,
#     `X-Amz-Target` = paste0(SERVICE_ID, ".", action),
#     Authorization= sig$SignatureHeader
#   ),
#   body = sig$Body
# )
# #stop_for_status(response)
# content(response, type = "application/json")
