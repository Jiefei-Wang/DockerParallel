aws_package_configure <- new.env()
aws_package_configure$access_key_id <- NULL
aws_package_configure$secret_access_key <- NULL
aws_package_configure$region <- NULL

aws_set_credentials<-function(key_file=NULL,
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
    aws_package_configure$access_key_id <- credentials$key
    aws_package_configure$secret_access_key <- credentials$secret
  }
  if(!is.null(credentials$region)){
    aws_package_configure$region <- credentials$region
  }
}

aws_get_credentials <- function(){
  out_access_key_id <- aws_package_configure$access_key_id
  out_secret_access_key <- aws_package_configure$secret_access_key
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
    region = aws_package_configure$region
  )
}

get_access_key_id <- function(){
  aws_package_configure$access_key_id
}

get_secret_access_key <- function(){
  aws_package_configure$secret_access_key
}

get_aws_region<-function(){
  aws_package_configure$region
}

#
# aws_package_configure$region<- "ap-southeast-1"
#
# key_file <- "../accessKeys.csv"
# aws_set_credentials(key_file)
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
#   key=package_configure$aws_credentials$access_key_id,
#   secret=package_configure$aws_credentials$secret_access_key,
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
