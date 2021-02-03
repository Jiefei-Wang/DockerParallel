SERVICE_ID <- "AmazonEC2ContainerServiceV20141113"

ecs_REST_request <-function(method, target, headers, body){
  service= "ecs"
  region <- get_ecs_region()
  host <- paste0(service, ".", region,".amazonaws.com")
  url <- paste0("https://", host)
  amz_target <- paste0(SERVICE_ID, ".", target)
  datetime <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  sig <- aws.signature::signature_v4_auth(
    datetime = datetime,
    region = region,
    verb = method, service = service, action = "/",
    request_body = body,
    key = get_access_key_id(),
    secret = get_secret_access_key(),
    canonical_headers = c(
      Host = host,
      `Content-Type` = "application/x-amz-json-1.1",
      `X-Amz-Target` = amz_target,
      `X-Amz-Date` = datetime,
      headers
    )
  )
  response <- POST_EX(
    url,
    add_headers(
      `Content-Type` = "application/x-amz-json-1.1",
      `X-Amz-Date` = datetime,
      `X-Amz-Target` = amz_target,
      Authorization= sig$SignatureHeader,
      .headers = as.character(headers)
    ),
    body = sig$Body
  )
  if(httr::http_error(response)){
    stop(content(response, type = "text"))
  }
  #stop_for_status(response)
  content(response, type = "application/json")
}


ecs_POST<-function(target, request = NULL, headers=c()){
  if(is.null(request)||length(request)==0){
    body = "{}"
  }else{
    body <- toJSON(request)
  }
  ecs_REST_request(method= "POST", target=target,
               headers=headers,
               body=body)
}


## Code from: https://github.com/cloudyr/aws.ec2/blob/master/R/ec2HTTP.R
ec2_GET <-
  function(
    action,
    query = list(),
    headers = list()
  ) {
    version = "2016-11-15"
    region <- get_ecs_region()
    query$Action = action
    query$Version <- version
    url <- paste0("https://ec2.", region, ".amazonaws.com")
    datetime <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    Sig <- aws.signature::signature_v4_auth(
      datetime = datetime,
      region = region,
      service = "ec2",
      verb = "GET",
      action = "/",
      query_args = query,
      canonical_headers = list(
        host = paste0("ec2.", region, ".amazonaws.com"),
        `X-Amz-Date` = datetime
      ),
      request_body = "",
      key = get_access_key_id(),
      secret = get_secret_access_key())
    headers[["x-amz-date"]] <- datetime
    headers[["Authorization"]] <- Sig$SignatureHeader
    H <- do.call(httr::add_headers, headers)

    # execute request
    if (length(query)) {
      r <- GET_EX(url, H, query = query)
    } else {
      r <- GET_EX(url, H)
    }
    if (httr::http_error(r)) {
      tmp <- gsub("\n\\s*", "", httr::content(r, "text", encoding = "UTF-8"))
      x <- try(xml2::as_list(xml2::read_xml(tmp)), silent = TRUE)
      if(!is.null(x$Response$Errors$Error)){
        msg <- paste0(x$Response$Errors$Error$Code,"\nMessage: ",x$Response$Errors$Error$Message)
      }else{
        msg <- paste0(x, collapse = "\n")
      }
      stop(msg, call. = FALSE)
    } else {
      tmp <- gsub("\n\\s*", "", httr::content(r, "text", encoding = "UTF-8"))
      out <- try(xml2::as_list(xml2::read_xml(tmp)), silent = TRUE)
      if (inherits(out, "try-error")) {
        out <- structure(httr::content(r, "text", encoding = "UTF-8"))
      }
    }
    out <- out[[1]]
    return(out)
  }
