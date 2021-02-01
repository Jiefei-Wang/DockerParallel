SERVICE_ID <- "AmazonEC2ContainerServiceV20141113"

ecs_REST_request <-function(method, target, headers, body){
  service= "ecs"
  region <- get_aws_region()
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
  response <- POST(
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
  #stop_for_status(response)
  content(response, type = "application/json")
}


ecs_post<-function(target, headers=c(), request = NULL){
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
    target,
    query = list(),
    headers = list()
  ) {
    version = "2016-11-15"
    region <- get_aws_region()
    query$Action = target
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
      secret = get_secret_access_key(),
      verbose = verbose)
    headers[["x-amz-date"]] <- datetime
    headers[["Authorization"]] <- Sig$SignatureHeader
    H <- do.call(httr::add_headers, headers)

    # execute request
    if (length(query)) {
      r <- httr::GET(url, H, query = query)
    } else {
      r <- httr::GET(url, H)
    }
    if (httr::http_error(r)) {
      tmp <- gsub("\n\\s*", "", httr::content(r, "text", encoding = "UTF-8"))
      x <- try(xml2::as_list(xml2::read_xml(tmp)), silent = TRUE)
      msg <- paste0(x, collapse = "\n")
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
