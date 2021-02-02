aws_create_internet_gateway <- function(){
  action <- "CreateInternetGateway"
  query <- list()
  query[["TagSpecification.1.ResourceType"]] <-"internet-gateway"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <- ec2_GET(action, query = query)
  response$internetGateway$internetGatewayId[[1]]
}
aws_delete_internet_gateway <- function(gateway_id){
  gateway_list <- aws_list_internet_gateways(id_filter = gateway_id)
  if(all(gateway_list$gateway_id!=gateway_id)){
    return()
  }
  idx <- which(gateway_list$gateway_id==gateway_id)
  vpc_id <- gateway_list$vpc_id[idx]
  if(vpc_id!="NULL"){
    detach_internet_gateway(vpc_id,gateway_id)
  }
  action <- "DeleteInternetGateway"
  query <- list(InternetGatewayId=gateway_id)
  response <- ec2_GET(action = action, query = query)
  response
}

aws_list_internet_gateways<-function(tag_filter = NULL,
                                     vpc_filter = NULL, id_filter = NULL){
  action <- "DescribeInternetGateways"
  query <- list()
  filter_i <- 0
  if(!is.null(vpc_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "attachment.vpc-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- vpc_filter
  }
  if(!is.null(id_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "internet-gateway-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- id_filter
  }
  if(!is.null(tag_filter)){
    for(j in seq_along(tag_filter)){
      filter_i = filter_i + 1
      query[[paste0("Filter.",filter_i,".Name")]] <- names(tag_filter)[j]
      query[[paste0("Filter.",filter_i,".Value.1")]] <- unname(tag_filter[j])
    }
  }

  response <- ec2_GET(action = action, query = query)
  result <- lapply(response$internetGatewaySet,process_gateway)
  result <- do.call("rbind", result)
  while(!is.null(response$nextToken)){
    query$NextToken <- response$nextToken
    response <- ec2_GET(action = action, query = query)
    result2 <- lapply(response$internetGatewaySet,process_gateway)
    result <- rbind(result, do.call("rbind", result2))
  }
  if(is.null(result)){
    data.frame(gateway_id=character(),
               vpc_id=character())
  }else{
    result
  }

}

process_gateway<-function(gateway){
  id <- gateway$internetGatewayId[[1]]
  attached_vpc <- lapply(gateway$attachmentSet,function(x)x$vpcId[[1]])
  idx <- which(!vapply(attached_vpc, is.null, logical(1)))
  if(length(idx)==0){
    attached_vpc<- "NULL"
  }else{
    attached_vpc <- attached_vpc[[idx]]
  }
  data.frame(gateway_id=rep(id,length(attached_vpc)),vpc_id=attached_vpc)
}

aws_config_internet_gateway <- function(config){
  if(!is_valid(config,"internet_gateway_id")){
    aws_config_vpc_id(config)
    gateway_list <- aws_list_internet_gateways(tag_filter = c(`tag:docker-parallel-tag`="docker-parallel-tag"))
    require_attach <- TRUE
    if(nrow(gateway_list)!=0){
      if(any(gateway_list$vpc_id==config$vpc_id)){
        require_attach <- FALSE
        config$internet_gateway_id <- gateway_list$gateway_id[gateway_list$vpc_id==config$vpc_id][1]
      }else if(any(gateway_list$vpc_id=="NULL")){
        config$internet_gateway_id <- gateway_list$gateway_id[gateway_list$vpc_id=="NULL"][1]
      }else{
        config$internet_gateway_id <- aws_create_internet_gateway()
      }
    }else{
      config$internet_gateway_id <- aws_create_internet_gateway()
    }
    if(require_attach){
      attach_internet_gateway(config$vpc_id, config$internet_gateway_id)
    }
    set_valid(config,"internet_gateway_id")
  }
  config$internet_gateway_id
}

attach_internet_gateway<-function(vpc_id, gateway_id){
  action <- "AttachInternetGateway"
  query <- list(InternetGatewayId=gateway_id, VpcId = vpc_id)
  response <- ec2_GET(action = action, query = query)
  response
}

detach_internet_gateway<-function(vpc_id, gateway_id){
  action <- "DetachInternetGateway"
  query <- list(InternetGatewayId=gateway_id, VpcId = vpc_id)
  response <- ec2_GET(action = action, query = query)
  response
}
