aws_list_route_tables<-function(tag_filter = NULL,
                                id_filter = NULL,
                                vpc_filter = NULL,
                                gateway_filter = NULL){
  action <- "DescribeRouteTables"
  query <- list()
  filter_i <- 0
  if(!is.null(id_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "route-table-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- id_filter
  }
  if(!is.null(vpc_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "vpc-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- vpc_filter
  }
  if(!is.null(gateway_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "route.gateway-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- gateway_filter
  }
  if(!is.null(tag_filter)){
    for(j in seq_along(tag_filter)){
      filter_i = filter_i + 1
      query[[paste0("Filter.",filter_i,".Name")]] <- names(tag_filter)[j]
      query[[paste0("Filter.",filter_i,".Value.1")]] <- unname(tag_filter[j])
    }
  }

  response <- ec2_GET(action = action, query = query)
  route_ids <- vapply(response$routeTableSet, function(x)x$routeTableId[[1]], character(1))
  vpc_ids <- vapply(response$routeTableSet, function(x)x$vpcId[[1]], character(1))
  while(!is.null(response$nextToken)){
    query$NextToken <- response$nextToken
    response <- ec2_GET(action = action, query = query)
    route_ids <- c(route_ids,
                   vapply(response$routeTableSet, function(x)x$routeTableId[[1]], character(1)))
    vpc_ids <- c(vpc_ids,
                 vapply(response$routeTableSet, function(x)x$vpcId[[1]], character(1)))
  }
  data.frame(route_id=route_ids,
             vpc_id=vpc_ids)
}



aws_config_route_table <- function(config){
  if(!is_valid(config, "route_table_id")){
    aws_config_vpc_id(config)
    route_table_list <- aws_list_route_tables(vpc_filter = config$vpc_id)
    if(config$route_table_id=="auto"){
      config$route_table_id <- route_table_list$route_id[1]
    }else{
      idx <- which(route_table_list$route_id==config$route_table_id)
      if(length(idx)==0){
        stop("The route table <",config$route_table_id,"> does not exit")
      }else{
        if(route_table_list$vpc_id!=config$vpc_id){
          stop("The route table <",config$route_table_id,"> and vpc <",config$vpc_id,"> does not match")
        }
      }
    }
    aws_config_default_route(config)
    set_valid(config, "route_table_id")
  }
  config$route_table_id
}


#################################
# route rule in a tale
#################################
aws_list_route<-function(table_id = NULL){
  action <- "DescribeRouteTables"
  query <- list()
  query[["RouteTableId.1"]] <- table_id
  response <- ec2_GET(action = action, query = query)
  route_set <- response$routeTableSet$item$routeSet

  dest_cidr <- vapply(route_set, function(x) x$destinationCidrBlock[[1]], character(1))
  gateway_ids <- vapply(route_set, function(x) x$gatewayId[[1]], character(1))
  states <- vapply(route_set, function(x) x$state[[1]], character(1))
  dest_cidr <- dest_cidr[states=="active"]
  gateway_ids <- gateway_ids[states=="active"]

  data.frame(cidr =dest_cidr,
             gateway = gateway_ids)
}

aws_config_default_route<-function(config){
  aws_config_internet_gateway(config)
  route_list <- aws_list_route(config$route_table_id)
  if(!"0.0.0.0/0"%in%route_list$cidr){
      action <- "CreateRoute"
      query <- list(
        DestinationCidrBlock = "0.0.0.0/0",
        GatewayId = config$internet_gateway_id,
        RouteTableId = config$route_table_id
      )
      response <- ec2_GET(action = action, query = query)
  }
}


