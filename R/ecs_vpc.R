ecs_create_vpc <- function(){
  action <- "CreateVpc"
  query <- list()
  query$CidrBlock <- "10.0.0.0/16"
  query[["TagSpecification.1.ResourceType"]] <-"vpc"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <- ec2_GET(action, query = query)
  response$vpc$vpcId[[1]]
}

ecs_delete_vpc <- function(vpc_id){
  gateway_list<- ecs_list_internet_gateways(vpc_filter = vpc_id)
  for(i in gateway_list$gateway_id){
    detach_internet_gateway(vpc_id, i)
  }
  security_group_list <- ecs_list_security_groups(vpc_filter = vpc_id)
  for(i in security_group_list$group_id[security_group_list$name!="default"]){
    ecs_delete_security_group(i)
  }
  subnet_list <- ecs_list_subnets(vpc_filter = vpc_id)
  for(i in subnet_list$subnet_id){
    ecs_delete_subnet(i)
  }
  action <- "DeleteVpc"
  query <- list(VpcId=vpc_id)
  response <- ec2_GET(action = action, query = query)
  response
}

ecs_list_vpcs<-function(tag_filter = NULL, id_filter = NULL){
  action <- "DescribeVpcs"
  query <- list()
  filter_i <- 0
  if(!is.null(id_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "vpc-id"
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
  vpc_ids <- vapply(response$vpcSet,function(x)x$vpcId[[1]],character(1))
  while(!is.null(response$nextToken)){
    query$NextToken <- response$nextToken
    response <- ec2_GET(action = action, query = query)
    vpc_ids <- c(vpc_ids, vapply(response$vpcSet,function(x)x$vpcId[[1]],character(1)))
  }
  unname(vpc_ids)
}

ecs_config_vpc_id <- function(config){
  if(!is_valid(config,"vpc_id")){
    if(config$vpc_id=="auto"){
      vpc_list <- ecs_list_vpcs(tag_filter = c(`tag:docker-parallel-tag`="docker-parallel-tag"))
      if(length(vpc_list)!=0){
        config$vpc_id <- vpc_list[1]
      }else{
        config$vpc_id <- ecs_create_vpc()
      }
    }else{
      vpc_list <- ecs_list_vpcs(id_filter = config$vpc_id)
      if(length(vpc_list)==0){
        stop("The specific vpc id <",config$vpc_id,"> does not exist!")
      }
    }
    set_valid(config, "vpc_id")
  }
  config$vpc_id
}
