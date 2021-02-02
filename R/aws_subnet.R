aws_create_subnet <- function(vpc_id, cidr){
  action <- "CreateSubnet"
  query <- list()
  query$VpcId <- vpc_id
  query$CidrBlock <- cidr
  query[["TagSpecification.1.ResourceType"]] <-"subnet"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <- ec2_GET(action, query = query)
  response$subnet$subnetId[[1]]
}
aws_delete_subnet <- function(subnet_id){
  action <- "DeleteSubnet"
  query <- list(SubnetId=subnet_id)
  response <- ec2_GET(action = action, query = query)
  response
}

aws_list_subnets<-function(tag_filter = NULL, id_filter = NULL, vpc_filter = NULL){
  action <- "DescribeSubnets"
  query <- list()
  filter_i <- 0
  if(!is.null(id_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "subnet-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- id_filter
  }
  if(!is.null(vpc_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "vpc-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- vpc_filter
  }
  if(!is.null(tag_filter)){
    for(j in seq_along(tag_filter)){
      filter_i = filter_i +1
      query[[paste0("Filter.",filter_i,".Name")]] <- names(tag_filter)[j]
      query[[paste0("Filter.",filter_i,".Value.1")]] <- unname(tag_filter[j])
    }
  }

  response <- ec2_GET(action = action, query = query)
  subnet_ids <- vapply(response$subnetSet,function(x)x$subnetId[[1]],character(1))
  vpc_ids <- vapply(response$subnetSet,function(x)x$vpcId[[1]],character(1))
  cidr <- vapply(response$subnetSet,function(x)x$cidrBlock[[1]],character(1))
  while(!is.null(response$nextToken)){
    query$NextToken <- response$nextToken
    response <- ec2_GET(action = action, query = query)
    subnet_ids <- c(subnet_ids,
                 vapply(response$subnetSet,function(x)x$subnetId[[1]],character(1)))
    vpc_ids <- c(vpc_ids,
                    vapply(response$subnetSet,function(x)x$vpcId[[1]],character(1)))
    cidr <- c(cidr,
                 vapply(response$subnetSet,function(x)x$cidrBlock[[1]],character(1)))
  }
  data.frame(subnet_id= subnet_ids, vpc_id = vpc_ids, cidr = cidr)
}


aws_config_subnet_id <- function(config){
  if(!is_valid(config,"subnet_id")){
    aws_config_vpc_id(config)
    if(config$subnet_id=="auto"){
      subnet_list <- aws_list_subnets(tag_filter = c(`tag:docker-parallel-tag`="docker-parallel-tag"))
      if(nrow(subnet_list)!=0){
        config$subnet_id <- subnet_list$subnet_id[1]
      }else{
        config$subnet_id <- aws_create_subnet(config$vpc_id, "10.0.0.0/16")
      }
    }else{
      subnet_list <- aws_list_subnets(id_filter = config$subnet_id)
      if(nrow(subnet_list)==0){
        stop("The subnet <",config$subnet_id,"> does not exist!")
      }
    }
    set_valid(config, "subnet_id")
  }
  config$subnet_id
}
