aws_create_security_group <- function(group_name, vpc_id){
  action <- "CreateSecurityGroup"
  query <- list()
  query$GroupDescription <- "Security group used by R worker nodes"
  query$GroupName <- group_name
  query$VpcId <- vpc_id
  query[["TagSpecification.1.ResourceType"]] <-"security-group"
  query[["TagSpecification.1.Tag.1.Key"]] <-"docker-parallel-tag"
  query[["TagSpecification.1.Tag.1.Value"]] <-"docker-parallel-tag"
  response <- ec2_GET(action, query = query)
  response$groupId[[1]]
}
aws_delete_security_group <- function(group_id){
  action <- "DeleteSecurityGroup"
  query <- list(GroupId=group_id)
  response <- ec2_GET(action = action, query = query)
  response
}
aws_list_security_groups<-function(tag_filter = NULL,
                                   vpc_filter = NULL, name_filter = NULL, id_filter = NULL){
  action <- "DescribeSecurityGroups"
  query <- list()
  filter_i <- 0
  if(!is.null(vpc_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "vpc-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- vpc_filter
  }
  if(!is.null(id_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "group-id"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- id_filter
  }
  if(!is.null(name_filter)){
    filter_i = filter_i + 1
    query[[paste0("Filter.",filter_i,".Name")]] <- "group-name"
    query[[paste0("Filter.",filter_i,".Value.1")]] <- name_filter
  }
  if(!is.null(tag_filter)){
    for(j in seq_along(tag_filter)){
      filter_i = filter_i + 1
      query[[paste0("Filter.",filter_i,".Name")]] <- names(tag_filter)[j]
      query[[paste0("Filter.",filter_i,".Value.1")]] <- unname(tag_filter[j])
    }
  }

  response <- ec2_GET(action = action, query = query)
  group_names <- vapply(response$securityGroupInfo, function(x)x$groupName[[1]], character(1))
  group_ids <- vapply(response$securityGroupInfo, function(x)x$groupId[[1]], character(1))
  group_vpc_ids <- vapply(response$securityGroupInfo, function(x)x$vpcId[[1]], character(1))
  while(!is.null(response$nextToken)){
    query$NextToken <- response$nextToken
    response <- ec2_GET(action = action, query = query)
    group_names <- c(group_names,
                     vapply(response$securityGroupInfo, function(x)x$groupName[[1]], character(1)))
    group_ids <- c(group_ids,
                   vapply(response$securityGroupInfo, function(x)x$groupId[[1]], character(1)))
    group_vpc_ids <- c(group_vpc_ids,
                       vapply(response$securityGroupInfo, function(x)x$vpcId[[1]], character(1)))
  }
  data.frame(name=group_names,
             group_id= group_ids,
             vpc_id= group_vpc_ids)
}

aws_config_security_group_id <- function(config){
  if(!is_valid(config, "security_group_id")){
    aws_config_vpc_id(config)
    if(config$security_group_id=="auto"){
      security_group_list <- aws_list_security_groups(name_filter = config$security_group_name
                                                      ,vpc_filter = config$vpc_id)
      if(nrow(security_group_list)!=0){
        config$security_group_id <- security_group_list$group_id[1]
      }else{
        config$security_group_id <-
          aws_create_security_group(config$security_group_name, config$vpc_id)
      }
    }else{
      security_group_list <- aws_list_security_groups(id_filter = config$security_group_id
                                                      ,vpc_filter = config$vpc_id)
      config$security_group_name <- security_group_list$name[1]
    }
    aws_config_inbound_permissions(config)
    set_valid(config, "security_group_id")
  }
  config$security_group_id
}

#################################
# security group policy
#################################
aws_create_security_group_ipv4_rule <- function(security_group_id){
  action <- "AuthorizeSecurityGroupIngress"
  query <- list()
  query$GroupId <- security_group_id
  query[["IpPermissions.1.FromPort"]] <- 22
  query[["IpPermissions.1.ToPort"]] <- 22
  query[["IpPermissions.1.IpProtocol"]] <- "tcp"
  query[["IpPermissions.1.IpRanges.CidrIp"]] <- "0.0.0.0/0"
  query[["IpPermissions.1.IpRanges.Description"]] <- "allow all ipv4 ssh access"
  response <- ec2_GET(action, query = query)
  response
}
aws_create_security_group_ipv6_rule <- function(security_group_id){
  action <- "AuthorizeSecurityGroupIngress"
  query <- list()
  query$GroupId <- security_group_id
  query[["IpPermissions.1.FromPort"]] <- 22
  query[["IpPermissions.1.ToPort"]] <- 22
  query[["IpPermissions.1.IpProtocol"]] <- "tcp"
  query[["IpPermissions.1.Ipv6Ranges.CidrIpv6"]] <- "::/0"
  query[["IpPermissions.1.Ipv6Ranges.Description"]] <- "allow all ipv6 ssh access"
  response <- ec2_GET(action, query = query)
  response
}

aws_list_security_inbound_rule<-function(security_group_id){
  action <- "DescribeSecurityGroups"
  query <- list()
  filter_i <- 1
  query[[paste0("Filter.",filter_i,".Name")]] <- "group-id"
  query[[paste0("Filter.",filter_i,".Value.1")]] <- security_group_id

  response <- ec2_GET(action = action, query = query)
  from_port_list<-c()
  to_port_list<-c()
  ip_list<-c()
  for(i in response$securityGroupInfo$item$ipPermissions){
    from_port <- i$fromPort[[1]]
    to_port <- i$toPort[[1]]
    ipv4_range <- vapply(i$ipRanges, function(x)x$cidrIp[[1]], character(1))
    ipv6_range <- vapply(i$ipv6Ranges, function(x)x$cidrIp[[1]], character(1))
    ip <- c(ipv4_range,ipv6_range)
    from_port_list <- c(from_port_list, rep(from_port,length(ip)))
    to_port_list <- c(to_port_list, rep(to_port,length(ip)))
    ip_list <- c(ip_list, ip)
  }
  data.frame(from = as.numeric(from_port_list), to = as.numeric(to_port_list), ip = ip_list)
}


aws_config_inbound_permissions<-function(config){
  inbound_rules <- aws_list_security_inbound_rule(config$security_group_id)
  ipv4_idx <- which(inbound_rules$ip=="0.0.0.0/0")
  if(length(ipv4_idx)==0||
     all(inbound_rules$from[ipv4_idx]>22|inbound_rules$to[ipv4_idx]<22)){
    aws_create_security_group_ipv4_rule(config$security_group_id)
  }
  ipv6_idx <- which(inbound_rules$ip=="::/0")
  if(length(ipv6_idx)==0||
     all(inbound_rules$from[ipv6_idx]>22|inbound_rules$to[ipv6_idx]<22)){
    aws_create_security_group_ipv6_rule(config$security_group_id)
  }
}
