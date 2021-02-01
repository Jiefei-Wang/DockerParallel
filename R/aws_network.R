
aws_configure_network<-function(verbose= TRUE){
  if(!get_aws_configure("network_initialized")){
    verbose_print(verbose, "Setting up VPC")
    vpc_id <- aws_find_vpc_id()
    verbose_print(verbose, "VPC:", vpc_id)
    ## gateway
    verbose_print(verbose, "Setting up gateway")
    gateway_id <- aws_find_internet_gateway()
    verbose_print(verbose, "Gateway:", gateway_id)
    verbose_print(verbose, "Attaching gateway to the VPC")
    attach_internet_gateway(vpc_id,gateway_id)
    ## route table
    verbose_print(verbose, "Setting up route table")
    route_table_id <- aws_find_route_table()
    verbose_print(verbose, "Route table:", route_table_id)
    verbose_print(verbose, "Adding the gateway as the default route")
    add_default_route(route_table_id,gateway_id)
    ## security group
    verbose_print(verbose, "Setting up security group")
    security_group_id <- aws_find_security_group_id()
    verbose_print(verbose, "Security group:",security_group_id)
    verbose_print(verbose, "Adding the default security rule")
    aws_add_security_group_rule(security_group_id)
    ## subnet
    verbose_print(verbose, "Setting up subnet")
    subnet_id <- aws_find_subnet_id()
    verbose_print(verbose, "Subnet:", subnet_id)
    set_aws_configure("network_initialized", TRUE)
  }else{
    verbose_print(verbose, "Network has been initialized")
  }
}
#################################
# vpc
#################################


#################################
# subnet
#################################
aws_create_subnet <- function(){
  config <- fromJSON(file="R/json_config/create-subnet.json",simplify=FALSE)
  config$VpcId<-aws_find_vpc_id()
  output <- aws_run_cmd(c("ec2","create-subnet"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  set_aws_configure("subnet_id",result$Subnet$SubnetId)
  result
}
aws_delete_subnet <- function(subnet_id){
  output <- aws_run_cmd(c("ec2","delete-subnet","--subnet-id", subnet_id),config=NULL)
}

aws_list_subnets<-function(worker_only=FALSE, args=NULL){
  if(worker_only){
    command <- c("ec2","describe-subnets","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag", args)
  }else{
    command <- c("ec2","describe-subnets", args)
  }
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]]
  subnet_ids <- vapply(json_result,function(x)x$SubnetId,character(1))
  vpc_ids <- vapply(json_result,function(x)x$VpcId,character(1))
  cidr <- vapply(json_result,function(x)x$CidrBlock,character(1))
  data.frame(subnet_id=subnet_ids, vpc_id = vpc_ids,
             cidr=cidr)
}
aws_find_subnet_id <- function(){
  if(!is_aws_configure_valid("subnet_id")){
    subnet_list <- aws_list_subnets(worker_only = TRUE)
    if(nrow(subnet_list)!=0){
      set_aws_configure("subnet_id",subnet_list$subnet_id[1])
    }else{
      aws_create_subnet()
    }
  }
  get_aws_configure("subnet_id")
}


#################################
# internet gateway
# set vpc before you use it
#################################
aws_create_internet_gateway <- function(){
  config <- fromJSON(file="R/json_config/create-internet-gateway.json",simplify=FALSE)
  output <- aws_run_cmd(c("ec2","create-internet-gateway"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  set_aws_configure("internet_gateway_id",result$InternetGateway$InternetGatewayId)
  result
}
aws_delete_internet_gateway <- function(gateway_id){
  gateway_list<- aws_list_internet_gateways(args = c("--filters",paste0("Name=internet-gateway-id,Values=",gateway_id)))
  for(i in gateway_list$vpc_id[gateway_list$vpc_id!="NULL"]){
    detach_internet_gateway(i,gateway_id)
  }
  output <- aws_run_cmd(c("ec2","delete-internet-gateway","--internet-gateway-id", gateway_id),config=NULL)
}

aws_list_internet_gateways<-function(worker_only=FALSE, args = NULL){
  if(worker_only){
    command <- c("ec2","describe-internet-gateways","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag",args)
  }else{
    command <- c("ec2","describe-internet-gateways",args)
  }
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]]
  result <- data.frame(
    matrix(ncol=2,nrow=0,
           dimnames=list(NULL,
                         c("gateway_id", "vpc_id"))
    ))
  for(i in json_result){
    result<-rbind(result, process_gateway(i))
  }
  result
}

process_gateway<-function(gateway){
  id <- gateway$InternetGatewayId
  attached_vpc <- vapply(gateway$Attachments,function(x)x$VpcId,character(1))
  if(length(attached_vpc)==0){
    attached_vpc<- "NULL"
  }
  data.frame(gateway_id=rep(id,length(attached_vpc)),vpc_id=attached_vpc)
}

aws_find_internet_gateway <- function(){
  if(!is_aws_configure_valid("internet_gateway_id")){
    gateway_list <- aws_list_internet_gateways(worker_only = TRUE)
    if(nrow(gateway_list)!=0){
      set_aws_configure("internet_gateway_id", gateway_list$gateway_id[1])
    }else{
      aws_create_internet_gateway()
    }
  }
  get_aws_configure("internet_gateway_id")
}

attach_internet_gateway<-function(vpc_id, gateway_id){
  gateway_list <- aws_list_internet_gateways(
    args=c("--filters",paste0("Name=internet-gateway-id,Values=",gateway_id))
  )
  if(length(gateway_list)!=0&&sum(gateway_list$vpc_id==vpc_id)!=0){
    return()
  }
  aws_run_cmd(c("ec2","attach-internet-gateway",
                "--internet-gateway-id", gateway_id,
                "--vpc-id", vpc_id),config=NULL)
}

detach_internet_gateway<-function(vpc_id, gateway_id){
  aws_run_cmd(c("ec2","detach-internet-gateway",
                "--internet-gateway-id", gateway_id,
                "--vpc-id", vpc_id),config=NULL)
}
#################################
# route table
# You must have vpc initialized before run these functions
#################################
aws_list_route_tables<-function(worker_only=FALSE){
  command <- c("ec2","describe-route-tables")
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]]

  route_table_ids <- vapply(json_result,function(x)x$RouteTableId,character(1))
  vpc_ids <- vapply(json_result,function(x)x$VpcId,character(1))
  if(worker_only){
    idx <- (vpc_ids==aws_find_vpc_id())
    route_table_ids<-route_table_ids[idx]
    vpc_ids<-vpc_ids[idx]
  }
  data.frame(table_id =route_table_ids,
             vpc_id = vpc_ids)
}

aws_find_route_table <- function(){
  if(!is_aws_configure_valid("route_table_id")){
    route_table_list <- aws_list_route_tables(worker_only = TRUE)
    set_aws_configure("route_table_id",route_table_list$table_id[1])
  }
  get_aws_configure("route_table_id")
}

#################################
# route
#################################
aws_list_route<-function(table_id = NULL){
  if(is.null(table_id)){
    table_id <- aws_find_route_table()
  }
  command <- c("ec2","describe-route-tables","--filters",paste0("Name=route-table-id,Values=",table_id))
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]][[1]]

  route_cidrs <- vapply(json_result$Routes,function(x)x$DestinationCidrBlock,character(1))
  route_gateways <- vapply(json_result$Routes,function(x)x$GatewayId,character(1))
  route_states <- vapply(json_result$Routes,function(x)x$State,character(1))

  route_cidrs<-route_cidrs[route_states=="active"]
  route_gateways<-route_gateways[route_states=="active"]

  data.frame(cidr =route_cidrs,
             gateway = route_gateways)
}

add_default_route<-function(table_id, gateway_id){
  route_list <- aws_list_route(table_id)
  if(!"0.0.0.0/0"%in%route_list$cidr){
    aws_run_cmd(c("ec2","create-route",
                  "--route-table-id",table_id,
                  "--gateway-id", gateway_id,
                  "--destination-cidr-block", "0.0.0.0/0"),config=NULL)
  }
}

#################################
# security group
#################################
aws_create_security_group <- function(group_name=NULL){
  if(is.null(group_name)){
    group_name <- get_aws_configure("security_group_name")
  }
  config <- fromJSON(file="R/json_config/create-security-group.json",simplify=FALSE)
  config$GroupName <- group_name
  config$VpcId <- aws_find_vpc_id()
  group_list <- aws_list_security_groups()
  if(nrow(group_list)!=0&&
     sum(group_list$name==config$GroupName&group_list$vpc_id==config$VpcId)){
    warning("The group name <", config$GroupName,
            "> has existed in the vpc <",
            config$VpcId,">")
    return()
  }
  output <- aws_run_cmd(c("ec2","create-security-group"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  set_aws_configure("group_name", group_name)
  set_aws_configure("security_group_id",result$GroupId)
  result
}
aws_delete_security_group <- function(group_id){
  aws_run_cmd(c("ec2","delete-security-group","--group-id", group_id),config=NULL)
}
aws_list_security_groups<-function(worker_only = FALSE, args = NULL){
  if(worker_only){
    command <- c("ec2","describe-security-groups","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag",args)
  }else{
    command <- c("ec2","describe-security-groups",args)
  }

  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]]
  group_names <- vapply(json_result,function(x)x$GroupName,character(1))
  group_ids <- vapply(json_result,function(x)x$GroupId,character(1))
  group_vpc_ids <- vapply(json_result,function(x)x$VpcId,character(1))
  is_worker_security_group <- vapply(json_result,function(x)match_tag(x$Tags,"docker-parallel-tag"),logical(1))
  data.frame(name=group_names,
             id= group_ids,
             vpc_id= group_vpc_ids,
             is_worker_security_group = is_worker_security_group)
}
aws_find_security_group_id <- function(){
  if(!is_aws_configure_valid("security_group_id")){
    security_group_list <- aws_list_security_groups(worker_only = TRUE)
    if(nrow(security_group_list)!=0){
      set_aws_configure("security_group_name",security_group_list$name[1])
      set_aws_configure("security_group_id",security_group_list$id[1])
    }else{
      aws_create_security_group()
    }
  }
  get_aws_configure("security_group_id")
}

#################################
# security group policy
#################################
aws_add_security_group_rule <- function(group_id=NULL){
  if(is.null(group_id)){
    group_id <- get_aws_configure("security_group_id")
  }
  config <- fromJSON(file="R/json_config/authorize-security-group-ingress.json",simplify=FALSE)
  config$GroupId <- group_id
  existing_rule <- aws_list_security_rule()
  if(sum(existing_rule$from_port==22&
         existing_rule$to_port==22&
         existing_rule$type=="tcp")!=0){
    return()
  }
  aws_run_cmd(c("ec2","authorize-security-group-ingress"),config=config)
}

aws_list_security_rule<-function(group_id=NULL){
  if(is.null(group_id)){
    group_id <- aws_find_security_group_id()
  }
  command <- c("ec2","describe-security-groups","--filters",
               paste0("Name=group-id,Values=",group_id))
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]][[1]]
  inbound_rules <- json_result$IpPermissions
  result <- data.frame(
    matrix(ncol=5,nrow=0,
           dimnames=list(NULL,
                         c("from_port", "to_port", "type", "ipv4", "ipv6"))
    ))
  for(i in inbound_rules){
    result<-rbind(result, process_inbound_permissions(i))
  }
  result
}


process_inbound_permissions<-function(permission){
  ipv4 <- vapply(permission$IpRanges,function(x)x$CidrIp,character(1))
  ipv6 <- vapply(permission$Ipv6Ranges,function(x)x$CidrIp,character(1))
  n_items <- length(ipv4) + length(ipv6)
  ipv4 <- c(ipv4, rep("", n_items-length(ipv4)))
  ipv6 <- c(rep("", n_items-length(ipv6)), ipv6)

  from_port <- rep(permission$FromPort, n_items)
  to_port<-rep(permission$ToPort, n_items)
  type <-rep(permission$IpProtocol, n_items)

  data.frame(from_port = from_port,
             to_port = to_port,
             type = type,
             ipv4=ipv4,
             ipv6=ipv6)
}

