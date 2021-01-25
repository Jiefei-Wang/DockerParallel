
aws_credential <- list(
  region = "us-east-2",
  access_key_id = "",
  secret_access_key = ""
)

aws_login<-function(){
  system2("aws", args = "configure",
          input=c(aws_credential$access_key_id,
                  aws_credential$secret_access_key,
                  aws_credential$region,
                  ""))
}

save_json <- function(config){
  tmp_file <- tempfile()
  write(toJSON(config), tmp_file)
  tmp_file
}

aws_run_cmd <- function(args, config=NULL){
  full_args <- c("--region", aws_credential$region, args)
  if(!is.null(config)){
    config_path <- save_json(config)
    full_args <- c(full_args, "--cli-input-json", paste0("file://",config_path))
  }
  system2("aws", args = full_args,stdout=TRUE,env=c(AWS_PAGER=""))
}

match_tag<-function(tag_list,target){
  for(i in tag_list){
    if(i$Key==target){
      return(TRUE)
    }
  }
  return(FALSE)
}
#################################
# Cluster
#################################
aws_create_cluster<-function(cluster_name = NULL){
  if(is.null(cluster_name)){
    cluster_name <- aws_configure$default_cluster_name
  }
  config <- fromJSON(file="R/json_config/create-cluster.json",simplify=FALSE)
  config$clusterName <- cluster_name
  output <- aws_run_cmd(c("ecs","create-cluster"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$cluster_name <- result$cluster$clusterArn
  result
}

aws_delete_cluster <- function(cluster_name){
  output <- aws_run_cmd(c("ecs","delete-cluster","--cluster", cluster_name),config=NULL)
  fromJSON(paste0(output,collapse = "\n"))
}

aws_list_clusters<-function(){
  output <- aws_run_cmd(c("ecs","list-clusters"),config=NULL)
  result <- fromJSON(paste0(output,collapse = "\n"))$clusterArns
  result
}
aws_find_cluster_name <- function(){
  if(aws_configure$cluster_name==""){
    cluster_list <- aws_list_clusters()
    idx <- which(endsWith(cluster_list,aws_configure$default_cluster_name))
    if(length(idx)!=0){
      aws_configure$cluster_name = cluster_list[idx]
    }else{
      aws_create_cluster()
    }
  }
  aws_configure$cluster_name
}
#################################
# task
#################################
aws_create_task_definition <- function(task_name = NULL,
                                       cpu = "256", memory = "512"){
  if(is.null(task_name)){
    task_name <- aws_configure$default_task_definition_name
  }
  config <- fromJSON(file="R/json_config/task-definition.json",simplify=FALSE)
  config$family <- task_name
  config$cpu = cpu
  config$memory = memory
  output <- aws_run_cmd(c("ecs","register-task-definition"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$task_definition_name <- result$taskDefinition$family
  result
}

aws_delete_task_definition <- function(task_name){
  output <- aws_run_cmd(c("ecs","deregister-task-definition","--task-definition", task_name),config=NULL)
  fromJSON(paste0(output,collapse = "\n"))
}

aws_list_task_definitions<-function(worker_only = FALSE){
  if(worker_only){
    command <- c("ecs","list-task-definitions","--family-prefix",
                 aws_configure$default_task_definition_name)
  }else{
    command <- c("ecs","list-task-definitions")
  }
  output <- aws_run_cmd(command,config=NULL)
  result <- fromJSON(paste0(output,collapse = "\n"))$taskDefinitionArns
  result
}

aws_find_task_definition <- function(){
  if(aws_configure$task_definition_name==""){
    task_definition_list <- aws_list_task_definitions(worker_only = TRUE)
    if(length(task_definition_list)!=0){
      aws_configure$task_definition_name = task_definition_list[length(task_definition_list)]
    }else{
      aws_create_task_definition()
    }
  }
  aws_configure$task_definition_name
}

#################################
# vpc
#################################
aws_create_vpc <- function(){
  config <- fromJSON(file="R/json_config/create-vpc.json",simplify=FALSE)
  output <- aws_run_cmd(c("ec2","create-vpc"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$vpc_id <- result$Vpc$VpcId
  result
}
aws_delete_vpc <- function(vpc_id){
  output <- aws_run_cmd(c("ec2","delete-vpc","--vpc-id", vpc_id),config=NULL)
}

aws_list_vpcs<-function(worker_only=FALSE){
  if(worker_only){
    command <- c("ec2","describe-vpcs","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag")
  }else{
    command <- c("ec2","describe-vpcs")
  }
  output <- aws_run_cmd(command,config=NULL)
  json_result <- fromJSON(paste0(output,collapse = "\n"))[[1]]
  ids <- vapply(json_result,function(x)x$VpcId,character(1))
  is_worker_vpc <- vapply(json_result,function(x)match_tag(x$Tags,"docker-parallel-tag"),logical(1))
  data.frame(id=ids, is_worker_vpc = is_worker_vpc)
}
aws_find_vpc_id <- function(){
  if(aws_configure$vpc_id==""){
    vpc_list <- aws_list_vpcs(worker_only = TRUE)
    if(nrow(vpc_list)!=0){
      aws_configure$vpc_id = vpc_list$id[1]
    }else{
      aws_create_vpc()
    }
  }
  aws_configure$vpc_id
}

#################################
# subnet
#################################
aws_create_subnet <- function(){
  config <- fromJSON(file="R/json_config/create-subnet.json",simplify=FALSE)
  config$VpcId<-aws_find_vpc_id()
  output <- aws_run_cmd(c("ec2","create-subnet"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$subnet_id <- result$Subnet$SubnetId
  result
}
aws_delete_subnet <- function(subnet_id){
  output <- aws_run_cmd(c("ec2","delete-subnet","--subnet-id", subnet_id),config=NULL)
}

aws_list_subnets<-function(worker_only=FALSE){
  if(worker_only){
    command <- c("ec2","describe-subnets","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag")
  }else{
    command <- c("ec2","describe-subnets")
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
  if(aws_configure$subnet_id==""){
    subnet_list <- aws_list_subnets(worker_only = TRUE)
    if(nrow(subnet_list)!=0){
      aws_configure$subnet_id <- subnet_list$subnet_id[1]
    }else{
      aws_create_subnet()
    }
  }
  aws_configure$subnet_id
}



#################################
# security group
#################################
aws_create_security_group <- function(group_name=NULL){
  if(is.null(group_name)){
    group_name <- aws_configure$default_security_group_name
  }
  config <- fromJSON(file="R/json_config/create-security-group.json",simplify=FALSE)
  config$GroupName=group_name
  config$VpcId=aws_find_vpc_id()
  group_list<- aws_list_security_groups()
  if(nrow(group_list)!=0&&
     sum(group_list$name==config$GroupName&group_list$vpc_id==config$VpcId)){
    warning("The group name <", config$GroupName,
            "> has existed in the vpc <",
            config$VpcId,">")
    return()
  }
  output <- aws_run_cmd(c("ec2","create-security-group"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$security_group_id <- result$GroupId
  result
}
aws_delete_security_group <- function(group_id){
  output <- aws_run_cmd(c("ec2","delete-security-group","--group-id", group_id),config=NULL)
}
aws_list_security_groups<-function(worker_only = FALSE){
  if(worker_only){
    command <- c("ec2","describe-security-groups","--filters",
                 "Name=tag:docker-parallel-tag,Values=docker-parallel-tag")
  }else{
    command <- c("ec2","describe-security-groups")
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
  if(aws_configure$security_group_id==""){
    security_group_list <- aws_list_security_groups(worker_only = TRUE)
    if(nrow(security_group_list)!=0){
      aws_configure$security_group_id = security_group_list$id[1]
    }else{
      aws_create_security_group()
    }
  }
  aws_configure$security_group_id
}

#################################
# security group policy
#################################
aws_add_security_group_rule <- function(group_id){
  if(is.null(group_id)){
    group_id <- aws_configure$default_security_group_name
  }
  config <- fromJSON(file="R/json_config/authorize-security-group-ingress.json",simplify=FALSE)
  config$GroupId <- group_id
  #existing_rule <- aws_list_security_rule()
  output <- aws_run_cmd(c("ec2","authorize-security-group-ingress"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$security_group_name <- result$GroupId
  result
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
  inbound_from_ports <- vapply(inbound_rules,function(x)x$FromPort,numeric(1))
  inbound_to_ports <- vapply(inbound_rules,function(x)x$ToPort,numeric(1))
  inbound_types <- vapply(inbound_rules,function(x)x$IpProtocol,character(1))
  inbound_ipv4 <- vapply(inbound_rules,function(x)x$IpRanges[[1]]$CidrIp,character(1))
  inbound_ipv6 <- vapply(inbound_rules,function(x)x$Ipv6Ranges[[1]]$CidrIp,character(1))
  data.frame(from_port = inbound_from_ports,
             to_port = inbound_to_ports,
             type = inbound_types,
             ipv4=inbound_ipv4,
             ipv6=inbound_ipv6)
}


process_inbound_permissions<-function(){

}


#################################
# run task
#################################
aws_run_task <- function(count = 1, task_definition=NULL, subnet_id=NULL,
                         security_group_id=NULL){
  if(is.null(task_definition)){
    task_definition <- aws_find_task_definition()
  }
  if(is.null(subnet_id)){
    subnet_id <- aws_find_subnet_id()
  }
  if(is.null(security_group_id)){
    security_group_id <- aws_find_security_group_id()
  }
  config <- fromJSON(file="R/json_config/run-task.json",simplify=FALSE)
  config$taskDefinition <- task_definition
  config$count <- 1
  config$networkConfiguration$awsvpcConfiguration$securityGroups[[1]]<-security_group_id
  config$networkConfiguration$awsvpcConfiguration$subnets[[1]]<-subnet_id
  #existing_rule <- aws_list_security_rule()
  output <- aws_run_cmd(c("ecs","run-task"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  aws_configure$security_group_name <- result$GroupId
  result
}






