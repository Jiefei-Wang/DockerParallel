aws_create_vpc <- function(){
  request <- aws_get_json("create-vpc.json")
  response <- aws_post("CreateVpc", request = request, service = "ec2")
  response
}

aws_delete_vpc <- function(vpc_id){
  gateway_list<- aws_list_internet_gateways(args = c("--filters",paste0("Name=attachment.vpc-id,Values=",vpc_id)))
  for(i in gateway_list$gateway_id){
    detach_internet_gateway(vpc_id,i)
  }
  security_group_list <- aws_list_security_groups(args = c("--filters",paste0("Name=vpc-id,Values=",vpc_id)))
  for(i in security_group_list$id[security_group_list$name!="default"]){
    aws_delete_security_group(i)
  }
  subnet_list <- aws_list_subnets(args = c("--filters",paste0("Name=vpc-id,Values=",vpc_id)))
  for(i in subnet_list$subnet_id){
    aws_delete_subnet(i)
  }
  aws_run_cmd(c("ec2","delete-vpc","--vpc-id", vpc_id),config=NULL)
}

aws_list_vpcs<-function(tag_filter = NULL){
  query <- list()

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
  if(!is_aws_configure_valid("vpc_id")){
    vpc_list <- aws_list_vpcs(worker_only = TRUE)
    if(nrow(vpc_list)!=0){
      set_aws_configure("vpc_id",vpc_list$id[1])
    }else{
      aws_create_vpc()
    }
  }
  get_aws_configure("vpc_id")
}
