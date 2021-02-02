# config <- aws_configuration()
# aws_initialize_configuration(config)
aws_initialize_configuration<-function(config, verbose= TRUE){
  ## Cluster name
  verbose_print(verbose, "Setting up cluster")
  cluster_name <- aws_config_cluster_name(config)
  verbose_print(verbose, "Cluster name: \t", cluster_name)
  ## VPC
  verbose_print(verbose, "Setting up VPC")
  vpc_id <- aws_config_vpc_id(config)
  verbose_print(verbose, "VPC: \t", vpc_id)
  ## subnet
  verbose_print(verbose, "Setting up subnet")
  subnet_id <- aws_config_subnet_id(config)
  verbose_print(verbose, "Subnet id: \t", subnet_id)
  ## gateway
  verbose_print(verbose, "Setting up gateway")
  gateway_id <- aws_config_internet_gateway(config)
  verbose_print(verbose, "Gateway: \t", gateway_id)
  ## route table
  verbose_print(verbose, "Setting up route table")
  route_table_id <- aws_config_route_table(config)
  verbose_print(verbose, "Route table: \t", route_table_id)
  ## security group
  verbose_print(verbose, "Setting up security group")
  security_group_id <- aws_config_security_group_id(config)
  verbose_print(verbose, "Security group: \t",security_group_id)
  ## Task definition
  verbose_print(verbose, "Setting up task defintion")
  task_definition <- aws_config_task_definition(config)
  verbose_print(verbose, "Task defintion: \t",task_definition)
}

aws_cleanup_configuration <- function(config, verbose = TRUE){
  verbose_print(verbose, "Deleting worker cluster")
  if(config$cluster_name=="R-worker-cluster"){
    tryCatch(aws_delete_cluster(config$cluster_name),
              error = function(e) message(e))
  }
  verbose_print(verbose, "Deleting vpc")
  if(config$vpc_id!="auto"){
    tryCatch(aws_delete_vpc(config$vpc_id),
              error = function(e) message(e))
  }
  verbose_print(verbose, "Deleting internet gateway")
  if(config$internet_gateway_id!="NULL"){
    tryCatch(aws_delete_internet_gateway(config$internet_gateway_id),
              error = function(e) message(e))
  }
  invisible()
}
