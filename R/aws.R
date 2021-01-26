save_json <- function(config){
  tmp_file <- tempfile()
  write(toJSON(config), tmp_file)
  tmp_file
}

aws_run_cmd <- function(args, config=NULL){
  full_args <- c("--region", get_aws_configure("region"),
                 "--profile", get_aws_credentials("profile_name")
                 , args)
  if(!is.null(config)){
    config_path <- save_json(config)
    full_args <- c(full_args, "--cli-input-json", paste0("file://",config_path))
  }
  output<- system2("aws", args = full_args,stdout=TRUE,env=c(AWS_PAGER=""))
  output
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
    cluster_name <- get_aws_configure("cluster_name")
  }
  config <- fromJSON(file="R/json_config/create-cluster.json",simplify=FALSE)
  config$clusterName <- cluster_name
  output <- aws_run_cmd(c("ecs","create-cluster"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  set_aws_configure("cluster_name",result$cluster$clusterArn)
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
  if(!is_aws_configure_valid("cluster_name")){
    cluster_list <- aws_list_clusters()
    if(length(cluster_list)!=0){
      idx <- which(endsWith(cluster_list,.aws_configure()$cluster_name))
      if(length(idx)!=0){
        set_aws_configure("cluster_name", cluster_list[idx[1]])
      }else{
        aws_create_cluster()
      }
    }else{
      aws_create_cluster()
    }
  }
  get_aws_configure("cluster_name")
}
#################################
# task
#################################
aws_create_task_definition <- function(task_name = NULL){
  if(is.null(task_name)){
    task_name <- get_aws_configure("task_definition_name")
  }
  config <- fromJSON(file="R/json_config/task-definition.json",simplify=FALSE)
  config$family <- task_name
  config$cpu = get_aws_configure("cpu")
  config$memory = get_aws_configure("memory")
  output <- aws_run_cmd(c("ecs","register-task-definition"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  set_aws_configure("task_definition_name",result$taskDefinition$taskDefinitionArn)
  result
}

aws_delete_task_definition <- function(task_name){
  output <- aws_run_cmd(c("ecs","deregister-task-definition","--task-definition", task_name),config=NULL)
  fromJSON(paste0(output,collapse = "\n"))
}

aws_list_task_definitions<-function(worker_only = FALSE){
  if(worker_only){
    command <- c("ecs","list-task-definitions","--family-prefix",
                 get_aws_configure("task_definition_name"))
  }else{
    command <- c("ecs","list-task-definitions")
  }
  output <- aws_run_cmd(command,config=NULL)
  result <- fromJSON(paste0(output,collapse = "\n"))$taskDefinitionArns
  result
}

aws_find_task_definition <- function(){
  if(!is_aws_configure_valid("task_definition_name")){
    task_definition_list <- aws_list_task_definitions(worker_only = TRUE)
    if(length(task_definition_list)!=0){
      set_aws_configure("task_definition_name",task_definition_list[length(task_definition_list)])
    }else{
      aws_create_task_definition()
    }
  }
  get_aws_configure("task_definition_name")
}

#################################
# run task
#################################
aws_run_task <- function(count = 1, task_definition=NULL){
  if(is.null(task_definition)){
    task_definition <- aws_find_task_definition()
  }
  aws_configure_network()
  subnet_id <- aws_find_subnet_id()
  security_group_id <- aws_find_security_group_id()

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






