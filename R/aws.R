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
get_tag_value <-function(tag_list, tag_name, tag_value, target){
  for(i in tag_list){
    if(i[[tag_name]]==target){
      return(i[[tag_value]])
    }
  }
  return(NULL)
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

#################################
# task
#################################


#################################
#  task management
#################################
aws_run_task <- function(n_workers = 1, task_definition=NULL, verbose= TRUE){
  check_ssh_key()
  aws_configure_network(verbose)
  started_number <- 0
  task_ids <- c()
  while(started_number!=n_workers){
    start_time <- Sys.time()
    required_number <- min(10,n_workers-started_number)
    verbose_print(verbose, "starting containers:",started_number,"/",n_workers)
    task_ids<-c(task_ids,
                aws_run_task_internal(task_count=required_number,
                                      n_workers=1,
                                      task_definition = task_definition,
                                      verbose=verbose))
    end_time <- Sys.time()
    started_number <- started_number+required_number
    if(started_number!=n_workers){
      Sys.sleep(11-(end_time-start_time))
    }
  }
  task_id = task_ids
}


valid_vcpu_number <- c(256,512,1024,2048,4096)
calculate_resources<-function(cpu_per_worker, memory_per_worker, n_worker){
  required_cpu<- cpu_per_worker*n_worker
  cpu <- min(valid_vcpu_number[valid_vcpu_number>=required_cpu])
  required_memory <- memory_per_worker*n_worker
  if(required_memory!=512)
    required_memory<-ceiling(required_memory/1024)*1024
  list(cpu = cpu, memory = required_memory)
}


aws_run_task_internal<-function(task_count = 1, n_workers = 1,
                                task_definition=NULL, verbose= TRUE){
  if(is.null(task_definition)){
    task_definition <- aws_find_task_definition()
  }
  resources <- calculate_resources(cpu=as.numeric(get_aws_configure("cpu")),
                                   memory=as.numeric(get_aws_configure("memory")),
                                   n_worker = n_workers)

  cluster_name <- aws_find_cluster_name()
  subnet_id <- aws_find_subnet_id()
  security_group_id <- aws_find_security_group_id()

  config <- fromJSON(file="R/json_config/run-task.json",simplify=FALSE)
  config$overrides$containerOverrides[[1]]$environment[[1]]$value<-
    get_ssh_public_key_value()
  config$overrides$containerOverrides[[1]]$cpu <- resources$cpu
  config$overrides$containerOverrides[[1]]$memory <- resources$memory
  config$taskDefinition <- task_definition
  config$count <- task_count
  config$networkConfiguration$awsvpcConfiguration$securityGroups[[1]]<-security_group_id
  config$networkConfiguration$awsvpcConfiguration$subnets[[1]]<-subnet_id
  #existing_rule <- aws_list_security_rule()
  output <- aws_run_cmd(c("ecs","run-task"),config=config)
  result <- fromJSON(paste0(output,collapse = "\n"))
  task_ids <- vapply(result$task,function(x)x$taskArn, character(1))
  task_ids
}



aws_list_tasks<-function(){
  cluster_name <- aws_find_cluster_name()
  command <- c("ecs","list-tasks","--cluster",
               cluster_name)

  output <- aws_run_cmd(command,config=NULL)
  task_ids <- fromJSON(paste0(output,collapse = "\n"))$taskArns
  task_ids
}

aws_get_task_details <- function(task_ids, get_ip = FALSE){
  result <- c()
  described_task_number <- 0
  while(described_task_number!=length(task_ids)){
    current_task_number <- min(100,length(task_ids)-described_task_number)
    current_task_id <- task_ids[(described_task_number+1):(described_task_number+current_task_number)]
    result<-rbind(result, aws_get_task_details_internal(current_task_id,get_ip=get_ip))
    described_task_number <- described_task_number + current_task_number
  }
  result
}

aws_get_task_details_internal<-function(task_ids, get_ip = FALSE){
  cluster_name <- aws_find_cluster_name()
  command <- c("ecs", "describe-tasks",
               "--cluster", cluster_name,
               "--tasks", paste0(task_ids,collapse = " "))
  output <- aws_run_cmd(command,config=NULL)
  result <- fromJSON(paste0(output,collapse = "\n"))$tasks
  task_ids <- vapply(result,function(x)x$taskArn, character(1))
  status <- vapply(result,function(x)x$lastStatus, character(1))
  if(get_ip){
    IPs <- vapply(result,process_instance_IP,character(1))
    data.frame(task_id = task_ids, status = status, IP = IPs)
  }else{
    data.frame(task_id = task_ids, status = status)
  }
}


process_instance_IP <- function(x){
  eni<-NULL
  for(i in x$attachments){
    if(i$type=="ElasticNetworkInterface"&&i$status=="ATTACHED"){
      eni<- get_tag_value(i$details,"name","value","networkInterfaceId")
    }
  }
  if(!is.null(eni)){
    command <- c("ec2","describe-network-interfaces","--network-interface-ids",eni)
    output <- aws_run_cmd(command,config=NULL)
    result <- fromJSON(paste0(output,collapse = "\n"))
    result$NetworkInterfaces[[1]]$Association$PublicIp
  }else{
    ""
  }
}

aws_stop_tasks<-function(task_ids){
  cluster_name <- aws_find_cluster_name()
  for(id in task_ids){
    command <- c("ecs", "stop-task", "--cluster",cluster_name, "--task",id)
    aws_run_cmd(command,config=NULL)
  }
}

wait_tasks_to_run<-function(task_ids, progress_bar = FALSE){
  if(progress_bar){
    pb <- txtProgressBar(min=0,max = length(task_ids), style=3)
  }
  success <- FALSE
  while(TRUE){
    task_details <- aws_get_task_details(task_ids)
    if(nrow(task_details)!=length(task_ids)){
      break
    }
    if(sum(task_details$status=="STOPPED")>0){
      break
    }
    if(sum(task_details$status=="RUNNING")==length(task_ids)){
      success <- TRUE
      break
    }
    if(progress_bar){
      setTxtProgressBar(pb, sum(task_details$status=="RUNNING"))
    }
  }
  if(progress_bar){
    close(pb)
  }
  success
}
