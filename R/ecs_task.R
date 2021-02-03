#################################
#  task management
#################################
## config <- ecs_configuration()
## ecs_run_task(config, 4)
ecs_run_task <- function(config, n_workers = 1, verbose= TRUE){
  check_ssh_key()
  ecs_initialize_configuration(config, verbose)
  ## configure hardware
  cpu <- as.numeric(config$cpu)
  memory <- as.numeric(config$memory)
  worker_per_container <- get_n_worker_per_container(cpu,memory)
  # worker_per_container <- 2
  n_containers <- ceiling(n_workers/worker_per_container)
  worker_in_last_container <- n_workers%%worker_per_container
  regular_container_hardware <-
    get_valid_fargate_cpu_memory(cpu*worker_per_container, memory*worker_per_container)
  last_containder_hardware <-
    get_valid_fargate_cpu_memory(cpu*worker_in_last_container, memory*worker_in_last_container)

  if(regular_container_hardware$cpu!=cpu*worker_per_container||
     regular_container_hardware$memory!= memory*worker_per_container){
    message("The required Fargate hardware does not exist,\n",
            "we have upgrate it to cpu ",
            round(regular_container_hardware$cpu/worker_per_container,3)
            ,"units and memory ",
            round(regular_container_hardware$memory/worker_per_container,3)
            ,"MB")
  }

  started_number <- 0
  task_ids <- c()
  while(started_number<n_containers){
    start_time <- Sys.time()
    required_number <- min(10,n_containers-started_number)
    verbose_print(verbose, "starting containers:",started_number,"/",n_containers)
    if(required_number + started_number== n_containers&&worker_in_last_container!=0){
      regular_container_number <- required_number-1
      last_container_indicator <- TRUE
    }else{
      regular_container_number <- required_number
      last_container_indicator <- FALSE
    }
    if(regular_container_number>0){
      task_ids<-c(task_ids,
                  ecs_run_task_internal(config=config,
                                        cpu = regular_container_hardware$cpu,
                                        memory = regular_container_hardware$memory,
                                        task_count=regular_container_number,
                                        verbose=verbose))
    }
    if(last_container_indicator){
      task_ids<-c(task_ids,
                  ecs_run_task_internal(config=config,
                                        cpu = last_containder_hardware$cpu,
                                        memory = last_containder_hardware$memory,
                                        task_count=1,
                                        verbose=verbose))
    }
    end_time <- Sys.time()
    started_number <- started_number+required_number
    if(started_number!=n_containers){
      sleep_time <- 11-(end_time-start_time)
      if(sleep_time>0)
        Sys.sleep(sleep_time)
    }
  }
  workers_in_container <- rep(worker_per_container, n_containers)
  if(worker_in_last_container!=0)
    workers_in_container[n_containers] <- worker_in_last_container
  data.frame(task_id = task_ids,
             n_workers = workers_in_container)
}


## CPU and memory must be valid
ecs_run_task_internal<-function(config, cpu, memory, task_count = 1, verbose= TRUE){
  stopifnot(task_count<=10)
  request <- fromJSON(file="R/json_config/run-task.json",simplify=FALSE)
  request$cluster <- config$cluster_name
  request$overrides$cpu<- as.character(cpu)
  request$overrides$memory<- as.character(memory)
  request$taskDefinition <- config$task_definition_name
  request$count <- task_count
  request$overrides$containerOverrides[[1]]$environment[[1]]$value <- get_ssh_public_key_value()
  request$networkConfiguration$awsvpcConfiguration$securityGroups[[1]]<-config$security_group_id
  request$networkConfiguration$awsvpcConfiguration$subnets[[1]]<-config$subnet_id
  #existing_rule <- ecs_list_security_rule()
  response <- ecs_POST("RunTask", request = request)
  ids <- vapply(response$tasks,function(x)x$taskArn, character(1))
  ids
}


ecs_list_tasks<-function(cluster_name,
                         status = c("RUNNING", "PENDING","STOPPED"),
                         task_family = NULL){
  status <- match.arg(status)
  request <- list()
  request$cluster <- cluster_name
  request$desiredStatus <- status
  if(!is.null(task_family)){
    request$family <- task_family
  }
  target <- "ListTasks"
  response <- ecs_POST(target, request = request)

  arns <- unlist(response$taskArns)
  arns
}
ecs_stop_tasks<-function(cluster_name, task_ids){
  target <- "StopTask"
  request <- list()
  request$cluster <- cluster_name
  for(id in task_ids){
    request$task <- id
    response <- ecs_POST(target, request = request)
  }
}



ecs_get_task_details <- function(cluster_name, task_ids, get_ip = FALSE){
  result <- c()
  described_task_number <- 0
  while(described_task_number!=length(task_ids)){
    current_task_number <- min(100,length(task_ids)-described_task_number)
    current_task_id <- task_ids[(described_task_number+1):(described_task_number+current_task_number)]
    result<-rbind(result,
                  ecs_get_task_details_internal(cluster_name, current_task_id,get_ip=get_ip))
    described_task_number <- described_task_number + current_task_number
  }
  result
}

ecs_get_task_details_internal<-function(cluster_name, task_ids, get_ip = FALSE){
  target <- "DescribeTasks"
  request <- list(
    cluster = cluster_name,
    tasks=as.list(task_ids)
  )
  response <- ecs_POST(target, request = request)

  task_ids <- vapply(response$tasks,function(x)x$taskArn, character(1))
  status <- vapply(response$tasks,function(x)x$lastStatus, character(1))
  if(get_ip){
    ENIs <- vapply(response$tasks,get_instance_ENI,character(1))
    idx <- which(ENIs!="")
    IPs <- rep("", length(task_ids))
    IPs[idx] <- get_instance_ip(ENIs[idx])
    data.frame(task_id = task_ids, status = status, IP = IPs)
  }else{
    data.frame(task_id = task_ids, status = status)
  }
}
get_instance_ENI<-function(x){
  eni<-""
  for(i in x$attachments){
    if(i$type=="ElasticNetworkInterface"&&i$status=="ATTACHED"){
      eni<- get_tag_value(i$details,"name","value","networkInterfaceId")
    }
  }
  eni
}

get_instance_ip <- function(ENIs){
  target <- "DescribeNetworkInterfaces"
  query <- list()
  for(i in seq_along(ENIs)){
    query[[paste0("NetworkInterfaceId.",i)]] <- ENIs[i]
  }
  response <- ec2_GET(target, query)
  IPs<- vapply(response$networkInterfaceSet, function(x)x$association$publicIp[[1]], character(1))
  IPs
}

# task_ids <- task_list$task_id
wait_tasks_to_run<-function(cluster_name, task_ids, progress_bar = FALSE){
  n_instance <- length(task_ids)
  instance_indicator <- rep(FALSE, n_instance)
  if(progress_bar){
    pb <- txtProgressBar(min=0,max = length(task_ids), style=3)
  }
  while(sum(instance_indicator)<n_instance){
    pending_idx <- which(!instance_indicator)
    task_details <- ecs_get_task_details(cluster_name, task_ids[pending_idx])
    if(nrow(task_details)!=length(pending_idx)){
      warning("Requestion task number and result does not match")
      break
    }
    if(sum(task_details$status=="STOPPED")>0){
      warning("The task has been stopped")
      break
    }
    running_idx <- pending_idx[task_details$status=="RUNNING"]
    instance_indicator[running_idx] <- TRUE

    if(progress_bar){
      setTxtProgressBar(pb, sum(instance_indicator))
    }
  }
  if(progress_bar){
    close(pb)
  }
  sum(instance_indicator)== n_instance
}
