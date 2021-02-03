makeNodePSOCKEX <- function(...){
  args <- list(...)
  args$master <- "127.0.0.1"
  args$user <- "root"
  args$rshopts<- c("-i", get_ssh_private_key())
  do.call(makeNodePSOCK,args = args)
}

## wait_tasks_to_run(a$task_id, progress_bar = TRUE)
makeAWSCloudCluster <- function(config, workers=1, verbose = FALSE){
  task_list <- ecs_run_task(config, n_workers  = workers, verbose = verbose)
  verbose_print(verbose,"Waiting for the initialization of the workers")
  success <- wait_tasks_to_run(config$cluster_name, task_list$task_id,
                               progress_bar = verbose)
  if(success){
    ip_list <- ecs_get_task_details(config$cluster_name,task_list$task_id,get_ip = TRUE)$IP
    IPs <- unlist(lapply(seq_along(ip_list),
                         function(i) rep(ip_list[i], task_list$n_workers[i])))
    cl <- makeClusterPSOCK(IPs, makeNode= makeNodePSOCKEX,verbose = verbose)
    cl
  }else{
    message("Fail to start the workers")
    ecs_stop_tasks(task_ids)
    NULL
  }
}
