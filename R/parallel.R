makeNodePSOCKEX <- function(...){
  args <- list(...)
  args$master <- "127.0.0.1"
  args$user <- "root"
  args$rshopts<- c("-i", get_ssh_private_key())
  do.call(makeNodePSOCK,args = args)
}

makeAWSCloudCluster <- function(workers=1, verbose = FALSE){
  task_ids <- aws_run_task(count = workers, verbose = verbose)
  verbose_print(verbose,"Waiting for the initialization of the workers")
  success <- wait_tasks_to_run(task_ids, progress_bar = verbose)
  if(success){
    ip<- aws_get_task_details(task_ids,get_ip = TRUE)$IP
    cl <- makeClusterPSOCK(ip, makeNode= makeNodePSOCKEX,verbose = verbose)
    cl
  }else{
    message("Fail to start the workers")
    aws_stop_tasks(task_ids)
    NULL
  }
}
