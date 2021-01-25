aws_configure <- new.env()
aws_configure$cluster_name <- "R-worker-cluster"
aws_configure$task_definition_name <- "R-worker-task-definition"
aws_configure$task_name <- "R-worker-task"
aws_configure$security_group_name <- "R-worker-security-group"
aws_configure$vpc_id <- ""
aws_configure$subnet_id <- ""
aws_configure$security_group_id <- ""



get_aws_configure<-function(){
  aws_configure$cluster_name <- "R-worker-cluster"
  aws_configure$task_definition_name <- "R-worker-task-definition"
  aws_configure$task_name <- "R-worker-task"
  aws_configure$security_group_name <- "R-worker-security-group"
  aws_configure$vpc_id <- "auto"
  aws_configure$subnet_id <- "auto"
  aws_configure$security_group_id <- "auto"
}


