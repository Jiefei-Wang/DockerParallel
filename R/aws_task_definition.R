aws_create_task_definition <- function(task_name, image, cpu, memory){
  request <- aws_get_json("task-definition.json")
  request$family <- task_name
  request$cpu <- as.character(cpu)
  request$memory <- as.character(memory)
  request$containerDefinitions[[1]]$image <- image
  response <- ecs_POST("RegisterTaskDefinition", request = request)
  response
}

aws_delete_task_definition <- function(task_name){
  request <- list(taskDefinition=task_name)
  response <- ecs_POST("DeregisterTaskDefinition", request = request)
  response
}

aws_list_task_definitions<-function(family_prefix = NULL){
  request <- list()
  if(!is.null(family_prefix)){
    request$familyPrefix <- family_prefix
  }
  target <- "ListTaskDefinitions"
  response <- ecs_POST(target, request = request)
  definition_list <- unlist(response$taskDefinitionArns)
  while(!is.null(response$nextToken)){
    request <- list(nextToken = response$nextToken)
    response <- ecs_POST(target, request = request)
    definition_list <- c(definition_list, unlist(response$taskDefinitionArns))
  }
  task_definition_info <- get_resource_names(definition_list)
  task_definition_info <- strsplit(task_definition_info,":", fixed = TRUE)
  task_definition_names <- vapply(task_definition_info, function(x)x[1], character(1))
  task_definition_versions <- vapply(task_definition_info, function(x)as.numeric(x[2]),numeric(1))
  data.frame(name = task_definition_names, version = task_definition_versions)
}

aws_config_task_definition <- function(config, n_workers = 1){
  task_definition_prefix <- config$task_definition_prefix
  cpu <- as.numeric(config$cpu)*n_workers
  memory <- as.numeric(config$memory)*n_workers
  resources <-get_valid_fargate_cpu_memory(cpu, memory)
  task_definition_name <- paste0(task_definition_prefix,
                                 "_",resources$cpu,
                                 "_",resources$memory,
                                 "_",gsub("[^a-zA-Z]+","",config$image))
  if(!is_valid(config, task_definition_name)){
    task_definition_list <- aws_list_task_definitions(task_definition_prefix)

    if(!any(task_definition_list$name==task_definition_name)){
      aws_create_task_definition(task_definition_name,
                                 image = config$image,
                                 cpu = cpu, memory=memory)
    }
    set_valid(config, task_definition_name)
  }
  task_definition_name
}
