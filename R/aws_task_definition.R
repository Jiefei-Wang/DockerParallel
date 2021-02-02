aws_create_task_definition <- function(task_name, image){
  request <- aws_get_json("task-definition.json")
  request$family <- task_name
  request$cpu <- "256"
  request$memory <- "512"
  request$containerDefinitions[[1]]$image <- image
  response <- ecs_POST("RegisterTaskDefinition", request = request)
  response$taskDefinition$taskDefinitionArn
}

aws_delete_task_definition <- function(task_name){
  request <- list(taskDefinition=task_name)
  response <- ecs_POST("DeregisterTaskDefinition", request = request)
  response
}

aws_list_task_definitions<-function(task_name = NULL){
  request <- list()
  if(!is.null(task_name)){
    request$familyPrefix <- task_name
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



aws_config_task_definition <- function(config){
  task_definition_name <- config$task_definition_name
  if(!is_valid(config, task_definition_name)){
    task_definition_list <- aws_list_task_definitions(task_definition_name)
    if(nrow(task_definition_list)==0){
      aws_create_task_definition(task_definition_name,
                                 image = config$image)
    }
    # task_definition_list <- aws_list_task_definitions(task_definition_name)
    # idx <- which.max(task_definition_list$version)
    # config$task_definition_id <- paste0(
    #   task_definition_list$name[idx],
    #   task_definition_list$version[idx]
    # )

    set_valid(config, task_definition_name)
  }
  task_definition_name
}
