CreateTaskDefinition <- function(taskName, CPU = 256, memory = 512, image){
    request <- ecs_get_json("task-definition.json")
    request$family <- taskName
    request$cpu <- as.character(CPU)
    request$memory <- as.character(memory)
    request$containerDefinitions[[1]]$image <- image
    response <- ecs_register_task_definition(request)
    response$taskDefinition$taskDefinitionArn
}

deleteTaskDefinition <- function(taskName){
    request <- list(taskDefinition = taskName)
    response <- ecs_deregister_task_definition(request)
    response
}

describeTaskDefinition <- function(taskName){
    response <- ecs_describe_task_definition(list(taskDefinition = taskName))
    response
}

listTaskDefinitions<-function(taskName = NULL){
    request <- list()
    if(!is.null(taskName)){
        request$familyPrefix <- taskName
    }
    defArns <- ecs_list_task_definitions(request)
    defInfo <- get_resource_names(defArns)
    defInfo <- strsplit(defInfo,":", fixed = TRUE)
    defNames <- vapply(defInfo, function(x)x[1], character(1))
    defVersions <- vapply(defInfo, function(x)as.numeric(x[2]),numeric(1))
    data.frame(name = defNames, version = defVersions)
}

configTaskDefinition <- function(x){
    if(is(x@server, "Container")){
        configTaskDefinitionInternal(x, "serverTaskDefName", x@server@image)
    }
    configTaskDefinitionInternal(x, "workerTaskDefName", x@worker@image)
}

configTaskDefinitionInternal <- function(x, taskAttrName, image){
    taskDefName <- getECSCloudData(x, taskAttrName)
    if(is.empty(taskDefName)){
        if(is.empty(do.call("@",list(x,taskAttrName)))){
            taskDefName <-ECSDefault[[taskAttrName]]
        }else{
            taskDefName <- do.call("@",list(x, taskAttrName))
        }
        definitionList <- listTaskDefinitions(taskDefName)
        if(nrow(definitionList)!=0){
            taskDescription <- describeTaskDefinition(taskDefName)
            taskImage <- taskDescription$taskDefinition$containerDefinitions[[1]]$image
        }else{
            taskImage <- "NULL"
        }

        if(taskImage != image){
            taskDefName <- CreateTaskDefinition(
                taskName = taskDefName,
                image = image
            )
        }
        setECSCloudData(x, taskAttrName, taskDefName)
    }
}

