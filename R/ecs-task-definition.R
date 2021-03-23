CreateTaskDefinition <- function(taskName, image, cpu = 256, memory = 512){
    request <- ecs_get_json("task-definition.json")
    containerDefinitions <- list(list(
        name = "dockerParallel",
        image=image,
        essential = TRUE
    ))
    response <- ecs_register_task_definition(family=taskName,
                                             cpu = as.character(cpu),
                                             memory = as.character(memory),
                                             containerDefinitions = containerDefinitions,
                                             networkMode = "awsvpc",
                                             requiresCompatibilities = "FARGATE"
    )
    response$taskDefinitionArn
}

deleteTaskDefinition <- function(taskName, version = NULL){
    if(!is.null(version)){
        taskName <- paste0(taskName,":", version)
    }
    response <- ecs_deregister_task_definition(taskDefinition = taskName)
    response
}

describeTaskDefinition <- function(taskName, version = NULL){
    if(!is.null(version)){
        taskName <- paste0(taskName,":", version)
    }
    response <- ecs_describe_task_definition(taskDefinition = taskName)
    response
}

listTaskDefinitions<-function(taskName = NULL){
    defArns <- ecs_list_task_definitions(familyPrefix = taskName)
    defInfo <- ecs_get_resource_names(defArns)
    defInfo <- strsplit(defInfo,":", fixed = TRUE)
    defNames <- vapply(defInfo, function(x)x[1], character(1))
    defVersions <- vapply(defInfo, function(x)as.numeric(x[2]),numeric(1))
    data.frame(name = defNames, version = defVersions)
}

configTaskDefinition <- function(x, workerImage, serverImage = NULL){
    if(!is.null(serverImage)){
        if(!x$serverTaskDefNameVerified){
            configTaskDefinitionInternal(x$serverTaskDefName, serverImage)
            x$serverTaskDefNameVerified <- TRUE
        }
    }
    if(!x$workerTaskDefNameVerified){
        configTaskDefinitionInternal(x$workerTaskDefName, workerImage)
        x$workerTaskDefNameVerified <- TRUE
    }
}

configTaskDefinitionInternal <- function(taskDefName, image){
    definitionList <- listTaskDefinitions(taskDefName)
    if(nrow(definitionList)!=0){
        version <- max(definitionList$version)
        taskDescription <- describeTaskDefinition(taskDefName,version = version)
        taskImage <- taskDescription$containerDefinitions[[1]]$image
    }else{
        taskImage <- "NULL"
    }
    if(taskImage != image){
        taskDefName <- CreateTaskDefinition(
            taskName = taskDefName,
            image = image
        )
    }
}

