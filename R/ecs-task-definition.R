CreateTaskDefinition <- function(taskName, image, cpu = 256, memory = 512){
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
    defInfo <- ECSGetResourceNames(defArns)
    defInfo <- strsplit(defInfo,":", fixed = TRUE)
    defNames <- vapply(defInfo, function(x)x[1], character(1))
    defVersions <- vapply(defInfo, function(x)as.numeric(x[2]),numeric(1))
    data.frame(name = defNames, version = defVersions)
}

configTaskDefinition <- function(x){
    if(!x$taskDefNameVerified){
        definitionList <- listTaskDefinitions(taskName = x$taskDefName)
        if(nrow(definitionList)==0){
            taskDefName <- CreateTaskDefinition(
                taskName = x$taskDefName,
                image = "waiting-for-filling"
            )
        }
        x$taskDefNameVerified <- TRUE
    }
}
