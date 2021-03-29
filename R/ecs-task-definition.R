CreateTaskDefinition <- function(taskName, name, image, cpu = 256, memory = 512){
    containerDefinitions <- list(list(
        name = name,
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

configTaskDefinition <- function(x, cloudConfig){

    configTaskDefinitionInternal(x,
                                 "serverTaskDefName",
                                 cloudConfig$serverContainer)
    configTaskDefinitionInternal(x,
                                 "workerTaskDefName",
                                 cloudConfig$workerContainer)

}

configTaskDefinitionInternal <- function(x, taskNameSlot, container){
    taskDefName <- x$field(taskNameSlot)
    verifySlot <- paste0(taskNameSlot, "Verified")
    taskDefNameVerified <- x$field(verifySlot)
    if(!taskDefNameVerified){
        definitionList <- listTaskDefinitions(taskName = taskDefName)
        needDef <- nrow(definitionList)==0
        if(!needDef){
            idx <- which.max(definitionList$version)
            taskInfo <- describeTaskDefinition(taskDefName, definitionList$version[idx])

            nameCheck <- identical(
                taskInfo$containerDefinitions[[1]]$name,
                container@name
                )
            imageCheck <- identical(
                taskInfo$containerDefinitions[[1]]$image,
                container@image)

            needDef <- !all(nameCheck, imageCheck)
        }

        if(needDef){
            CreateTaskDefinition(
                taskName = taskDefName,
                name = container@name,
                image =  container@image
            )
        }
        x$field(verifySlot, TRUE)
    }
}

