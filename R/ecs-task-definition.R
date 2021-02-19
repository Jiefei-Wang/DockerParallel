CreateTaskDefinition <- function(taskName, container){
    request <- ecs_get_json("task-definition.json")
    request$family <- taskName
    request$cpu <- as.character(getECSCPU(container))
    request$memory <- as.character(getECSMem(container))
    request$containerDefinitions[[1]]$image <- container@image
    response <- ecs_register_task_definition(request)
    response$taskDefinition$taskDefinitionArn
}

deleteTaskDefinition <- function(task_name){
    request <- list(taskDefinition = task_name)
    response <- ecs_deregister_task_definition(request)
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
    serverTaskDefName <- getECSData(x, "serverTaskDefName")
    if(is.null(serverTaskDefName)){
        if(is.empty(x@serverTaskDefName)){
            serverTaskDefName <-ECSDefault$serverTaskDefName
        }else{
            serverTaskDefName <- x@serverTaskDefName
        }
        serverTaskDefName <- CreateTaskDefinition(
            serverTaskDefName,
            x@server
        )
        setECSData(x, "serverTaskDefName", serverTaskDefName)
    }

    workerTaskDefName <- getECSData(x, "workerTaskDefName")
    if(is.null(workerTaskDefName)){
        if(is.empty(x@workerTaskDefName)){
            workerTaskDefName <-ECSDefault$workerTaskDefName
        }else{
            workerTaskDefName <- x@workerTaskDefName
        }
        workerTaskDefName <- CreateTaskDefinition(
            workerTaskDefName,
            x@worker
        )
        setECSData(x, "workerTaskDefName", workerTaskDefName)
    }
}

