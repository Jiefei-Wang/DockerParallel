clusterMethods <- c(
    "startCluster",
    "startServer",
    "startWorkers",
    "stopCluster",
    "stopServer",
    "stopWorkers",
    "getWorkerNumber",
    "status"
)

dockerCluster <- function(cloudProvider = ECSProvider(),
                          cloudConfig = CloudConfig(),
                          cloudRuntime = CloudRuntime(),
                          verbose = 2){
    if(!is.null(cloudRuntime$clusterIp)){
        cloudConfig$serverContainer = NULL
    }
    .DockerCluster(cloudProvider=cloudProvider,
                   cloudConfig=cloudConfig,
                   cloudRuntime=cloudRuntime,
                   verbose= as.integer(verbose))
}

#' @export
setMethod(f = "names",signature = "DockerCluster",
          definition = function(x){
              clusterMethods
          })
#' @export
setMethod(f = "$",signature = "DockerCluster",
          definition = function(x, name){
              if(name%in%clusterMethods){
                    func <- get(name)
                    newFunc <- createTempFunction(name, func)
              }
              parent.env(environment(newFunc)) <- environment()
              newFunc
          })

createTempFunction <- function(name, func){
    funcFormals <- formals(func)
    funcFormals <- funcFormals[names(funcFormals)!="cluster"]
    parameters <- ""
    if(length(funcFormals)!=0){
        parameters <- paste0(
            paste0(names(funcFormals), "=", names(funcFormals)),collapse = ",")
        parameters <- paste0(",", parameters)
    }
    functionTemplate <- "
                              function(){
                        functionName(cluster = x parameters)
                    }
                              "
    functionTemplate <- sub("functionName",name, functionTemplate)
    functionTemplate <- sub("parameters",parameters, functionTemplate)
    newFuncExpression <-
        parse(text = functionTemplate
        )[[1]]

    newFunc <- eval(newFuncExpression)
    formals(newFunc) <- funcFormals
    newFunc
}
