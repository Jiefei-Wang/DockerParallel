# cluster$startCluster()
# cluster$startServer()
# cluster$startWorkers(workerNum)
# cluster$stopCluster()
# cluster$stopServer()
# cluster$stopWorkers(workerNum)
# cluster$getWorkerNumber()
# cluster$status()
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
                          verbose = FALSE){
    if(!is.null(cloudRuntime$clusterIp)){
        cloudConfig$serverContainer = NULL
    }
    .DockerCluster(cloudProvider=cloudProvider,
                   cloudConfig=cloudConfig,
                   cloudRuntime=cloudRuntime,
                   verbose= verbose)
}


#' @export
setMethod(f = "$",signature = "ECSConfig",
          definition = function(x, name){
              if(name != "more"){
                  x[[name, exact = FALSE]]
              }else{
                  function()showDetails(x)
              }
          })
