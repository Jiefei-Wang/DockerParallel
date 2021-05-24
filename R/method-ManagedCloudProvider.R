setMethod("setDockerWorkerNumber", "ManagedCloudProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose)
          {
              verbosePrint(verbose, "Setting worker number to ", workerNumber)
              if(cluster$isServerRunning()){
                  ## set the expected worker number
                  removeDiedWorkers(cluster)

                  workerOffset <- workerNumber - getManagedWorkerNumber(provider)
                  if(workerOffset > 0){
                      addManagedWorkersInternal(cluster = cluster,
                                                container = container,
                                                hardware = hardware,
                                                workerNumber = workerOffset)
                  }
                  if(workerOffset < 0){
                      removeManagedWorkersInternal(cluster = cluster,
                                                   workerNumber = abs(workerOffset))
                  }
              }
          })
setMethod("getDockerWorkerNumbers", "ManagedCloudProvider", function(provider, cluster, verbose){
    if(cluster$isServerRunning()){
        removeDiedWorkers(cluster)
        runningWorkers <- getManagedWorkerNumber(provider)
        list(initializing = 0L, running = runningWorkers)
    }else{
        list(initializing = 0L, running = 0L)
    }

})
