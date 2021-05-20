setMethod("setDockerWorkerNumber", "ManagedCloudProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose)
          {
              verbosePrint(verbose, "Setting worker number to ", workerNumber)
              ## set the expected worker number
              removeDiedWorkers(cluster)

              workerOffset <- workerNumber - getManagedWorkerNumber(provider)
              if(workerOffset > 0){
                  addWorkersInternal(cluster = cluster,
                                     container = container,
                                     hardware = hardware,
                                     workerNumber = workerOffset)
              }
              if(workerOffset < 0){
                  removeWorkersInternal(cluster = cluster,
                                        workerNumber = abs(workerOffset))
              }
          })
