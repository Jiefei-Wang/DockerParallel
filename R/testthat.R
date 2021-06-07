expectWorkerNotRunning <- function(cluster){
    workerNumbers <- cluster$getWorkerNumbers()
    testthat::expect_equal(workerNumbers$running, 0)
    testthat::expect_equal(workerNumbers$initializing, 0)
}
expectWorkerRunning <- function(cluster, expectNum){
    workerNumbers <- cluster$getWorkerNumbers()
    testthat::expect_equal(workerNumbers$running + workerNumbers$initializing, expectNum)
}

#' The general testthat function for testing the cluster
#'
#' The general testthat function for testing the cluster. The function should
#' be called by the cloud provider to test the functions in the provider.
#' if `testReconnect` is `TRUE`,
#' The provider must define `reconnectDockerCluster` for making the test function work.
#'
#' @param cloudProvider The CloudProvider
#' @param workerContainer The workerContainer
#' @param workerNumber Integer(1), The number of workers used in the unit test
#' @param testReconnect Logical(1), whether to test the reconnect feature
#' @param ... Additional parameters passed to `makeDockerCluster`
#'
#' @return No return value
#' @export
generalDockerClusterTest <- function(cloudProvider,
                                     workerContainer,
                                     workerNumber = 5L,
                                     testReconnect = TRUE,
                                     ...){
    workerPerContainer <- workerContainer$maxWorkerNum
    testthat::expect_error(
        cluster <- makeDockerCluster(cloudProvider = cloudProvider$copy(),
                                     workerContainer = workerContainer$copy(),
                                     workerNumber = 0L,
                                     workerCpu = 256,
                                     workerMemory = 512,
                                     ...)
        ,NA)


    ## worker number
    expectWorkerNotRunning(cluster)

    ## Server info
    testthat::expect_false(cluster$isServerRunning())

    ## Worker info
    expectWorkerNotRunning(cluster)
    testthat::expect_equal(.getWorkerHardware(cluster)@cpu, 256)
    testthat::expect_equal(.getWorkerHardware(cluster)@memory, 512)



    ## start server
    testthat::expect_error(cluster$startServer(),NA)
    testthat::expect_true(cluster$isServerRunning())

    ## start workers
    testthat::expect_error(cluster$setWorkerNumber(workerNumber),NA)
    expectWorkerRunning(cluster, workerNumber)


    ## register backend"
    testthat::expect_error(cluster$registerBackend(), NA)


    ## stop workers"
    testthat::expect_error(cluster$setWorkerNumber(0),NA)
    expectWorkerNotRunning(cluster)

    ## stop server
    testthat::expect_error(cluster$stopServer(),NA)
    testthat::expect_false(cluster$isServerRunning())
    rm(cluster)
    gc()

    if(testReconnect){
        testthat::expect_error(
            cluster <- makeDockerCluster(cloudProvider = cloudProvider$copy(),
                                         workerContainer = workerContainer$copy(),
                                         workerNumber = 1,
                                         workerCpu = 256,
                                         workerMemory = 512,
                                         ...)
            ,NA)
        ## Start cluster
        testthat::expect_error(cluster$startCluster(),NA)
        testthat::expect_true(cluster$isServerRunning())
        expectWorkerRunning(cluster, 1)

        ## stop on exit
        cluster$stopClusterOnExit <- TRUE
        rm(cluster)
        gc()

        ## Check if the cluster has been stopped
        testthat::expect_error(
            cluster <- makeDockerCluster(cloudProvider = cloudProvider$copy(),
                                         workerContainer = workerContainer$copy(),
                                         workerNumber = 1,
                                         workerCpu = 256,
                                         workerMemory = 512,
                                         ...)
            ,NA)
        testthat::expect_false(cluster$clusterExists())

        ## Start the cluster again
        testthat::expect_error(cluster$startCluster(),NA)
        testthat::expect_true(cluster$isServerRunning())
        expectWorkerRunning(cluster, 1)
        cluster$stopClusterOnExit <- FALSE
        rm(cluster)
        gc()

        ## Check if the cluster is still running
        testthat::expect_error(
            cluster <- makeDockerCluster(cloudProvider = cloudProvider$copy(),
                                         workerContainer = workerContainer$copy(),
                                         workerNumber = 1,
                                         workerCpu = 256,
                                         workerMemory = 512,
                                         ...)
            ,NA)
        testthat::expect_true(cluster$clusterExists())
        testthat::expect_error(cluster$reconnect(),NA)
        testthat::expect_true(cluster$isServerRunning())
        expectWorkerRunning(cluster, 1)

        ## Stop the cluster
        testthat::expect_error(cluster$stopCluster(),NA)
        testthat::expect_false(cluster$isServerRunning())
        expectWorkerNotRunning(cluster)

        rm(cluster)
        gc()
    }
}
