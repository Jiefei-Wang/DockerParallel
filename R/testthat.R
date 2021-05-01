expectServerNotRunning <- function(cluster){
    testthat::expect_true(is.null(.getServerHandle(cluster)))
    testthat::expect_true(is.null(.getServerPublicIp(cluster)))
    testthat::expect_true(is.null(.getServerPrivateIp(cluster)))
}
expectServerRunning <- function(cluster){
    testthat::expect_true(!is.null(.getServerHandle(cluster)))
    testthat::expect_true(!is.null(.getServerPublicIp(cluster)))
    testthat::expect_true(!is.null(.getServerPrivateIp(cluster)))
}
expectWorkerNotRunning <- function(cluster){
    testthat::expect_equal(cluster$getWorkerNumber(), 0)
    testthat::expect_equal(length(.getWorkerHandles(cluster)), 0)
}
expectWorkerRunning <- function(cluster, expectNum, workerPerContainer){
    testthat::expect_equal(cluster$getWorkerNumber(), expectNum)
    testthat::expect_equal(cluster$getExpectedWorkerNumber(), expectNum)
    testthat::expect_equal(length(.getWorkerHandles(cluster)), expectNum)
    testthat::expect_equal(length(unique(.getWorkerHandles(cluster))),
                           ceiling(expectNum/workerPerContainer))
}

#' The general testthat function for testing the cluster
#'
#' The general testthat function for testing the cluster. The function is
#' generally called by the cloud provider to test the functions in the provider.
#' if `testReconnect` is `TRUE`,
#' The provider must define `reconnectDockerCluster` for making the test function work.
#'
#' @param cloudProvider The CloudProvider
#' @param workerContainer The workerContainer
#' @param workerNumber The number of workers used in the unit test
#' @param testReconnect Logical, whether to test the reconnect feature
#'
#' @return No return value
#' @export
generalDockerClusterTest <- function(cloudProvider, workerContainer, workerNumber = 3L,
                                     testReconnect = TRUE){
    workerPerContainer <- workerContainer$maxWorkerNum
    testthat::expect_error(
        cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                     workerContainer = workerContainer,
                                     workerNumber = workerNumber,
                                     workerCpu = 256,
                                     workerMemory = 512)
        ,NA)


    testthat::test_that("basic test",{
        testthat::expect_equal(cluster$getWorkerNumber(), 0)
        testthat::expect_equal(cluster$getExpectedWorkerNumber(), workerNumber)

        ## Server info
        testthat::expect_true(!cluster$isServerRunning())
        expectServerNotRunning(cluster)

        ## Worker info
        expectWorkerNotRunning(cluster)
        testthat::expect_equal(.getWorkerHardware(cluster)@cpu, 256)
        testthat::expect_equal(.getWorkerHardware(cluster)@memory, 512)
    })


    testthat::test_that("start server",{
        testthat::expect_error(cluster$startServer(),NA)
        testthat::expect_true(cluster$isServerRunning())
        expectServerRunning(cluster)
    })

    testthat::test_that("start workers",{
        testthat::expect_error(cluster$addWorkers(0),NA)
        expectWorkerRunning(cluster, workerNumber, workerPerContainer)
    })

    testthat::test_that("register backend",{
        testthat::expect_error(cluster$registerBackend(), NA)
    })

    testthat::test_that("stop workers",{
        testthat::expect_error(cluster$setWorkerNumber(0),NA)
        testthat::expect_equal(cluster$getExpectedWorkerNumber(), 0)
        expectWorkerNotRunning(cluster)
    })

    testthat::test_that("stop server",{
        testthat::expect_error(cluster$stopServer(),NA)
        expectServerNotRunning(cluster)
    })

    if(testReconnect){
        testthat::test_that("stop cluster on exit",{
            testthat::expect_error(
                cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                             workerContainer = workerContainer,
                                             workerNumber = 1,
                                             workerCpu = 256,
                                             workerMemory = 512)
                ,NA)
            ## Start cluster
            testthat::expect_error(cluster$startCluster(),NA)
            expectServerRunning(cluster)
            expectWorkerRunning(cluster, 1, workerPerContainer)

            ## stop on exit
            cluster$stopClusterOnExit <- TRUE
            rm(cluster)
            gc()

            ## Check if the cluster has been stopped
            testthat::expect_error(
            cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                         workerContainer = workerContainer,
                                         workerNumber = 1,
                                         workerCpu = 256,
                                         workerMemory = 512)
            ,NA)
            testthat::expect_error(cluster$reconnect())

            ## Start the cluster again
            testthat::expect_error(cluster$startCluster(),NA)
            expectServerRunning(cluster)
            expectWorkerRunning(cluster, 1, workerPerContainer)
            cluster$stopClusterOnExit <- FALSE
            rm(cluster)
            gc()

            ## Check if the cluster is still running
            testthat::expect_error(
            cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                          workerContainer = workerContainer,
                                          workerNumber = 1,
                                          workerCpu = 256,
                                          workerMemory = 512)
            ,NA)
            testthat::expect_error(cluster$reconnect(),NA)
            expectServerRunning(cluster)
            expectWorkerRunning(cluster, 1, workerPerContainer)

            ## Stop the cluster
            testthat::expect_error(cluster$stopCluster(),NA)
            testthat::expect_equal(cluster$getExpectedWorkerNumber(), 1)
            expectServerNotRunning(cluster)
            expectWorkerNotRunning(cluster)
        })
    }

}
