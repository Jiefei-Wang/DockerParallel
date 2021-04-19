expectServerNotRunning <- function(cluster){
    expect_true(is.null(.getServerHandle(cluster)))
    expect_true(is.null(.getServerPublicIp(cluster)))
    expect_true(is.null(.getServerPrivateIp(cluster)))
}
expectServerRunning <- function(cluster){
    expect_true(!is.null(.getServerHandle(cluster)))
    expect_true(!is.null(.getServerPublicIp(cluster)))
    expect_true(!is.null(.getServerPrivateIp(cluster)))
}
expectWorkerNotRunning <- function(cluster){
    expect_equal(cluster$getWorkerNumber(), 0)
    expect_equal(length(.getWorkerHandles(cluster)), 0)
}
expectWorkerRunning <- function(cluster, expectNum, workerPerContainer){
    expect_equal(cluster$getWorkerNumber(), expectNum)
    expect_equal(cluster$getExpectedWorkerNumber(), expectNum)
    expect_equal(length(.getWorkerHandles(cluster)), expectNum)
    expect_equal(length(unique(.getWorkerHandles(cluster))),
                 ceiling(expectNum/workerPerContainer))
}
generalDockerClusterTest <- function(cloudProvider, workerContainer, workerNumber = 3L){
    workerNumber <- 3L
    workerPerContainer <- workerContainer$maxWorkerNum
    cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                 workerContainer = workerContainer,
                                 workerNumber = workerNumber,
                                 workerCpu = 256,
                                 workerMemory = 512)




    test_that("basic test",{
        expect_equal(cluster$getWorkerNumber(), 0)
        expect_equal(cluster$getExpectedWorkerNumber(), workerNumber)

        ## Server info
        expect_true(!cluster$isServerRunning())
        expectServerNotRunning(cluster)

        ## Worker info
        expectWorkerNotRunning(cluster)
        expect_equal(.getWorkerHardware(cluster)@cpu, 256)
        expect_equal(.getWorkerHardware(cluster)@memory, 512)
    })


    test_that("start server",{
        cluster$startServer()
        expect_true(cluster$isServerRunning())
        expectServerRunning(cluster)
    })

    test_that("start workers",{
        cluster$addWorkers(0)
        expectWorkerRunning(cluster, workerNumber, workerPerContainer)
    })

    test_that("register backend",{
        expect_error(cluster$registerBackend(), NA)
    })

    test_that("stop workers",{
        cluster$setWorkerNumber(0)
        expect_equal(cluster$getExpectedWorkerNumber(), 0)
        expectWorkerNotRunning(cluster)
    })

    test_that("stop server",{
        cluster$stopServer()
        expectServerNotRunning(cluster)
    })


    test_that("stop cluster on exit",{
        cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                     workerContainer = workerContainer,
                                     workerNumber = 1,
                                     workerCpu = 256,
                                     workerMemory = 512)
        ## Start cluster
        cluster$startCluster()
        expectServerRunning(cluster)
        expectWorkerRunning(cluster, 1, workerPerContainer)

        ## stop on exit
        cluster$stopClusterOnExit <- TRUE
        rm(cluster)
        gc()

        ## Check if the cluster has been stopped
        cluster <- makeDockerCluster(cloudProvider = cloudProvider,
                                     workerContainer = workerContainer,
                                     workerNumber = 1,
                                     workerCpu = 256,
                                     workerMemory = 512)
        expect_error(cluster$reconnect())

        ## Start the cluster again
        cluster$startCluster()
        expectServerRunning(cluster)
        expectWorkerRunning(cluster, 1, workerPerContainer)
        cluster$stopClusterOnExit <- FALSE
        rm(cluster)
        gc()

        ## Check if the cluster is still running
        cluster <<- makeDockerCluster(cloudProvider = cloudProvider,
                                      workerContainer = workerContainer,
                                      workerNumber = 1,
                                      workerCpu = 256,
                                      workerMemory = 512)
        cluster$reconnect()
        expectServerRunning(cluster)
        expectWorkerRunning(cluster, 1, workerPerContainer)

        ## Stop the cluster
        cluster$stopCluster()
        expect_equal(cluster$getExpectedWorkerNumber(), 0)
        expectServerNotRunning(cluster)
        expectWorkerNotRunning(cluster)
    })
}
