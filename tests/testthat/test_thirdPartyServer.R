resetDummyProvider()
cluster <- NULL
serverData <- NULL
verbose <- 0

test_that("DockerCluster constructor", {
    expect_error(
        provider <- DummyProvider()
        ,NA)
    expect_error(
        container <- DummyWorkerContainer()
        ,NA)
    expect_error(
        serverData <<- CloudPrivateServer(
            publicIp = "192.168.1.1", publicPort = 123,
            privateIp = "127.0.0.1", privatePort = 456,
            serverClientSameLAN = TRUE)
        ,NA)
    expect_error(
        cluster <<- makeDockerCluster(
            cloudProvider = provider,
            workerContainer = container,
            privateServerData = serverData,
            verbose = verbose)
        ,NA)
})
test_that("DockerCluster server status", {
    expect_true(cluster$isServerRunning())
    expect_true(cluster$clusterExists())
})

test_that("DockerCluster set worker number", {
    expect_error(
        cluster$setWorkerNumber(10),
        NA
    )
    expect_identical(cluster$getWorkerNumbers(),
                     list(initializing = 0L, running = 10L, expected = 10L))
})

test_that("DockerCluster stop server", {
    expect_error(
        cluster$stopServer(),
        NA
    )
    expect_true(cluster$isServerRunning())
    expect_true(cluster$clusterExists())
})


test_that("DockerCluster worker container", {
    container <- cluster@cloudProvider$workerContainer
    expect_equal(container$environment$serverIp, serverData$publicIp)
    expect_equal(container$environment$serverPort, serverData$publicPort)
})

test_that("DockerCluster server container", {
    container <- cluster@cloudProvider$serverContainer
    expect_true(length(container$environment) == 0)
})


test_that("DockerCluster register backend", {
    expect_error(
        cluster$registerBackend(),
        NA
    )
    expect_error(
        cluster$deregisterBackend(),
        NA
    )
})

test_that("DockerCluster stop cluster", {
    expect_error(
        cluster$stopCluster(),
        NA
    )
    expect_identical(cluster$getWorkerNumbers(),
                     list(initializing = 0L, running = 0L, expected = 10L))
    expect_true(cluster$isServerRunning())

    expect_error(
        cluster$update(),
        NA
    )
})


test_that("DockerCluster cleanup", {
    cluster <<- NULL
    expect_error(
        gc(),
        NA
    )
})


