workerHandles <- names(getAllHandles())
for(i in workerHandles){
    handleName <- getHandleName(i)
    Sys.unsetenv(handleName)
}

Sys.setenv(DummyManagedProvider = "")
cluster <- NULL

test_that("DockerCluster constructor", {
    expect_error(
        provider <- DummyManagedProvider()
        ,NA)
    expect_error(
        container <- DummyWorkerContainer()
        ,NA)
    expect_error(
        cluster <<- makeDockerCluster(
            cloudProvider = provider,
            workerContainer = container)
        ,NA)
})

test_that("DockerCluster print", {
    expect_error(
        show(cluster),
        NA
    )
})

test_that("DockerCluster set worker number", {
    expect_error(
        cluster$setWorkerNumber(10),
        NA
    )
    expect_equal(.getExpectedWorkerNumber(cluster), 10)
    expect_identical(cluster$getWorkerNumber(),
                     list(initializing = 0L, running = 0L, expected = 10L))

})

test_that("DockerCluster run server", {
    expect_error(
        cluster$startServer(),
        NA
    )
    expect_true(cluster@cloudProvider$initialized)
    expect_true(cluster$isServerRunning())
    expect_identical(cluster$getWorkerNumber(),
                     list(initializing = 0L, running = 0L, expected = 10L))
    expect_equal(cluster@cloudRuntime$serverPublicIp, "8.8.8.8")
    expect_equal(cluster@cloudRuntime$serverPublicPort, 123)
    expect_equal(cluster@cloudRuntime$serverPrivateIp, "192.168.1.1")
    expect_equal(cluster@cloudRuntime$serverPrivatePort, 456)

    expect_identical(cluster@cloudProvider$serverContainer$image, "serverImage")
})


test_that("DockerCluster run worker", {
    expect_error(
        cluster$setWorkerNumber(5),
        NA
    )
    expect_identical(cluster$getWorkerNumber(),
                     list(initializing = 0L, running = 5L, expected = 5L))
    expect_identical(cluster@cloudProvider$workerContainer$image, "workerImage")
})

test_that("DockerCluster update", {
    expect_error(
        cluster$update(),
        NA
    )
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
    expect_identical(cluster$getWorkerNumber(),
                     list(initializing = 0L, running = 0L, expected = 5L))
    expect_false(cluster$isServerRunning())

    expect_error(
        cluster$update(),
        NA
    )
})


test_that("DockerCluster reconnect", {
    provider1 <- DummyManagedProvider()
    container1 <- DummyWorkerContainer()
    cluster1 <- makeDockerCluster(
        cloudProvider = provider1,
        workerContainer = container1,
        workerNumber = 5)
    expect_error(
        cluster1$startCluster(),
        NA
    )
    expect_identical(
        cluster1$getWorkerNumber(),
        list(initializing = 0L, running = 5L, expected = 5L)
    )

    provider2 <- DummyManagedProvider()
    container2 <- DummyWorkerContainer()
    cluster2 <- makeDockerCluster(
        cloudProvider = provider2,
        workerContainer = container2,
        workerNumber = 5)
    expect_error(cluster2$reconnect(),
                 NA)
    expect_identical(
        cluster1$getWorkerNumber(),
        list(initializing = 0L, running = 5L, expected = 5L)
    )
})



test_that("DockerCluster auto remove the cluster", {
    gc()
    provider <- DummyManagedProvider()
    container <- DummyWorkerContainer()
    cluster <- makeDockerCluster(
        cloudProvider = provider,
        workerContainer = container)
    expect_error(cluster$reconnect())
})

