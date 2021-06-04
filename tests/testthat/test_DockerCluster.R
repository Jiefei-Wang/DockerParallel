resetDummyProvider()
cluster <- NULL
verbose <- 0

test_that("DockerCluster constructor", {
    expect_error(
        provider <- DummyProvider()
        ,NA)
    expect_error(
        container <- DummyWorkerContainer()
        ,NA)
    expect_error(
        cluster <<- makeDockerCluster(
            cloudProvider = provider,
            workerContainer = container,
            verbose = verbose)
        ,NA)
    })

test_that("Register the backend when the cluster is not running", {
    expect_error(cluster$registerBackend())
})


test_that("DockerCluster print", {
    expect_error(
        capture.output(show(cluster)),
        NA
    )
})

test_that("DockerCluster set worker number", {
    expect_error(
        cluster$setWorkerNumber(10),
        NA
    )
    expect_equal(.getExpectedWorkerNumber(cluster), 10)
    expect_identical(cluster$getWorkerNumbers(),
                     list(initializing = 0L, running = 0L, expected = 10L))

})

test_that("DockerCluster run server", {
    expect_error(
        cluster$startServer(),
        NA
    )
    expect_true(cluster@cloudProvider$initialized)
    expect_true(cluster$isServerRunning())
    expect_identical(cluster$getWorkerNumbers(),
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
    expect_identical(cluster$getWorkerNumbers(),
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
    expect_identical(cluster$getWorkerNumbers(),
                     list(initializing = 0L, running = 0L, expected = 5L))
    expect_false(cluster$isServerRunning())

    expect_error(
        cluster$update(),
        NA
    )
})


test_that("DockerCluster reconnect: 1 server 5 worker", {
    gc()
    provider1 <- DummyProvider()
    container1 <- DummyWorkerContainer()
    cluster1 <- makeDockerCluster(
        cloudProvider = provider1,
        workerContainer = container1,
        workerNumber = 5,
        verbose = verbose)
    expect_false(cluster1$clusterExists())
    expect_error(
        cluster1$startCluster(),
        NA
    )
    expect_identical(
        cluster1$getWorkerNumbers(),
        list(initializing = 0L, running = 5L, expected = 5L)
    )

    provider2 <- DummyProvider()
    container2 <- DummyWorkerContainer()
    cluster2 <- makeDockerCluster(
        cloudProvider = provider2,
        workerContainer = container2,
        workerNumber = 5,
        verbose = verbose)
    expect_true(cluster2$clusterExists())
    expect_error(cluster2$reconnect(),
                 NA)
    expect_identical(
        cluster1$getWorkerNumbers(),
        list(initializing = 0L, running = 5L, expected = 5L)
    )
})


test_that("DockerCluster reconnect: 1 server no worker", {
    gc()
    provider1 <- DummyProvider()
    container1 <- DummyWorkerContainer()
    cluster1 <- makeDockerCluster(
        cloudProvider = provider1,
        workerContainer = container1,
        workerNumber = 0,
        verbose = verbose)
    expect_false(cluster1$clusterExists())
    expect_error(
        cluster1$startCluster(),
        NA
    )
    expect_identical(
        cluster1$getWorkerNumbers(),
        list(initializing = 0L, running = 0L, expected = 0L)
    )

    provider2 <- DummyProvider()
    container2 <- DummyWorkerContainer()
    cluster2 <- makeDockerCluster(
        cloudProvider = provider2,
        workerContainer = container2,
        workerNumber = 5,
        verbose = verbose)
    expect_true(cluster2$clusterExists())
    expect_error(cluster2$reconnect(),
                 NA)
    expect_identical(
        cluster1$getWorkerNumbers(),
        list(initializing = 0L, running = 0L, expected = 0L)
    )
    gc()
})


test_that("DockerCluster auto remove the cluster", {
    gc()
    provider <- DummyProvider()
    container <- DummyWorkerContainer()
    cluster <- makeDockerCluster(
        cloudProvider = provider,
        workerContainer = container,
        verbose = verbose)
    expect_false(cluster$clusterExists())
    expect_error(cluster$reconnect())
})



