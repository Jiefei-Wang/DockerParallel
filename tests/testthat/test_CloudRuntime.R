test_that("CloudRuntime constructor", {
    expect_error(runtime <- CloudRuntime(),NA)
    expect_error(
        runtime <- CloudRuntime(serverFromOtherSource = FALSE,
                                serverPublicIp = "127.0.0.1",
                                serverPublicPort = 123,
                                serverPrivateIp = "127.0.0.2",
                                serverPrivatePort = 456,
                                initializingWorkerNumber = 1,
                                runningWorkerNumber = 2)
        ,NA)
    expect_equal(runtime$serverFromOtherSource, FALSE)
    expect_equal(runtime$serverPublicIp, "127.0.0.1")
    expect_equal(runtime$serverPublicPort, 123)
    expect_equal(runtime$serverPrivateIp, "127.0.0.2")
    expect_equal(runtime$serverPrivatePort, 456)
    expect_equal(runtime$initializingWorkerNumber, 1)
    expect_equal(runtime$runningWorkerNumber, 2)
})
