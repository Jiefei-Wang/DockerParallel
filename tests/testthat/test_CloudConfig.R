test_that("CloudConfig Constructor", {
    expect_error(config <- CloudConfig(), NA)
    expect_error(
        config <- CloudConfig(
            jobQueueName = "test",
            expectedWorkerNumber = 2,
            serverHardware = DockerHardware(cpu = 128),
            workerHardware = DockerHardware(cpu = 256),
            serverPort = 123,
            serverPassword = "mypassword",
            serverWorkerSameLAN = FALSE,
            serverClientSameLAN = TRUE)
        ,NA)
    expect_equal(config$jobQueueName, "test")
    expect_equal(config$expectedWorkerNumber, 2)
    expect_equal(config$serverHardware, DockerHardware(cpu = 128))
    expect_equal(config$workerHardware, DockerHardware(cpu = 256))
    expect_equal(config$serverPort, 123)
    expect_equal(config$serverPassword, "mypassword")
    expect_equal(config$serverWorkerSameLAN, FALSE)
    expect_equal(config$serverClientSameLAN, TRUE)
})
