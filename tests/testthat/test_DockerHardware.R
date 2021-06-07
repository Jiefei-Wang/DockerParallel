test_that("DockerHardware constructor", {
    expect_error(
        hardware <- DockerHardware()
        ,NA)
    expect_error(
        hardware <- DockerHardware(
            cpu = 512,
            memory = 1024,
            id = "test")
        ,NA)
    expect_equal(hardware@cpu, 512)
    expect_equal(hardware@memory, 1024)
    expect_equal(hardware@id, "test")
})
