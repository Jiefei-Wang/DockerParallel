# context("Testing the Docker cluster object")
#
# workerPerContainer <- 2L
# provider <- ECSFargateProvider()
# container <- BiocFERWorkerContainer()
# container$maxWorkerNum <- workerPerContainer
#
# generalDockerClusterTest(cloudProvider = provider, workerContainer = container)
#
#
# library(foreach)
# test_that("add R package", {
#     cluster <- makeDockerCluster(cloudProvider = provider,
#                                  workerContainer = container,
#                                  workerNumber = 1,
#                                  workerCpu = 256,
#                                  workerMemory = 512,
#                                  serverCpu = 256,
#                                  serverMemory = 512)
#     cluster$workerContainer$addRPackages("BiocGenerics")
#     cluster$startCluster()
#     runningWorkerNum <- 0
#     startTime <- Sys.time()
#     while(runningWorkerNum != 1 && difftime(Sys.time(), startTime, units = "secs") < 60*2){
#         runningWorkerNum <- foreach::getDoParWorkers()
#     }
#     expect_equal(runningWorkerNum, 1)
#
#     res <- foreach(i = 1) %dopar%{
#         requireNamespace("BiocGenerics", quietly = TRUE)
#     }
#     expect_true(res[[1]])
# })
