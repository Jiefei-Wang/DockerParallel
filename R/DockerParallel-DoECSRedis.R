registerDoECSRedis <- function(x, verbose = FALSE){
    queue <- getECSClusterData(x, "serverQueue")
    serverClientIP <- getECSClusterData(x, "serverClientIP")
    serverPassword <- getECSClusterData(x, "serverPassword")
    doRedis::registerDoRedis(queue,
                    host = serverClientIP,
                    password = serverPassword)
}



