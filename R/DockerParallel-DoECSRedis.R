registerDoECSRedis <- function(x, verbose = FALSE){
    queue <- x$serverQueue
    doRedis::registerDoRedis(queue,
                    host = x$serverIP,
                    password = x$serverPassword)
}
