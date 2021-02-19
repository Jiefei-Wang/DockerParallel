Container <- function(cpu = 0.25, memory = 1024, ...){
  .Container(cpu = cpu, memory = memory, ...)
}

redisServerContainer <- function(cpu = 0.25, memory = 2048,
                                 environment = list(),
                                 image = "dockerparallel/parallel-redis-server",
                                 exec = NULL
                                 ){
  Container(cpu = cpu,
            memory = memory,
            environment = environment,
            image = image,
            exec = exec
            )
}

redisWorkerContainer <- function(cpu = 0.25, memory = 512,
                                 environment = list(),
                                 image = "dockerparallel/parallel-redis-worker",
                                 exec = NULL
){
  Container(cpu = cpu,
            memory = memory,
            environment = environment,
            image = image,
            exec = exec
  )
}


setMethod("show", "Container", function(object){
  cat("  image:  ", object@image, "\n")
  cat("  CPU:    ", object@cpu, "cores\n")
  cat("  Memory: ", object@memory, "MB\n")
  invisible(NULL)
})

getECSCPU <- function(container){
    as.numeric(container@cpu)*1024
}

getECSMem <- function(container){
    as.numeric(container@memory)
}


