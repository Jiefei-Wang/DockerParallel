.DockerContainer$methods(
    show = function(){
        cat("Container reference object\n")
        cat("  Image:     ", .self$image, "\n")
        cat("  maxWorkers:", .self$maxWorkerNum, "\n")
        cat("  Environment variables:\n")
        for(i in names(.self$environment)){
            cat("    ",i,": ", .self$environment[[i]], "\n",sep="")
        }
        invisible(NULL)
    }
)

containerExportedMethods <- c("getSysPackages", "setSysPackages", "addSysPackages",
                              "getRPackages", "setRPackages", "addRPackages")
setMethod("getExportedNames", "DockerContainer",
          function(x){
              containerExportedMethods
          }
)


setMethod("getExportedObject", "DockerContainer",
          function(x, name){
              if(!name%in%containerExportedMethods)
                  stop("Undefined object <",name,">")
              get(name)
          }
)

getSysPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages
}
setSysPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- packages
}
addSysPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- c(packages,workerContainer$sysPackages)
}

getRPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages
}
setRPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- packages
}
addRPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- c(packages,workerContainer$RPackages)
}
