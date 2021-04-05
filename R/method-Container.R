.Container$methods(
    show = function(){
        cat("Container reference object\n")
        cat("  Image:     ", .self$image, "\n")
        if(!is.null(.self$command)){
            cat("  Command:   ", .self$command, "\n")
        }
        cat("  maxWorkers:", .self$maxWorkerNum, "\n")
        cat("  Environment variables:\n")
        for(i in names(.self$environment)){
            cat("    ",i,": ", .self$environment[[i]], "\n",sep="")
        }
        invisible(NULL)
    }
)
