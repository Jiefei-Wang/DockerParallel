ecs_get_json <-function(file_name){
  pkg_path <- find.package("DockerParallel")
  file_path <- file.path(pkg_path,"json_config",file_name)
  if(!file.exists(file_path)){
    file_path <- file.path("inst","json_config",file_name)
  }
  fromJSON(file=file_path,simplify=FALSE)
}

verbosePrint<-function(verbose, ...){
  if(verbose)
    message(...)
}
## get resource name from ARN
get_resource_names <- function(ARNs){
  unname(vapply(ARNs, get_resource_name, character(1)))
}
get_resource_name <- function(ARN){
  match_index <- gregexpr(":",ARN,fixed = TRUE)[[1]]
  x <- substring(ARN, match_index[5]+1)
  separator <-regexpr("/", x, fixed = TRUE)
  if(separator==-1){
    x
  }else{
    sub(".+?/","",x)
  }
}


valid_fargate_settings <- list()
valid_fargate_settings$"256" <- c(512,1024,2048)
valid_fargate_settings$"512" <- 1024*(1:4)
valid_fargate_settings$"1024" <- 1024*(2:8)
valid_fargate_settings$"2048" <- 1024*(4:16)
valid_fargate_settings$"4096" <- 1024*(8:30)

getValidFargateCpuMemory<-function(cpu,memory){
  result <- list()
  valid_cpu_numbers <- as.numeric(names(valid_fargate_settings))
  valid_cpus <- valid_cpu_numbers[valid_cpu_numbers>=cpu]
  for(i in valid_cpus){
    idx <- which(valid_fargate_settings[[as.character(i)]]>=memory)
    if(length(idx)!=0){
      return(list(cpu = i, memory = valid_fargate_settings[[as.character(i)]][idx[1]]))
    }
  }
  NULL
}

getNWorkerPerContainer <- function(cpu_per_worker, memory_per_worker){
  max_worker <- min(floor(4096/as.numeric(cpu_per_worker)),floor(1024*30/as.numeric(memory_per_worker)))
  if(max_worker==0){
    stop("Cannot find a fargate hardware to accommodate the CPU and memory requirement for the worker")
  }
  max_worker
}

get_tag_value <-function(tag_list, tag_name, tag_value, target){
  for(i in tag_list){
    if(i[[tag_name]]==target){
      return(i[[tag_value]])
    }
  }
  return(NULL)
}


getFilter <- function(x){
  query <- list()
  filter_name <- names(x)
  for(i in seq_along(x)){
    query[[paste0("Filter.",i,".Name")]] <- filter_name[i]
    for(j in seq_along(x[[i]])){
      query[[paste0("Filter.",i,".Value.",j)]] <- x[[i]][[j]]
    }
  }
  query
}

is.empty <- function(x){
    is.null(x) || length(x)==0
}


getEnvJson <- function(x){
    result <- list()
    for(i in seq_along(x)){
        result[[i]] <- list(name = names(x)[i], value = as.character(x[[i]]))
    }
    result
}
