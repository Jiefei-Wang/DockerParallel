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


validFargateSettings <- list()
validFargateSettings$"256" <- c(512,1024,2048)
validFargateSettings$"512" <- 1024*(1:4)
validFargateSettings$"1024" <- 1024*(2:8)
validFargateSettings$"2048" <- 1024*(4:16)
validFargateSettings$"4096" <- 1024*(8:30)

getValidFargateCpuMemory<-function(CPU,memory){
  result <- list()
  validCPUNumbers <- as.numeric(names(validFargateSettings))
  validCPUs <- validCPUNumbers[validCPUNumbers>=CPU]
  for(i in validCPUs){
    idx <- which(validFargateSettings[[as.character(i)]]>=memory)
    if(length(idx)!=0){
      return(list(CPU = i, memory = validFargateSettings[[as.character(i)]][idx[1]]))
    }
  }
  NULL
}

getMaxWorkerPerContainer <- function(CPUPerWorker, memoryPerWorker){
  maxWorker <- min(floor(4096/as.numeric(CPUPerWorker)),floor(1024*30/as.numeric(memoryPerWorker)))
  if(maxWorker==0){
    stop("Cannot find a fargate hardware to accommodate the CPU and memory requirement for the worker")
  }
  maxWorker
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
is.valid <- function(x, attrName){
  data1 <- getECSCloudData(x, attrName)
  data2 <- do.call("@",list(x, attrName))
  if(is.empty(data2)){
    !is.empty(data1)
  }else{
    !is.empty(data1)&& data1==data2
  }
}

is.invalid <- function(x, attrName){
  !is.valid(x, attrName)
}

getEnvJson <- function(x){
    result <- list()
    for(i in seq_along(x)){
        result[[i]] <- list(name = names(x)[i], value = as.character(x[[i]]))
    }
    result
}
