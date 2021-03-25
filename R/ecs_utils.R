ecs_get_json <-function(file_name){
  pkg_path <- find.package("DockerParallel")
  file_path <- file.path(pkg_path,"json_config",file_name)
  if(!file.exists(file_path)){
    file_path <- file.path("inst","json_config",file_name)
  }
  rjson::fromJSON(file=file_path,simplify=FALSE)
}

verbosePrint<-function(verbose, ...){
  if(verbose)
    message(...)
}


ecs_list_to_array <- function(x, name = "name", value = "value"){
  result <- list()
  for(i in seq_along(x)){
    result[[i]] <- list()
    result[[i]][[name]] <- names(x)[i]
    result[[i]][[value]] <- x[[i]]
  }
  result
}

## get resource name from ARN
ECSGetResourceNames <- function(ARNs){
  unname(vapply(ARNs, ECSGetResourceName, character(1)))
}
ECSGetResourceName <- function(ARN){
  match_index <- gregexpr(":",ARN,fixed = TRUE)[[1]]
  x <- substring(ARN, match_index[5]+1)
  separator <-regexpr("/", x, fixed = TRUE)
  if(separator==-1){
    x
  }else{
    sub(".+?/","",x)
  }
}

environmentToJSON <- function(x){
  x <- x[!vapply(x, is.null, logical(1))]
  result <- list()
  for(i in seq_along(x)){
    result[[i]] <- list(name = names(x)[i], value = as.character(x[[i]]))
  }
  result
}

validFargateSettings <- list()
validFargateSettings$"256" <- c(512,1024,2048)
validFargateSettings$"512" <- 1024*(1:4)
validFargateSettings$"1024" <- 1024*(2:8)
validFargateSettings$"2048" <- 1024*(4:16)
validFargateSettings$"4096" <- 1024*(8:30)

getValidFargateHardware<-function(hardware){
  cpu <- hardware@cpu
  memory <- hardware@memory

  result <- list()
  validCpuNumbers <- as.numeric(names(validFargateSettings))
  validCpus <- validCpuNumbers[validCpuNumbers>=cpu]
  for(i in validCpus){
    idx <- which(validFargateSettings[[as.character(i)]]>=memory)
    if(length(idx)!=0){
      hardware@cpu <- i
      hardware@memory <- validFargateSettings[[as.character(i)]][idx[1]]
      break
    }
  }
  hardware
}

getMaxWorkerPerContainer <- function(hardware){
  cpu <- hardware@cpu
  memory <- hardware@memory
  maxWorker <- min(floor(4096/cpu),floor(1024*30/memory))
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
