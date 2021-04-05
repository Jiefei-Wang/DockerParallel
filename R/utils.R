add_asteriod <- function(x, begin_number, end_number){
  substr(x,begin_number+1,nchar(x)-end_number-1) <-
    paste0(rep("*",nchar(x)-begin_number-end_number),collapse = "")
}

generateRandomPassword <- function(len = 26){
    paste0(letters[sample(26, len, replace = TRUE)],collapse = "")
}


verbosePrint<-function(verbose, ...){
    if(verbose)
        message(...)
}
is.empty <- function(x){
    is.null(x) || length(x)==0
}
myknapsack <- function (workerPerHandle, killedWorkerNum)
{
  idx <- which(workerPerHandle<=killedWorkerNum)
  if(length(idx)==0){
    return(list(capacity=0, indices = c()))
  }
  KnapsackSolution <-
    adagio::knapsack(workerPerHandle[idx],
                     workerPerHandle[idx],
                     killedWorkerNum)
  KnapsackSolution$indices <- idx[KnapsackSolution$indices]
  KnapsackSolution
}

makeReadMe <- function(){
  fileName <- "vignettes/vignette.md"
  content <- readChar(fileName, file.info(fileName)$size)
  content <- gsub("---.+?---","",content)
  content <- gsub("^(\r\n)+","",content)
  content <- gsub("([a-zA-Z]+)\\.jpg","vignettes/\\1.jpg",content)
  readme <- file("README.md")
  writeLines(content, readme)
  close(readme)
}


