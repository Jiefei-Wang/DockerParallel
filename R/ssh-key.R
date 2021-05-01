#' Set the ssh key file
#'
#' Set the ssh key file. This function will be called when the package is
#' loaded. If no argument is provided and the current stored path is `NULL`, it
#' will look at the environment variables `DockerParallelSSHPublicKey`
#'
#' @param publicKey path to the public key
#' @examples
#' ## Getting the path from the environment variable "DockerParallelSSHPublicKey"
#' setSSHPubKeyPath()
#' @return The path to the public key
#' @export
setSSHPubKeyPath<-function(publicKey=NULL){
  if(!is.null(publicKey)){
    packageSetting$publicKey <- publicKey
  }else{
    publicKeyEnv <- Sys.getenv("DockerParallelSSHPublicKey")
    if(publicKeyEnv!=""){
      packageSetting$publicKey<- publicKeyEnv
    }
  }

  packageSetting$publicKey
}
#' Get the path to the public ssh key
#'
#' Get the path to the public ssh key
#'
#' @examples
#' getSSHPubKeyPath()
#' @return The path to the public ssh key
#' @export
getSSHPubKeyPath <- function(){
  packageSetting$publicKey
}
#' Get the public ssh key
#'
#' Get the public ssh key
#'
#' @examples
#' getSSHPubKeyValue()
#' @return The public ssh key
#' @export
getSSHPubKeyValue <- function(){
  fileName <- getSSHPubKeyPath()
  if(!is.empty(fileName)){
    pubkey <- readChar(fileName, file.info(fileName)$size, useBytes  = TRUE)
    pubkey <- gsub("[\r\n]", "", pubkey)
    pubkey
  }else{
    NULL
  }

}
