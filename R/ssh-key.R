#' Set the ssh key file
#'
#' Set the ssh key file. This function will be called when the package is
#' loaded. If no argument is provided, it
#' will look at the environment variables `DockerParallelSSHPublicKey`
#'
#' @param publicKey path to the public key
#' @export
setSSHPubKeyPath<-function(publicKey=NULL){
  if(!is.null(publicKey)){
    packageSetting$publicKey <- publicKey
  }
  public_key_env <- Sys.getenv("DockerParallelSSHPublicKey")

  if(is.null(packageSetting$publicKey)){
    if(public_key_env!=""){
      packageSetting$publicKey<- public_key_env
    }
  }
  packageSetting$publicKey
}
#' @export
getSSHPubKeyPath <- function(){
  packageSetting$publicKey
}
#' @export
getSSHPubKeyValue <- function(){
  fileName <- getSSHPubKey()
  if(!is.empty(fileName)){
      pubkey <- readChar(fileName, file.info(fileName)$size)
      pubkey <- gsub("[\r\n]", "", pubkey)
      pubkey
  }else{
      NULL
  }

}
