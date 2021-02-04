#' Set the ssh key file
#'
#' Set the ssh key file. This function will be called when the package is
#' loaded. If no argument is provided, it
#' will look at the environment variables `DockerParallelSSHPrivateKey`
#' and `DockerParallelSSHPublicKey`
#'
#' @param private_key path to the private key
#' @param public_key path to the public key
#' @export
set_ssh_key<-function(private_key=NULL, public_key=NULL){
  if(!is.null(private_key)){
    package_setting$private_key <- private_key
  }
  if(!is.null(public_key)){
    package_setting$public_key <- public_key
  }
  private_key_env <- Sys.getenv("DockerParallelSSHPrivateKey")
  public_key_env <- Sys.getenv("DockerParallelSSHPublicKey")
  if(is.null(package_setting$private_key)){
    if(private_key_env!=""){
      package_setting$private_key<- private_key_env
    }
  }
  if(is.null(package_setting$public_key)){
    if(public_key_env!=""){
      package_setting$public_key<- public_key_env
    }else{
      public_key_env <- paste0(private_key_env,".pub")
      if(file.exists(public_key_env)){
        package_setting$public_key<- public_key_env
      }
    }
  }
  list(private_key = package_setting$private_key,
       public_key=package_setting$public_key)
}

check_ssh_key<-function(){
  if(is.null(get_ssh_private_key())){
    stop("SSH private key is not set! Please set them via `set_ssh_key`")
  }
  if(is.null(get_ssh_public_key())){
    stop("SSH public key is not set! Please set them via `set_ssh_key`")
  }
  if(!file.exists(get_ssh_private_key())){
    stop("SSH private key does not exist! Please set them via `set_ssh_key`")
  }
  if(!file.exists(get_ssh_public_key())){
    stop("SSH public key does not exist! Please set them via `set_ssh_key`")
  }
}

get_ssh_private_key <- function(){
  package_setting$private_key
}
get_ssh_public_key <- function(){
  package_setting$public_key
}
get_ssh_public_key_value <- function(){
  fileName <- get_ssh_public_key()
  pubkey <- readChar(fileName, file.info(fileName)$size)
  pubkey<-gsub("[\r\n]", "", pubkey)
  pubkey
}
