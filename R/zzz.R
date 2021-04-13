#' @import aws.ecx
#' @importFrom doRedis registerDoRedis
NULL

packageSetting<-new.env()
packageSetting$publicKey <- NULL


.onLoad <- function(libname, pkgname){
    setSSHPubKeyPath()
}
