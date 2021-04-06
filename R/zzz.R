#' @import aws.ecx
#' @importFrom doRedis registerDoRedis
#' @importFrom rjson fromJSON toJSON
NULL

packageSetting<-new.env()
packageSetting$publicKey <- NULL


.onLoad <- function(libname, pkgname){
    setSSHPubKeyPath()
}
