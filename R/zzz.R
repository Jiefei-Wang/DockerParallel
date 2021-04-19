#' @import aws.ecx
#' @import methods
#' @import utils
#' @import testthat
#' @importFrom doRedis registerDoRedis
NULL

packageSetting<-new.env()
packageSetting$publicKey <- NULL


.onLoad <- function(libname, pkgname){
    setSSHPubKeyPath()
}
