#' @import methods
#' @import utils
#' @importFrom jsonlite base64_enc
NULL

packageSetting<-new.env()
packageSetting$publicKey <- NULL
packageSetting$cloudProvider <- NULL
packageSetting$workerContainer <- NULL


.onLoad <- function(libname, pkgname){
    setSSHPubKeyPath()
}
