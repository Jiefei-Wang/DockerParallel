#' @import methods
#' @import utils
NULL

packageSetting<-new.env()
packageSetting$publicKey <- NULL
packageSetting$cloudProvider <- NULL
packageSetting$workerContainer <- NULL


.onLoad <- function(libname, pkgname){
    setSSHPubKeyPath()
}
