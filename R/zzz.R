#' @importFrom rjson fromJSON toJSON
#' @importFrom parallelly makeNodePSOCK makeClusterPSOCK
#' @importFrom aws.signature locate_credentials signature_v4_auth
#' @importFrom httr POST GET http_error add_headers content timeout
#' @import xml2
NULL

package_setting<-new.env()
package_setting$private_key <- NULL
package_setting$public_key <- NULL
package_setting$print_when_retrying <- TRUE
package_setting$retry_time <- 3
package_setting$REST_timeout <- 10

set_retry_time <- function(x){
  package_setting$retry_time <- x
}
set_print_when_retrying <- function(x){
  package_setting$print_when_retrying <- x
}

.onLoad <- function(libname, pkgname){
  set_ssh_key()
  ecs_set_credentials()

}
