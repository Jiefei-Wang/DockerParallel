#' commom params
#'
#'
#' @param verbose Integer. The verbose level, default 1.
#' @param provider S4 `CloudProvider` object. The service provider.
#' @param cluster S4 `DockerCluster` object.
#' @param container S4 `DockerContainer` Object.
#' @param hardware S4 `DockerHardware` Object.
#' @rdname generics-commonParams
#' @name generics-commonParams
#' @return No return value
NULL


############################################################
##      common generics for provider and container
############################################################
#' Get the exported method and variable from the provider or container
#'
#' Get the exported method and variable from the provider or container. These
#' methods should be used by the developer to export their APIs to the user. The
#' `DockerCluster` object will call `getExportedNames` and `getExportedObject` and
#' export them to the user.
#'
#' @param x A cloud provider or container object
#'
#' @details
#' If the exported object is a function, the exported function will be defined in
#' an environment such that the `DockerCluster` object is assigned to the variable `cluster`.
#' In other words, the exported function can use the variable `cluster` without define it.
#' This can be useful if the developer needs to change anything in the cluster without
#' asking the user to provide the `DockerCluster` object. The best practice is to define
#' `cluster` as the function argument, the argument will be removed when the function is
#' exported to the user. The user would not be bothered with the redundant `cluster` argument.
#'
#' @returns
#' getExportedNames: The names of the exported functions or variables
#' getExportedObject: The exported functions or variable
#' @rdname exported-apis
#' @export
setGeneric("getExportedNames", function(x){
    standardGeneric("getExportedNames")
})

#' @param name The name of the exported object
#' @rdname exported-apis
#' @export
setGeneric("getExportedObject", function(x, name){
    standardGeneric("getExportedObject")
})

#' get/set docker cluster static data
#'
#' get/set docker cluster static data.
#' These functions are designed for the `reconnect` function for `DockerCluster`.
#' The return value can be serialized and used by the cloud provider to recover
#' the `DockerCluster` object. The default method for `DockerCluster` will
#' use `getDockerStaticData` to get the static data in `cloudConfig`, `ServerContainer`
#' and `WorkerContainer`.
#'
#' @param x The object which the static data will be extracted from
#' or the object that will hold the unserialized data.
#' @param staticData The data returned by `getDockerStaticData`
#' @return
#' getDockerStaticData: Any data that is serializable
#' setDockerStaticData: No return value should be expected, the object that
#' is passed to the function will be updated.
#' @rdname DockerStaticData
#' @export
setGeneric("getDockerStaticData", function(x){
    standardGeneric("getDockerStaticData")
})
#' @rdname DockerStaticData
#' @export
setGeneric("setDockerStaticData", function(x, staticData){
    standardGeneric("setDockerStaticData")
})







