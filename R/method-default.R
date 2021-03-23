setMethod("initialProvider", "ANY", function(provider, verbose = FALSE, ...){

})

setMethod("initialCluster", "ANY", function(provider, cluster, verbose = FALSE, ...){

})

setMethod("instanceAlive", "ANY", function(provider, instanceHandles, verbose = FALSE, ...){
    rep(TRUE, length(instanceHandles))
})
