
#sink the output down to nowhere and log out. 
zz <- file("/dev/null", open = "wt")
sink(zz,type = "m")
CoreAPIV2::logOut(con$coreApi)
sink()
