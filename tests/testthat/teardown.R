
#sink the output down to where the sun dont shine and log out. 
zz <- file("/dev/null", open = "wt")
sink(zz,type = "m")
CoreAPIV2::logOut(con$coreApi)
sink()
