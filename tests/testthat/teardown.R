
# sink the output down to nowhere and log out.
# no lint
zz <- file("/dev/null", open = "wt")
sink(zz, type = "m")
logOut(con$coreApi)
sink()
