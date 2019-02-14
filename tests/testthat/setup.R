api <- CoreAPIV2::coreAPI(env$auth)
con <- CoreAPIV2::authBasic(api, useVerbose = verbose)

data <- jsonlite::fromJSON(env$data)

# configure test environment for JSON SDK usage
CoreAPIV2::loadXmlConfigFile(con$coreApi, appName = "core_CoreApp.SDK")
