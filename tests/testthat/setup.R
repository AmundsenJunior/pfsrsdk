api <- coreAPI(env$auth)
con <- authBasic(api, useVerbose = verbose)

data <- jsonlite::fromJSON(env$data)

# configure test environment for JSON SDK usage
pfsrsdk:::loadXmlConfigFile(con$coreApi, appName = "core_CoreApp.SDK")
