api <- CoreAPIV2::coreAPI(env$auth)
con <- CoreAPIV2::authBasic(api, useVerbose = verbose)

data <- jsonlite::fromJSON(env$data)