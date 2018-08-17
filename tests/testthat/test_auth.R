
context("Tests for authentication")


rm(list=ls())

verbose <- TRUE
#setup to test against multiple environments
#name files Auth-[pfsversion].json example Auth-5.3.8 for this to pick them up. 
environments<<-list.files("test_environment","^(Auth-)[0-9]+\\.[0-9]+\\.[0-9]+\\.json$",full.names=TRUE)

cat(paste0("\n environments:\n",environments,"\n"))

lapply(environments, function(x){

     test_that(paste("test login parameters for environment and updating metadata", x),
            {
              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(x)
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
              
              metadata<- CoreAPIV2::updateMetadata(con$coreApi,useVerbose=TRUE)
              print(httr::http_status(metadata$response))
              expect_match(httr::http_status(metadata$response)$category ,"Success" )
              
              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
  
  test_that("single account with bad password returns error",
                       {
                         verbose <- FALSE
                         api <- CoreAPIV2::coreAPI(x)
                        bapi<- api
                        bapi$pwd <-"badpassword"
                        expect_error(CoreAPIV2::authBasic(bapi,useVerbose = verbose))
               })
}
)






