#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getEntityByName.
  
context("Tests for getEntityByName")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  
  con <- Connect(x)
  test_that(paste("test getEntityByName() on: ", x), {
    ta1 <- CoreAPIV2::getEntityByName(con$coreApi, POCOASSOC, POCOASSOC1NAME,FALSE,FALSE)
    Name <- ta1$entity[[1]]$Name
    expect_match(Name, POCOASSOC1NAME, all = verbose)
    
  })
  
  CoreAPIV2::logOut(con$coreApi)
})

