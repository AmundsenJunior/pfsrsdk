#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getEntityByName.

context("Tests for getEntityByName")

test_that(paste("test getEntityByName() on:", env$auth), {
  ta1 <- getEntityByName(con$coreApi, data$persistentEntityType, data$persistentEntityName, TRUE, useVerbose = verbose)
  name <- ta1$entity[[1]]$Name

  expect_match(name, data$persistentEntityName, all = verbose)
  expect_true(!is.null(ta1$entity[[1]]$`Id@odata.type`))
})
