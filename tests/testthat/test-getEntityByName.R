#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityByName.

context("Tests for getEntityByName")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test getEntityByName() on:", env$auth), {
  ta1 <- getEntityByName(con$coreApi, data$persistentEntityType, data$persistentEntityName, FALSE, FALSE)
  name <- ta1$entity[[1]]$Name

  expect_match(name, data$persistentEntityName, all = verbose)
})
