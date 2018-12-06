
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Tests for getEntityAssociations.

context("Tests for getEntityAssociations")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  test_that(paste("test getEntityAssociations() on: ", x), {
    ta1 <- CoreAPIV2::getEntityByName(con$coreApi, POCOASSOC, POCOASSOC1NAME, FALSE, FALSE)
    ta2 <- CoreAPIV2::getEntityByName(con$coreApi, POCOASSOC, POCOASSOC2NAME, FALSE, FALSE)
    PC60 <- CoreAPIV2::getEntityByName(con$coreApi, TESTPOCO, POCO60NAME, FALSE, FALSE)

    # test update associations

    updateValues <- list()
    updateValues[[ASSOCIATIONCONTEXTLISTNAME]] <- c(POCOASSOC, ta2$entity[[1]]$Barcode)

    us <- CoreAPIV2::updateEntityAssociations(con$coreApi, TESTPOCO, PC60$entity[[1]]$Barcode, updateValues, useVerbose = FALSE)
    as <- CoreAPIV2::getEntityAssociations(con$coreApi, TESTPOCO, PC60$entity[[1]]$Barcode, associationContext = ASSOCIATIONCONTEXTLISTNAME, fullMetadata = TRUE, useVerbose = FALSE)

    expect_match(as$entity[[1]]$Barcode, ta2$entity[[1]]$Barcode)

    # Change it back

    updateValues[[ASSOCIATIONCONTEXTLISTNAME]] <- c(POCOASSOC, ta1$entity[[1]]$Barcode)

    us <- CoreAPIV2::updateEntityAssociations(con$coreApi, TESTPOCO, PC60$entity[[1]]$Barcode, updateValues, useVerbose = FALSE)
    as <- CoreAPIV2::getEntityAssociations(con$coreApi, TESTPOCO, PC60$entity[[1]]$Barcode, associationContext = ASSOCIATIONCONTEXTLISTNAME, fullMetadata = TRUE, useVerbose = FALSE)

    expect_match(as$entity[[1]]$Barcode, ta1$entity[[1]]$Barcode)
  })

  CoreAPIV2::logOut(con$coreApi)
})
