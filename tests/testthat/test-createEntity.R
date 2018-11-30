
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description Tests for entity creation.
#' 
context("Tests for createEntity")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {

  con <- Connect(x)
  test_that(paste("test createEntity() on: ", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCO, useVerbose = FALSE)
    ta1 <- CoreAPIV2::getEntityByName(con$coreApi, POCOASSOC, POCOASSOC1NAME,FALSE,FALSE)

    body <- out$template

    body[["TST_STRING"]] <- "ACME"

    body[["TST_INTEGER"]] <- 3

    body[["TST_BOOL"]] <- T

    body[["TST_FILE"]] <- NULL
    
    body[[paste0(ASSOCIATIONCONTEXTLISTNAME,"@odata.bind")]] <- paste0(POCOASSOC, "('", ta1$entity[[1]]$Barcode,"')")

    return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = TESTPOCO, body = body, useVerbose = TRUE)

    barcode <- return$entity$Barcode


    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, TESTPOCO, barcode, useVerbose = verbose)$entity

    expect_match(b$Barcode, barcode, all = verbose)

    expect_match(b$TST_STRING, "ACME", all = verbose)

  })

  CoreAPIV2::logOut(con$coreApi)
})
