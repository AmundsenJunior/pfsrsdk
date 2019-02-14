#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for entity creation.
#'
context("Tests for createEntity")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("test createEntity() for POCO with Boolean attributes on:", env$auth), {
  out <- CoreAPIV2::getEntityMetadata(con$coreApi, data$testPocoCreateBoolType, useVerbose = verbose)

  body <- out$template
  for (name in names(data$testPocoCreateBoolAttrList)) {
    body[[name]] <- data$testPocoCreateBoolAttrList[[name]]
  }
  for (name in names(data$testPocoCreateBoolAssocList)) {
    body[[paste0(name, "@odata.bind")]] <- data$testPocoCreateBoolAssocList[[name]]
  }

  return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = data$testPocoCreateBoolType, body = body, useVerbose = TRUE)
  expect_equal(httr::status_code(return$response), 201)

  barcode <- return$entity$Barcode
  b <- CoreAPIV2::getEntityByBarcode(con$coreApi, data$testPocoCreateBoolType, barcode, useVerbose = verbose)$entity
  expect_equal(b[[names(data$testPocoCreateBoolAttrList[1])]], data$testPocoCreateBoolAttrList[[1]], all = verbose)
})

test_that(paste("test createEntity() for POCO with Decimal attributes on:", env$auth), {
  out <- CoreAPIV2::getEntityMetadata(con$coreApi, data$testPocoCreateDecType, useVerbose = verbose)

  body <- out$template
  for (name in names(data$testPocoCreateDecAttrList)) {
    body[[name]] <- data$testPocoCreateDecAttrList[[name]]
  }
  for (name in names(data$testPocoCreateDecAssocList)) {
    body[[paste0(name, "@odata.bind")]] <- data$testPocoCreateDecAssocList[[name]]
  }

  return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = data$testPocoCreateDecType, body = body, useVerbose = TRUE)
  expect_equal(httr::status_code(return$response), 201)

  barcode <- return$entity$Barcode
  b <- CoreAPIV2::getEntityByBarcode(con$coreApi, data$testPocoCreateDecType, barcode, useVerbose = verbose)$entity
  expect_equal(b[[names(data$testPocoCreateDecAttrList[1])]], data$testPocoCreateDecAttrList[[1]], all = verbose)
})

test_that(paste("test createEntity() for POCO with Integer attributes on:", env$auth), {
  out <- CoreAPIV2::getEntityMetadata(con$coreApi, data$testPocoCreateIntType, useVerbose = verbose)

  body <- out$template
  for (name in names(data$testPocoCreateIntAttrList)) {
    body[[name]] <- data$testPocoCreateIntAttrList[[name]]
  }
  for (name in names(data$testPocoCreateIntAssocList)) {
    body[[paste0(name, "@odata.bind")]] <- data$testPocoCreateIntAssocList[[name]]
  }

  return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = data$testPocoCreateIntType, body = body, useVerbose = TRUE)
  expect_equal(httr::status_code(return$response), 201)

  barcode <- return$entity$Barcode
  b <- CoreAPIV2::getEntityByBarcode(con$coreApi, data$testPocoCreateIntType, barcode, useVerbose = verbose)$entity
  expect_equal(b[[names(data$testPocoCreateIntAttrList[1])]], data$testPocoCreateIntAttrList[[1]], all = verbose)
})

test_that(paste("test createEntity() for POCO with String attributes on:", env$auth), {
  out <- CoreAPIV2::getEntityMetadata(con$coreApi, data$testPocoCreateStrType, useVerbose = verbose)

  body <- out$template
  for (name in names(data$testPocoCreateStrAttrList)) {
    body[[name]] <- data$testPocoCreateStrAttrList[[name]]
  }
  for (name in names(data$testPocoCreateStrAssocList)) {
    body[[paste0(name, "@odata.bind")]] <- data$testPocoCreateStrAssocList[[name]]
  }

  return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = data$testPocoCreateStrType, body = body, useVerbose = TRUE)
  expect_equal(httr::status_code(return$response), 201)

  barcode <- return$entity$Barcode
  b <- CoreAPIV2::getEntityByBarcode(con$coreApi, data$testPocoCreateStrType, barcode, useVerbose = verbose)$entity
  expect_equal(b[[names(data$testPocoCreateStrAttrList[1])]], data$testPocoCreateStrAttrList[[1]], all = verbose)
})
