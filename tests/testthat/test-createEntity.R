
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for entity creation.
#'
context("Tests for createEntity")

# Completed regression for 5.3.8 and 6.0.1

lapply(environments, function(x) {
  con <- Connect(x)
  
  test_that(paste("test createEntity() for POCO with Boolean attributes on:", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCOCREATEBOOL, useVerbose = FALSE)

    body <- out$template
    for(name in names(TESTPOCOCREATEBOOLATTRLIST)) { # FIX: unable to use lapply instead of for
        body[[name]] <- TESTPOCOCREATEBOOLATTRLIST[[name]]
    }
    for(name in names(TESTPOCOCREATEBOOLASSOCLIST)) {
      body[[paste0(name, "@odata.bind")]] <- TESTPOCOCREATEBOOLASSOCLIST[[name]]
    }
    
    return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = TESTPOCOCREATEBOOL, body = body, useVerbose = TRUE)
    expect_equal(httr::status_code(return$response), 201)

    barcode <- return$entity$Barcode
    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, TESTPOCOCREATEBOOL, barcode, useVerbose = verbose)$entity
    expect_equal(b[[names(TESTPOCOCREATEBOOLATTRLIST[1])]], TESTPOCOCREATEBOOLATTRLIST[[1]], all = verbose)
  })

  test_that(paste("test createEntity() for POCO with Decimal attributes on:", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCOCREATEDEC, useVerbose = FALSE)

    body <- out$template
    for(name in names(TESTPOCOCREATEDECATTRLIST)) { # FIX: unable to use lapply instead of for
      body[[name]] <- TESTPOCOCREATEDECATTRLIST[[name]]
    }
    for(name in names(TESTPOCOCREATEDECASSOCLIST)) {
      body[[paste0(name, "@odata.bind")]] <- TESTPOCOCREATEDECASSOCLIST[[name]]
    }
    
    return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = TESTPOCOCREATEDEC, body = body, useVerbose = TRUE)
    expect_equal(httr::status_code(return$response), 201)

    barcode <- return$entity$Barcode
    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, TESTPOCOCREATEDEC, barcode, useVerbose = verbose)$entity
    expect_equal(b[[names(TESTPOCOCREATEDECATTRLIST[1])]], TESTPOCOCREATEDECATTRLIST[[1]], all = verbose)
  })
  
  test_that(paste("test createEntity() for POCO with Integer attributes on:", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCOCREATEINT, useVerbose = FALSE)

    body <- out$template
    for(name in names(TESTPOCOCREATEINTATTRLIST)) { # FIX: unable to use lapply instead of for
      body[[name]] <- TESTPOCOCREATEINTATTRLIST[[name]]
    }
    for(name in names(TESTPOCOCREATEINTASSOCLIST)) {
      body[[paste0(name, "@odata.bind")]] <- TESTPOCOCREATEINTASSOCLIST[[name]]
    }
    
    return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = TESTPOCOCREATEINT, body = body, useVerbose = TRUE)
    expect_equal(httr::status_code(return$response), 201)

    barcode <- return$entity$Barcode
    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, TESTPOCOCREATEINT, barcode, useVerbose = verbose)$entity
    expect_equal(b[[names(TESTPOCOCREATEINTATTRLIST[1])]], TESTPOCOCREATEINTATTRLIST[[1]], all = verbose)
  })
  
  test_that(paste("test createEntity() for POCO with String attributes on:", x), {
    out <- CoreAPIV2::getEntityMetadata(con$coreApi, TESTPOCOCREATESTR, useVerbose = FALSE)

    body <- out$template
    for(name in names(TESTPOCOCREATESTRATTRLIST)) { # FIX: unable to use lapply instead of for
      body[[name]] <- TESTPOCOCREATESTRATTRLIST[[name]]
    }
    for(name in names(TESTPOCOCREATESTRASSOCLIST)) {
      body[[paste0(name, "@odata.bind")]] <- TESTPOCOCREATESTRASSOCLIST[[name]]
    }
    
    return <- CoreAPIV2::createEntity(coreApi = con$coreApi, entityType = TESTPOCOCREATESTR, body = body, useVerbose = TRUE)
    expect_equal(httr::status_code(return$response), 201)

    barcode <- return$entity$Barcode
    b <- CoreAPIV2::getEntityByBarcode(con$coreApi, TESTPOCOCREATESTR, barcode, useVerbose = verbose)$entity
    expect_equal(b[[names(TESTPOCOCREATESTRATTRLIST[1])]], TESTPOCOCREATESTRATTRLIST[[1]], all = verbose)
  })
  
  CoreAPIV2::logOut(con$coreApi)
})
