#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getAttributesColumnHeaders

context("Tests for getAttributesColumnHeaders")

case(
  grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
    test_that(paste("test getAttributesColumnHeaders() on:", env$auth), {
      expect_warning(
        result <- getAttributesColumnHeaders(con$coreApi,
          attributeList = NULL,
          data$testPocoGetAssocType,
          fullMetadata = FALSE,
          useVerbose = verbose
        ),
        paste("getAttributesColumnHeaders OData action not available in", con$coreApi$semVer)
      )
    })
  },
  grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
    test_that(paste("test getAttributesColumnHeaders() to retrieve all column headers for entity type attributes on:", env$auth), {
      result <- getAttributesColumnHeaders(con$coreApi,
        attributeList = NULL,
        data$testPocoGetAssocType,
        fullMetadata = FALSE,
        useVerbose = verbose
      )

      expect_gt(length(result$entity), 0)
      expect_equal(result$response$status_code, 200)
    })

    test_that(paste("test getAttributesColumnHeaders() for specified attributes on:", env$auth), {
      result <- getAttributesColumnHeaders(con$coreApi,
        data$testPocoAttrList,
        data$testPocoGetAssocType,
        fullMetadata = FALSE,
        useVerbose = verbose
      )

      expect_true(data$testPocoAttrList[[1]] %in% toupper(result$entity), TRUE)
    })

    test_that(paste("test getAttributesColumnHeaders() for invalid attributes on:", env$auth), {
      expect_warning(
        result <- getAttributesColumnHeaders(con$coreApi,
          attributeList = "",
          data$testPocoGetAssocType,
          fullMetadata = FALSE,
          useVerbose = verbose
        ),
        "Invalid list of attributes:"
      )
    })

    test_that(paste("test getAttributesColumnHeaders() with non-existing attribute on:", env$auth), {
      result <- getAttributesColumnHeaders(con$coreApi,
        data$testPocoNonExistingAttr,
        data$testPocoGetAssocType,
        fullMetadata = FALSE,
        useVerbose = verbose
      )

      expect_equal(length(result$entity), 0)
    })
  }
)
