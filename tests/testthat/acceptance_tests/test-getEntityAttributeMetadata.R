#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getEntityAttributeMetadata

context("Tests for getEntityAttributeMetadata")

case(
  grepl("[0-2]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
    test_that(paste("test getEntityAttributeMetadata() on:", env$auth), {
      expect_warning(
        result <- getEntityAttributeMetadata(con$coreApi,
          data$testPocoGetAssocType,
          fullMetadata = FALSE,
          useVerbose = verbose
        ),
        paste("getEntityAttributeMetadata OData action not available in", con$coreApi$semVer)
      )
    })
  },
  grepl("[3-9]+\\.[0-9]+\\.[0-9]+", con$coreApi$semVer) ~ {
    test_that(paste("test getEntityAttributeMetadata() on:", env$auth), {
      result <- getEntityAttributeMetadata(con$coreApi,
        data$testPocoGetAssocType,
        fullMetadata = FALSE,
        useVerbose = verbose
      )

      expect_equal(result$response$status_code, 200)

      expect_gt(
        length(
          lapply(
            result$entity,
            FUN = function(x)
              x$AttributeName
          )
        ), 0
      )
    })

    test_that(paste("getEntityAttributeMetadata returns successful with fullMetadata on:", env$auth), {
      result <- getEntityAttributeMetadata(con$coreApi,
        data$testPocoGetAssocType,
        fullMetadata = TRUE,
        useVerbose = verbose
      )

      expect_true(!is.null(result$entity[[1]]$`Id@odata.type`))
    })
  }
)
