.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pack <- list(
    #please keep this list in order. highest to lowest
    pfs.testedVersions = c("3.0.3","2.7.1"),
    pfs.untestedVersionMessage = "The version of PFS you are connecting to
                                  has not been explicitly tested with this 
                                  package. Proceed with caution.",
    pfs.tested = FALSE
  )
  toset <- !(names(op.pack) %in% names(op))
  if (any(toset)) options(op.pack[toset])
  invisible()
}