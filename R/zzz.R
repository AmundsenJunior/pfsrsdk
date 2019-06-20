.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pack <- list(
    pfs.testedVersions = utils::packageDescription(pkgname)$PfsCompatibility,
    pfs.untestedVersionMessage = "The version of PFS you are connecting to
                                  has not been explicitly tested with this 
                                  package. Proceed with caution.",
    pfs.tested = FALSE
  )
  toset <- !(names(op.pack) %in% names(op))
  if (any(toset)) options(op.pack[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if ("CoreAPIV2" %in% (.packages())) {
    packageStartupMessage("Please detach CoreAPIV2 from the environment (via detach() or unloadNamespace()).")
  }
}
