#' case - Performs a vectorized IF
#'
#' \code{case} Performs a vectorized IF
#' @param ... A set of two sided formulas as LHS~RHS where LHS is logical and RHS is any closure
#' @return Nothing is returned the first case to evaluate as true will execute in the parent environment
#' This function is purely used for its side effects. 
#' @export
#' @examples
#' \dontrun{
#' case(
#'    grepl("[0-2]+\\.[0-9]+\\.[0-9]+","3.7.1") ~ {
#'      body <- list("a",
#'                   "b")
#'    },
#'    grepl("[3-9]+\\.[0-9]+\\.[0-9]+","3.7.1") ~ {
#'      body <- list("b",
#'                   "a")
#'    } 
#')
#'}
#' @author Adam Wheeler, adam.j.wheeler@accenture.com
#' @description \code{case} Performs a vectorized IF

case <- function (...) 
{
  cases <- rlang::list2(...)
  n <- length(cases)
  if (n == 0) {
    abort("No cases provided")
  }
  query <- vector("list", n)
  value <- vector("list", n)
  
  
  query <-  lapply(cases, function(f){
    
    if (!inherits(f, "formula") || length(f) != 3) {
      abort(paste("Cases must be a two-sided formula, not a", typeof(f)))
    }
    query <- eval(f[[2]], envir = environment(f))
    if (!is.logical(query)) {
      abort(paste("Case",f[[2]], "LHS must be a logical, not", typeof(query)))
    }
    query
  }
  )
  f <- cases[[min(which(query==TRUE))]]
  if (!is.null(f)) eval(f[[3]], envir = environment(f))
}
