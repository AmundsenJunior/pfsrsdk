#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for Experiment creation.
#'
context("Tests for case")

test_that(paste("test case evaluates the first TRUE LHS"), {
  case(
    FALSE ~ {
      answer <- "fail"
    },
    TRUE ~ {
      answer <- "pass"
    },
    TRUE ~ {
      answer <- "fail"
    }
  )
  expect_equal(answer, "pass")
})

test_that(paste("test case requires a 2 sided formula i.e (LHS~RHS)"), {
  expect_error({case(
    FALSE ,
    TRUE ~ {
      answer <- "pass"
    },
    TRUE ~ {
      answer <- "fail"
    }
  )}, "Case LHS must be a two-sided formula, not a logical")
})

test_that(paste("test case requires a logical LHS"), {
  expect_error({
    case(
      10L ~ {
        answer <- "pass"
      },
      TRUE ~ {
        answer <- "pass"
      },
      TRUE ~ {
        answer <- "fail"
      }
    )
  }, "Case 10 LHS must be a logical, not integer")
})
