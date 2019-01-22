
#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description Tests for attributeCleanName.
#'

context("Tests for attributeCleanName")

# Completed regression for 5.3.8 and 6.0.1

test_that(paste("attributeCleanName() replaces spaces percent and ampersands with '_' and converts everything to uppercase "), {
  cleanedUp <- CoreAPIV2::attributeCleanName(" 1conVert me_toUPPERcase and make spaces underscores What about numbers 123453 32 _ and symbols!@#$%^&*()_+ -=`~ ")

  expect_equivalent(cleanedUp, "_1CONVERT_ME_TOUPPERCASE_AND_MAKE_SPACES_UNDERSCORES_WHAT_ABOUT_NUMBERS_123453_32___AND_SYMBOLS!@#$_^_*()_+_-=`~_")
})
