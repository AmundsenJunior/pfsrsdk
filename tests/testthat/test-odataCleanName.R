#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Test for odataCleanName.

context("Tests for odataCleanName")

name <- "1 make spaces underscores What about numbers 123453 32 _ and symbols!@#$%^&*()_+ -=`~ "

test_that(paste("odataCleanName() replaces spaces with '_'"), {
  cleanedUp <- odataCleanName(name)
  expect_equivalent(cleanedUp, "_1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+__=`~_")
})

test_that(paste("odataCleanName() applies the same substitutions for an unknown refType"), {
  cleanedUp <- odataCleanName(name, "alias")
  expect_equivalent(cleanedUp, "1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+__=`~_")
})

test_that(paste("odataCleanName() prepends an underscore for refTypes of odataObject that start with a number"), {
  cleanedUp <- odataCleanName(name, "odataObject")
  expect_equivalent(cleanedUp, "_1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+__=`~_")
})

test_that(paste("odataCleanName() leaves dashes ('-') intact for tenant refTypes"), {
  cleanedUp <- odataCleanName(name, "tenant")
  expect_equivalent(cleanedUp, "1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+_-=`~_")
})

teardown({
  name <- NULL
})
