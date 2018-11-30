#' @author Adam Wheeler adam.j.wheeler@accenture.com
#' @description \code Test for ODATAcleanName.

context("Tests for ODATAcleanName")

# Completed regression for 5.3.8 and 6.0.1



test_that(paste("ODATAcleanName() replaces spaces with '_' and converts everything to uppercase "), {
  
  CleanedUp <- CoreAPIV2::ODATAcleanName("1conVert me_toUPPERcase and make spaces underscores What about numbers 123453 32 _ and symbols!@#$%^&*()_+ -=`~ ")
  
  expect_equivalent(CleanedUp,"_1conVert_me_toUPPERcase_and_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+_-=`~_" )
  
})