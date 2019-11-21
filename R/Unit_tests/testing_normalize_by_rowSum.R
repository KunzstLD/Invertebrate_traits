#_______________________________________________________________
#### Unit test for normalize_by_rowSum ####
# tests the creation of pattern from column names
#______________________________________________________________

fun <- function(x, non_trait_cols) {
  
  # get trait names & create pattern for subset
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_names_pattern <-
    grep(pat, names(x), value = TRUE, invert = TRUE) %>%
    sub("\\_.*|\\..*", "", .) %>%
    unique() %>%
    paste0("^", .)
}

# expected values
value_to_expect <- c(
  "^bf",
  "^dev",
  "^feed",
  "^locom",
  "^ovip",
  "^ph",
  "^resp",
  "^size",
  "^stage",
  "^temp",
  "^volt"
)

#test 
test_that(
  "testing creation of trait names pattern",
  expect_equal(fun(x = Trait_AUS, 
                   non_trait_cols = c("unique_id",
                                      "family",
                                      "genus",
                                      "species",
                                      "order")),
               value_to_expect)
)