
#### Copy a word in a vector until next word occurs ####
copy_till_next_occur <- function(x) {
  pos <- which(!is.na(x))

  for (i in seq_along(pos)) {
    end_pos <- pos[length(pos)]

    if (pos[i] != end_pos) {
      x[pos[i]:(pos[i + 1] - 1)] <- x[pos[i]]
    }
    if (pos[i] == end_pos) {
      x[pos[i]:length(x)] <- x[end_pos]
    }
  }
  x
}

# test <- c(
#   NA_character_ ,
#   "ABC",
#   NA_character_,
#   NA_character_,
#   "DEF",
#   NA_character_,
#   "GHI",
#   NA_character_
# )
# copy_till_next_occur(x = test)

#### Capitalize first letter ####
simpleCap <- function(x) {
  x <- x[!is.na(x)]
  s <- tolower(x)
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

#### Google search ####
query_google <- function(x) {
  invisible(lapply(
    x,
    function(y) {
      utils::browseURL(url = paste0("https://google.com/search?q=", y))
    }
  ))
}
