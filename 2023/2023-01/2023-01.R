
input <- readr::read_lines("2023/2023-01/2023-01-input.txt")

# Part 1 ------------------------------------------------------------------

# extract first digits
first <- stringr::str_extract(input, "\\d")

# extract last digits
last <- stringr::str_extract(input, "\\d(?=\\D*$)")

# convert, combine, sum
sum(as.integer(paste(first, last, sep = "")))


# Part 2 ------------------------------------------------------------------

# define chars and numbers
chr <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
num <- as.character(1:9)

#' Function to replace an english integer name with its number
#' @param x a character vector
replace_num <- function(x) {
  for (i in 1:length(chr)) {
    x <- stringr::str_replace(x, chr[[i]], num[[i]])
  }
  return(x)
}

# construct regex
rx <- paste0("(?=(\\d|", paste(chr, collapse = "|"), "))")

# find matches
matches <-
  stringr::str_match_all(input, rx) |>
  purrr::map( ~ .x[, 2]) |>
  purrr::map(replace_num)

# get first and last nums
first <- purrr::map_vec(matches, ~.x[[1]])
last <- purrr::map_vec(matches, ~.x[[length(.x)]])

# convert, combine, sum
sum(as.integer(paste(first, last, sep = "")))
