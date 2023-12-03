
## NB: this solution is neither good nor quick!

# read input
input <- readr::read_lines("2023/2023-03/2023-03-input.txt")

# tidy into grid
input_grid <-
  tibble::tibble(input) |>
  dplyr::mutate(row = dplyr::row_number(),
                input = stringr::str_split(input, "")) |>
  tidyr::unnest(input) |>
  dplyr::mutate(col = dplyr::row_number(),
                .by = row) |>
  dplyr::mutate(numflag = input %in% 0:9,
                numflag = dplyr::consecutive_id(numflag))

# get things that aren't "." or a number
vals <- unique(input_grid$input)
vals <- vals[!vals %in% c(".", as.character(0:9))]

# Part 1 ------------------------------------------------------------------

# function to flag nearby vals
check_overlap <- function(test_grid) {
  check_grid <-
    tidyr::crossing(
      row = (test_grid$row - 1):(test_grid$row + 1),
      col = (test_grid$col - 1):(test_grid$col + 1)
    )

  check_grid <-
    input_grid |>
    dplyr::semi_join(check_grid, by = dplyr::join_by(row, col)) |>
    dplyr::anti_join(test_grid, by = dplyr::join_by(input, row, col))

  any(check_grid$input %in% vals)
}

# assign flags
input_grid$flag <-
  input_grid |>
  dplyr::mutate(id = dplyr::row_number()) |>
  split(~ id) |>
  purrr::map_vec(check_overlap, .progress = TRUE)

input_grid |>
  dplyr::mutate(numflag = input %in% 0:9,
                numflag = dplyr::consecutive_id(numflag)) |>
  dplyr::filter(any(flag), .by = numflag) |>
  dplyr::filter(!input %in% c(vals, ".")) |>
  dplyr::summarise(num = stringr::str_c(input, collapse = "") |> as.integer(),
                   .by = numflag) |>
  dplyr::pull(num) |>
  sum()


# Part 2 ------------------------------------------------------------------

# just get gears
gear_grid <- dplyr::filter(input_grid, input == "*")

# function to check if a gear is next to two unique numflags
check_gear <- function(test_grid){
  check_grid <-
    tidyr::crossing(
      row = (test_grid$row - 1):(test_grid$row + 1),
      col = (test_grid$col - 1):(test_grid$col + 1)
    )

  check_grid <-
    input_grid |>
    dplyr::semi_join(check_grid, by = dplyr::join_by(row, col)) |>
    dplyr::anti_join(test_grid, by = dplyr::join_by(input, row, col)) |>
    dplyr::filter(input != ".")

  if (dplyr::n_distinct(check_grid$numflag) == 2) {
    return(unique(check_grid$numflag))
  }
}

# get the numflags for gears
pairs <-
  gear_grid |>
  dplyr::mutate(id = dplyr::row_number()) |>
  split(~ id) |>
  purrr::map(check_gear) |>
  purrr::compact()

# reconstruct numbers
nums <-
  input_grid |>
  dplyr::filter(!input %in% c(vals, ".")) |>
  dplyr::summarise(num = stringr::str_c(input, collapse = "") |> as.integer(),
                   .by = numflag)

# function to get gear products
gear_prod <- function(x) {
  x <- dplyr::filter(nums, numflag %in% x)$num
  out <- x[[1]] * x[[2]]
  return(out)
}

# sum gear prods
purrr::map_vec(pairs, gear_prod) |>
  sum()

