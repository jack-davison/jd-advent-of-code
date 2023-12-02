
input <- readr::read_lines("2023/2023-02/2023-02-input.txt")

# tidy
input_tidy <-
  tibble::tibble(input) |>
  tidyr::separate_wider_delim(input, ": ", names = c("game", "balls")) |>
  tidyr::separate_longer_delim(balls, "; ") |>
  dplyr::mutate(round = row_number(),
                .by = game,
                .before = balls) |>
  tidyr::separate_longer_delim(balls, ", ") |>
  tidyr::separate_wider_delim(balls, " ", names = c("num", "color")) |>
  dplyr::mutate(game = readr::parse_number(game),
                num = as.integer(num))


# Part 1 ------------------------------------------------------------------

input_tidy |>
  # flag if too many cubes
  dplyr::mutate(
    flag = dplyr::case_when(
      color == "blue" & num > 14 ~ FALSE,
      color == "red" & num > 12 ~ FALSE,
      color == "green" & num > 13 ~ FALSE,
      .default = TRUE
    )
  ) |>
  # drop games with any flag
  dplyr::filter(all(flag), .by = game) |>
  # sum game numbers
  dplyr::distinct(game) |>
  dplyr::pull(game) |>
  sum()


# Part 2 ------------------------------------------------------------------

input_tidy |>
  # get colour cols
  tidyr::pivot_wider(names_from = color, values_from = num, values_fill = list(num = 0)) |>
  # get max of each per game
  dplyr::summarise(dplyr::across(green:blue, max), .by = game) |>
  # get product of all cols and sum
  dplyr::mutate(prod = green * red * blue) |>
  dplyr::pull(prod) |>
  sum()
