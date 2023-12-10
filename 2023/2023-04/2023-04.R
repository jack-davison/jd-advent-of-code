
# read input
input <- readr::read_fwf("2023/2023-04/2023-04-input.txt")

# tidy into long format
input_long <-
  input |>
  tidyr::pivot_longer(X3:X12, names_to = "win_id", values_to = "win_val") |>
  tidyr::pivot_longer(X14:X38, names_to = "num_id", values_to = "num_val") |>
  dplyr::mutate(card_id = readr::parse_number(X2), .before = win_id) |>
  dplyr::select(-(1:3))


# Part 1 ------------------------------------------------------------------

input_long |>
  dplyr::mutate(flag = win_val == num_val) |>
  dplyr::filter(flag) |>
  dplyr::count(card_id, flag) |>
  dplyr::mutate(score = 1 * (2 ^ (n - 1))) |>
  dplyr::pull(score) |> sum()
