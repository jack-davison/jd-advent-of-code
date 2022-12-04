
library(tidyverse)

# read data
raw <- tibble(x = read_lines("2022/day04/2022_day04_input.txt"))

# split strings into integers
starts_ends <-
  raw %>%
  separate(x, c("x", "y"), sep = ",") %>%
  separate(x, c("xa", "xb"), sep = "-", convert = TRUE) %>%
  separate(y, c("ya", "yb"), sep = "-", convert = TRUE) %>%
  mutate(pair = row_number())

# PART 1 (completely contained)
filter(starts_ends,
       (ya >= xa & yb <= xb) | (xa >= ya & xb <= yb)) %>%
  nrow()

# PART 2 (any overlap)
starts_ends %>%
  rowwise() %>%
  # get full list of numbers
  mutate(x = list(xa:xb),
         y = list(ya:yb),
         .keep = "unused") %>%
  # compare each combo
  unnest_longer(x) %>%
  unnest_longer(y) %>%
  # filter for overlaps
  filter(x == y) %>%
  # get number of distinct pairs
  pull(pair) %>%
  n_distinct()
