
library(tidyverse)

raw <- read_lines("2022/day06/2022_day06_input.txt")

# split line into list
lst <- str_split(raw, "")[[1]]

# PART 1 - list of 4
tibble(x = lst) %>%
  mutate(x1 = lag(x, n = 1),
         x2 = lag(x, n = 2),
         x3 = lag(x, n = 3),
         n = row_number()) %>%
  rowwise() %>%
  mutate(all = list(c(x, x1, x2, x3)),
         n_d = n_distinct(all)) %>%
  drop_na() %>%
  filter(n_d == 4)

# PART2 - list of 14
# terrible solution - revisit
tibble(x = lst) %>%
  mutate(
    x1 = lag(x, n = 1),
    x2 = lag(x, n = 2),
    x3 = lag(x, n = 3),
    x4 = lag(x, n = 4),
    x5 = lag(x, n = 5),
    x6 = lag(x, n = 6),
    x7 = lag(x, n = 7),
    x8 = lag(x, n = 8),
    x9 = lag(x, n = 9),
    x10 = lag(x, n = 10),
    x11 = lag(x, n = 11),
    x12 = lag(x, n = 12),
    x13 = lag(x, n = 13),
    n = row_number()
  ) %>%
  rowwise() %>%
  mutate(all = list(c(
    x, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13
  )),
  n_d = n_distinct(all)) %>%
  drop_na() %>%
  filter(n_d == 14)

