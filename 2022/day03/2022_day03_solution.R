
library(tidyverse)

# get scores for letters
scores <-
  tibble(letter = c(letters, LETTERS)) %>%
  mutate(score = row_number())

# read input
raw <- tibble(x = read_lines("2022/day03/2022_day03_input.txt"))

# PART 1 ------------------------------------------------------------------

# split into two compartments
splits <-
  raw %>%
  # split into two compartments
  mutate(len_compartment = str_length(x)/2,
         comp1 = str_sub(x, 0, len_compartment),
         comp2 = str_sub(x, len_compartment+1, -1)) %>%
  select(-x, -len_compartment) %>%
  # get backpack number (useful for checking)
  mutate(backpack = row_number(), .before = everything())

# get shared letters
shared_letters <-
  splits %>%
  # split into individual strings
  mutate(comp1 = str_split(comp1, ""),
         comp2 = str_split(comp2, "")) %>%
  # put each letter on one row
  unnest_longer(comp1) %>%
  unnest_longer(comp2) %>%
  # get matching letters
  filter(comp1 == comp2) %>%
  # unique rows
  distinct()

# find score
shared_letters %>%
  left_join(scores, by = c("comp2" = "letter")) %>%
  pull(score) %>%
  sum()


# PART 2 ------------------------------------------------------------------

# assign groups / backpacks
assigned_groups <-
  raw %>%
  # assign groups using modulus of 3
  mutate(group = if_else(row_number() %% 3 == 0, row_number(), NA_integer_)) %>%
  fill(group, .direction = "up") %>%
  # get backpack number per group
  group_by(group) %>%
  mutate(backpack = row_number())

# count letters
letter_counts <-
  assigned_groups %>%
  # split and unnest (as in Part 1)
  mutate(x = str_split(x, "")) %>%
  unnest_longer(x) %>%
  ungroup() %>%
  # count to get unique letter-group-backpack combos
  count(x, group, backpack) %>%
  # put each backpack in a different column
  pivot_wider(names_from = backpack, values_from = n) %>%
  # drop missing to get letters in every backpack
  drop_na()

# find score
letter_counts %>%
  left_join(scores, by = c("x" = "letter")) %>%
  pull(score) %>%
  sum()
