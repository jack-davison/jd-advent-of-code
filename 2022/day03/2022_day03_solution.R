
library(tidyverse)

# get scores for letters
scores <-
  tibble(letter = c(letters, LETTERS)) %>%
  mutate(score = row_number())

raw <- tibble(x = read_lines("2022/day03/2022_day03_input.txt"))


# PART 1 ------------------------------------------------------------------

# split into two backpacks
splits <-
  raw %>%
  mutate(len_compartment = str_length(x)/2,
         comp1 = str_sub(x, 0, len_compartment),
         comp2 = str_sub(x, len_compartment+1, -1)) %>%
  select(-x, -len_compartment) %>%
  mutate(backpack = row_number(), .before = everything())

# get shared letters
shared_letters <-
  splits %>%
  mutate(comp1 = str_split(comp1, ""),
         comp2 = str_split(comp2, "")) %>%
  unnest_longer(comp1) %>%
  unnest_longer(comp2) %>%
  filter(comp1 == comp2) %>%
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
  mutate(group = if_else(row_number() %% 3 == 0, row_number(), NA_integer_)) %>%
  fill(group, .direction = "up") %>%
  group_by(group) %>%
  mutate(backpack = row_number())

# rearrange data
letter_counts <-
  assigned_groups %>%
  mutate(x = str_split(x, "")) %>%
  unnest_longer(x) %>%
  ungroup() %>%
  count(x, group, backpack) %>%
  pivot_wider(names_from = backpack, values_from = n) %>%
  drop_na()

# find score
letter_counts %>%
  left_join(scores, by = c("x" = "letter")) %>%
  pull(score) %>%
  sum()
