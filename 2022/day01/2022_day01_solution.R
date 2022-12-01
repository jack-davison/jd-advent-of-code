

library(tidyverse)

# read inputs
x = read_lines("2022/day01/2022_day01_input.txt")

# rle to get individual elves, then group_by/summarise
elves <-
  tibble(x = x) %>%
  group_by(elf = rle(x == "") %>% magrittr::extract2("lengths") %>% rep(seq_along(.), .)) %>%
  mutate(x = as.numeric(x)) %>%
  summarise(x = sum(x)) %>%
  arrange(desc(x))

# PART ONE (highest calorie elf)
elves[1,]$x

# PART TWO (sum of top three elves)
sum(elves[1:3,]$x)
