
library(tidyverse)

# read raw data
raw <- tibble(x = read_lines("2022/day08/2022_day08_input.txt"))

# turn into tidy tbl
tidy <-
  raw %>%
  separate(x, into = as.character(0:99), sep = "", convert = TRUE) %>%
  mutate(row = row_number()) %>%
  pivot_longer(-row, names_to = "col", names_transform = list(col = as.integer)) %>%
  filter(value != "")

# for fun - plot
tidy %>%
  ggplot(aes(row, col)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c(option = "mako") +
  coord_equal(expand = FALSE) +
  theme_void()

# PART 1
# function to check visibility of one row
check_visibility <- function(i){
  print(i)
  dat <- tidy[i, ]

  see_from_left <- filter(tidy, col < dat$col, row == dat$row)$value
  see_from_left <- dat$value > max(see_from_left)

  see_from_right <- filter(tidy, col > dat$col, row == dat$row)$value
  see_from_right <- dat$value > max(see_from_right)

  see_from_top <- filter(tidy, row < dat$row, col == dat$col)$value
  see_from_top <- dat$value > max(see_from_top)

  see_from_bottom <- filter(tidy, row > dat$row, col == dat$col)$value
  see_from_bottom <- dat$value > max(see_from_bottom)

  any(see_from_left, see_from_right, see_from_top, see_from_bottom)
}

# run on all rows
vec <- purrr::map_lgl(1:nrow(tidy), check_visibility)

# get sum
sum(vec)

# PART 2
# TODO
