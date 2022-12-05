
library(tidyverse)

# read raw box data
raw_boxes <-
  tibble(x = read_lines("2022/day05/2022_day05_input.txt", n_max = 9))

# into a tidy data frame
tidy_boxes <-
  raw_boxes %>%
  mutate(x = str_replace_all(x, "    ", " [ ]") %>%
           str_replace_all("\\[ \\]\\[", "\\[ ] [") %>%
           str_trim()) %>%
  separate(x, "\\] \\[", into = as.character(1:9)) %>%
  mutate(across(everything(), ~str_remove_all(.x, "\\[|\\]") %>%
                  str_replace_all(" ", NA_character_))) %>%
  slice_head(n = 8) %>%
  mutate(row = row_number()) %>%
  pivot_longer(-"row", names_to = "column") %>%
  drop_na()

# nest boxes
nested_boxes <-
  tidy_boxes %>%
  group_by(column) %>%
  summarise(boxes = list(value)) %>%
  mutate(column = as.integer(column))

# convert to list
box_list <- nested_boxes$boxes
for (i in 1:length(box_list)){
  box_list[[i]] <- rev(box_list[[i]])
}

# get instructions
instructions <-
  tibble(x = read_lines("2022/day05/2022_day05_input.txt", skip = 10)) %>%
  extract(x,
          c("move", "from", "to"),
          regex = "move (\\d+) from (\\d+) to (\\d+)",
          convert = TRUE)

# if PART 1, move crates one at a time
# if PART 2, move as a stack
PART = 2

# loop over instructions
for (i in 1:nrow(instructions)){
  from = instructions[i,]$from
  move = instructions[i,]$move
  to = instructions[i,]$to

  tomove <- box_list[[from]][(length(box_list[[from]])-move+1):length(box_list[[from]])]
  if (PART == 1) tomove <- rev(tomove)
  box_list[[from]] <- box_list[[from]][1:(length(box_list[[from]])-move)]
  box_list[[to]] <- c(box_list[[to]], tomove)
}

box_list
