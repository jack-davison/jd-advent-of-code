
library(tidyverse)

# read raw data
raw <-
  read_table("2022/day02/2022_day02_input.txt",
             col_names = c("opp", "me"))


# Part 1 ------------------------------------------------------------------

# function to parse ABCXYZ as R/P/S
parse_rps <- function(vec){
  vec <- gsub("A|X", "R", vec)
  vec <- gsub("B|Y", "P", vec)
  vec <- gsub("C|Z", "S", vec)
  vec
}

# process data
outcomes <-
  raw %>%
  mutate(opp = parse_rps(opp),
         me = parse_rps(me),
         choice_score = case_when(
           me == "S" ~ 3,
           me == "P" ~ 2,
           me == "R" ~ 1
         ),
         win_score = case_when(
           opp == me ~ 3,
           opp == "P" & me == "S" ~ 6,
           opp == "R" & me == "P" ~ 6,
           opp == "S" & me == "R" ~ 6,
           TRUE ~ 0
         ),
         round_score = win_score + choice_score)

# out
sum(outcomes$round_score)


# Part 2 ------------------------------------------------------------------

# functions to get winning/losing RPS from RPS
# (replace with lower case to not gsub RPS repeatedly)
get_loser <- function(x){
  x <- gsub("P", "s", x)
  x <- gsub("S", "r", x)
  x <- gsub("R", "p", x)
  toupper(x)
}
get_winner <- function(x){
  x <- gsub("P", "r", x)
  x <- gsub("S", "p", x)
  x <- gsub("R", "s", x)
  toupper(x)
}

# process data
outcomes_v2 <-
  raw %>%
  mutate(opp = parse_rps(opp),
         win_score = case_when(
           me == "X" ~ 0,
           me == "Y" ~ 3,
           me == "Z" ~ 6
         ),
         me = case_when(
           win_score == 3 ~ opp,
           win_score == 0 ~ get_winner(opp),
           win_score == 6 ~ get_loser(opp)
         ),
         choice_score = case_when(
           me == "S" ~ 3,
           me == "P" ~ 2,
           me == "R" ~ 1
         ),
         round_score = win_score + choice_score)

# out
sum(outcomes_v2$round_score)
