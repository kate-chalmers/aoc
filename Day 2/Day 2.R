library(tidyverse)

dat <- read_csv("./Day 2/input.txt", skip_empty_rows = F)
dat <- rbind(colnames(dat),dat)
dat <- dat %>% rename(vals = 1)

# part 1

opponent_choice <- data.frame("opp" = c("A", "B", "C"),
                              "opp_choice" = c("Rock", "Paper", "Scissors"),
                              "opp_point" = c(1, 2, 3))

my_choices <- data.frame("me" = c("X", "Y", "Z"),
                         "me_choice" = c("Rock", "Paper", "Scissors"),
                         "me_point" = c(1, 2, 3))

dat %>%
  separate(vals, into=c("opp", "me"), sep=" ") %>%
  left_join(., opponent_choice, by="opp") %>%
  left_join(., my_choices, by = "me") %>%
  mutate(winner = ifelse(opp_choice == "Paper" & me_choice == "Scissors" | opp_choice == "Rock" & me_choice == "Paper" | me_choice == "Rock" & opp_choice == "Scissors", "me",
                         ifelse(opp_choice == "Paper" & me_choice == "Rock" | opp_choice == "Scissors" & me_choice == "Paper" | opp_choice == "Rock" & me_choice == "Scissors", "opp",
                                ifelse(opp_choice == me_choice, "draw", "check case")))) %>%
  mutate(add_points =
           case_when(
             winner == "opp" ~ 0,
             winner == "draw" ~ 3,
             winner == "me" ~ 6
             )
           ) %>%
  select(me_point, add_points) %>%
  mutate(total = me_point + add_points,
         fin = sum(total)) %>%
  distinct(fin) %>%
  pull(fin)


# part 2

my_choices_meaning <- data.frame("me" = c("X", "Y", "Z"),
                                 "winner" = c("opp", "draw", "me"))

my_choices <- data.frame("me_choice" = c("Rock", "Paper", "Scissors"),
                         "me_point" = c(1, 2, 3))


dat %>%
  separate(vals, into=c("opp", "me"), sep=" ") %>%
  left_join(., opponent_choice, by="opp") %>%
  left_join(., my_choices_meaning, by = "me") %>%
  mutate(me_choice = ifelse(winner == "me" & opp_choice == "Paper", "Scissors",
                            ifelse(winner == "me" & opp_choice == "Rock", "Paper",
                                   ifelse(winner == "me" & opp_choice == "Scissors", "Rock",
                                          ifelse(winner == "draw", opp_choice,
                                                 ifelse(winner == "opp" & opp_choice == "Rock", "Scissors",
                                                        ifelse(winner == "opp" & opp_choice == "Scissors", "Paper",
                                                               ifelse(winner == "opp" & opp_choice == "Paper", "Rock", "check case")))))))) %>%
  left_join(., my_choices, by = c("me_choice")) %>%
  mutate(add_points =
           case_when(
             winner == "opp" ~ 0,
             winner == "draw" ~ 3,
             winner == "me" ~ 6
           )
  ) %>%
  select(me_point, add_points) %>%
  mutate(total = me_point + add_points,
         fin = sum(total)) %>%
  distinct(fin) %>%
  pull(fin)




