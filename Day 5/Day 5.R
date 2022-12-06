library(tidyverse)

dat <- readLines("./Day 5/input.txt")

# data cleaning was most difficult part :o)
crates_full <- dat[1:8] %>%
  as.data.frame() %>%
  rename(crate = 1) %>%
  mutate(`1` = substr(crate, 1, 4),
         `2` = substr(crate, 5, 8),
         `3` = substr(crate, 9, 12),
         `4` = substr(crate, 13, 16),
         `5` = substr(crate, 17, 20),
         `6` = substr(crate, 21, 24),
         `7` = substr(crate, 25, 28),
         `8` = substr(crate, 29, 32),
         `9` = substr(crate, 33, 36)) %>%
  select(-crate) %>%
  as.matrix()

crates_full[crates_full == "    "] <- " "
crates_full[crates_full == "   "] <- " "

dat_tidy <- dat[11:length(dat)] %>%
  as.data.frame() %>%
  rename(moves = 1)

# part 1

instructions <- dat_tidy %>%
  separate(moves, into=c("amount", "move_from"), sep = "from") %>%
  separate(move_from, into=c("move_from", "move_to"), sep = "to") %>%
  mutate_if(is.character, parse_number)

crates <- crates_full

for(i in 1:nrow(instructions)) {

  amount <- instructions[i,] %>% pull(amount)
  move_from <- instructions[i,] %>% pull(move_from)
  move_to <- instructions[i,] %>% pull(move_to)

  move_from_row <- which(crates[,move_from] != " ")[1:amount]
  move_to_row <- which(crates[,move_to] == " ")
  move_to_row <- rev(move_to_row)[1:amount]
  move_to_row <- move_to_row[!is.na(move_to_row)]

  crates_moving <- crates[move_from_row,move_from]

  crates[move_from_row,move_from] <- " "

  if(length(crates[move_to_row,move_to]) < amount) {
    crates <- rbind(matrix(" ", nrow = amount - length(crates[move_to_row,move_to]), ncol=ncol(crates)), crates)
    move_to_row <- which(crates[,move_to] == " ")
    move_to_row <- rev(move_to_row)[1:amount]
  }

  crates[move_to_row,move_to] <- crates_moving

}

# lots of extra rows, but that's ok, answer is correct
fin_vals <- crates %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(!rowname) %>%
  filter(!value == " ") %>%
  group_by(name) %>%
  slice_head(n=1) %>%
  mutate(value = trimws(str_replace(value, "\\[", "")),
         value = str_replace(value, "\\]", "")) %>%
  pull(value)

paste0(fin_vals, collapse = "")

# part 2

# same as above, just reverse order of crate drop
crates <- crates_full

for(i in 1:nrow(instructions)) {

  amount <- instructions[i,] %>% pull(amount)
  move_from <- instructions[i,] %>% pull(move_from)
  move_to <- instructions[i,] %>% pull(move_to)

  move_from_row <- which(crates[,move_from] != " ")[1:amount]
  move_to_row <- which(crates[,move_to] == " ")
  move_to_row <- rev(move_to_row)[1:amount]
  move_to_row <- move_to_row[!is.na(move_to_row)]

  crates_moving <- crates[move_from_row,move_from]

  crates[move_from_row,move_from] <- " "

  if(length(crates[move_to_row,move_to]) < amount) {
    crates <- rbind(matrix(" ", nrow = amount - length(crates[move_to_row,move_to]), ncol=ncol(crates)), crates)
    move_to_row <- which(crates[,move_to] == " ")
    move_to_row <- rev(move_to_row)[1:amount]
  }

  crates[move_to_row,move_to] <- rev(crates_moving) # rev() is single change from above code

}

fin_vals <- crates %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(!rowname) %>%
  filter(!value == " ") %>%
  group_by(name) %>%
  slice_head(n=1) %>%
  mutate(value = trimws(str_replace(value, "\\[", "")),
         value = str_replace(value, "\\]", "")) %>%
  pull(value)

paste0(fin_vals, collapse = "")

