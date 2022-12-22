library(tidyverse)

dat <- readLines("./Day 8/input.txt")

# part 1

dat_tidy <- str_split_fixed(dat, "", nchar(dat)[1])

num_col <- ncol(dat_tidy)
num_row <- nrow(dat_tidy)

checkerFunction <- function(x, y, tester, group) {
  if(all(val > tester)) {
    temp <- data.frame("x" = x, "y" = y, "stat" = paste0("visible ", group))
  } else {
    temp <- data.frame("x" = x, "y" = y, "stat" = paste0("not visible ", group))
  }
  return(temp)
}

df_fin <- c()

for(x in 2:(num_row - 1)) {
  for(y in 2:(num_col - 1)) {
    val <- dat_tidy[x,y] %>% as.numeric()

    bottom_vals <- dat_tidy[(x+1):num_col,y] %>% as.numeric()
    top_vals <- dat_tidy[1:(x-1),y] %>% as.numeric()
    right_vals <- dat_tidy[x,(y+1):num_row] %>% as.numeric()
    left_vals <- dat_tidy[x,1:(y-1)] %>% as.numeric()

    bottom_vals <- checkerFunction(x, y, bottom_vals, "bottom")
    top_vals <- checkerFunction(x, y, top_vals, "top")
    right_vals <- checkerFunction(x, y, right_vals, "right")
    left_vals <- checkerFunction(x, y, left_vals, "left")

    df_fin <- rbind(df_fin, bottom_vals, top_vals, right_vals, left_vals)

  }
}

inner_trees <- df_fin %>%
  filter(!grepl("not", stat)) %>%
  mutate(stat = str_extract(stat, "visible")) %>%
  distinct() %>%
  nrow()

top_edge <- dat_tidy[1,] %>% length() - 1
left_edge <- dat_tidy[,1] %>% length() - 1
bottom_edge <- dat_tidy[num_row,] %>% length() - 1
right_edge <- dat_tidy[,num_col] %>% length() - 1

outter_trees <- top_edge + left_edge + bottom_edge + right_edge

inner_trees + outter_trees


# part 2

dat_tidy <- str_split_fixed(dat, "", nchar(dat)[1])

num_col <- ncol(dat_tidy)
num_row <- nrow(dat_tidy)

scenicViewer <- function(x, y, tester, group) {
  counter <- 0
  for(i in 1:length(tester)) {
    check <- tester[i]
    if(check >= val) {
      counter <- counter + 1
      break
    } else {
      counter <- counter + 1
    }
  }
  temp <- data.frame("x" = x, "y" = y, "stat" = paste0("scenic count ", group), "scenic" = counter)
  return(temp)
}

df_fin <- c()
for(x in 2:(num_row-1)) {
  for(y in 2:(num_col-1)) {

    val <- dat_tidy[x,y] %>% as.numeric()

    row_start <- x + 1
    row_end <- x - 1
    col_start <- y + 1
    col_end <- y - 1

    bottom_vals <- dat_tidy[row_start:num_row,y] %>% as.numeric()
    top_vals <- dat_tidy[row_end:1,y] %>% as.numeric()
    right_vals <- dat_tidy[x,col_start:num_col] %>% as.numeric()
    left_vals <- dat_tidy[x,col_end:1] %>% as.numeric()

    bottom_vals <- scenicViewer(x, y, bottom_vals, "bottom") %>% pull(scenic)
    top_vals <- scenicViewer(x, y, top_vals, "top") %>% pull(scenic)
    right_vals <- scenicViewer(x, y, right_vals, "right") %>% pull(scenic)
    left_vals <- scenicViewer(x, y, left_vals, "left") %>% pull(scenic)

    scenic_view <- bottom_vals * top_vals * right_vals * left_vals

    temp <- data.frame("x" = x, "y" = y, "scenic" = scenic_view)

    df_fin <- rbind(df_fin, temp)
  }
}

df_fin %>%
  filter(scenic == max(scenic)) %>%
  pull(scenic)


