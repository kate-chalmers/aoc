library(tidyverse)

dat <- read_csv("./Day 3/input.txt", skip_empty_rows = F)
dat <- rbind(colnames(dat),dat)
dat <- dat %>% rename(vals = 1)

# part 1

dat_tidy <- dat %>%
  mutate(
    stop = nchar(vals),
    start = (nchar(vals)/2)) %>%
  mutate(
    group_1 = substr(vals, start=1, stop=start),
    group_2 = substr(vals, start=start+1, stop=stop)) %>%
  select(group_1, group_2) %>%
  rownames_to_column() %>%
  pivot_longer(!rowname) %>%
  separate_rows(value, sep="") %>%
  filter(!value == "")

slices <- unique(dat_tidy$rowname)

df_fin <- data.frame()
for(i in 1:length(slices)) {

  group_1 <- dat_tidy %>%
    filter(rowname == slices[i] & name == "group_1") %>%
    pull(value)

  group_2 <- dat_tidy %>%
    filter(rowname == slices[i] & name == "group_2") %>%
    pull(value)

  val <- group_1[which(group_1 %in% group_2)]
  val <- data.frame("letter" = val, "rowname" = slices[i]) %>% distinct()

  df_fin <- rbind(df_fin, val)

}

winner <- data.frame(letter = c(letters, LETTERS), amount = 1:52)

df_fin %>%
  left_join(., winner, by = "letter") %>%
  mutate(amount = sum(amount))

# part 2

dat_tidy <- dat %>%
  rownames_to_column() %>%
  mutate(
    rowname = as.numeric(rowname),
    rowname = rowname - 1,
    group = rowname - rowname %% 3) %>%
  select(rowname, group, vals) %>%
  separate_rows(vals, sep="") %>%
  filter(!vals == "")

dat_tidy <- split(dat_tidy, dat_tidy$group)

df_fin <- c()
for(i in 1:length(dat_tidy)) {

  temp <- dat_tidy[[i]]

  groups <- unique(temp$rowname)

  group_1 <- temp %>%
    filter(rowname == groups[1]) %>%
    pull(vals)

  group_2 <- temp %>%
    filter(rowname == groups[2]) %>%
    pull(vals)

  group_3 <- temp%>%
    filter(rowname == groups[3]) %>%
    pull(vals)

  group_x <- group_1[group_1 %in% group_2]
  group_y <- group_2[group_2 %in% group_3]
  val <- group_x[group_x %in% group_y]
  val <- unique(val)

  val <- data.frame("letter" = val, "rowname" = paste0("group_", i))

  df_fin <- rbind(df_fin, val)

}

df_fin %>%
  left_join(., winner, by = "letter") %>%
  mutate(val = sum(amount))
