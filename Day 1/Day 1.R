library(tidyverse)

dat <- read_csv("./Day 1/input.txt", skip_empty_rows = F)
dat <- rbind(colnames(dat),dat)
dat <- dat %>% rename(vals = 1)

# part 1

dat %>%
  rownames_to_column() %>%
  mutate(group = ifelse(is.na(vals), paste0("group_", rowname), NA),
         group = zoo::na.locf(group, na.rm=F),
         group = ifelse(is.na(group), "group_x", group)) %>%
  drop_na() %>%
  group_by(group) %>%
  summarize(vals = sum(as.numeric(vals))) %>%
  ungroup() %>%
  arrange(-vals)


# part 2

dat %>%
  rownames_to_column() %>%
  mutate(group = ifelse(is.na(vals), paste0("group_", rowname), NA),
         group = zoo::na.locf(group, na.rm=F),
         group = ifelse(is.na(group), "group_x", group)) %>%
  drop_na() %>%
  group_by(group) %>%
  summarize(vals = sum(as.numeric(vals))) %>%
  ungroup() %>%
  arrange(-vals) %>%
  slice_head(n=3) %>%
  summarize(vals = sum(vals))
