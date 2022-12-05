library(tidyverse)

dat <- readLines("./Day 4/input.txt") %>% as.data.frame() %>% rename(vals = 1)

# part 1

dat_tidy <- dat %>%
  separate(vals, into=c("group1", "group2"), ",") %>%
  rownames_to_column() %>%
  pivot_longer(!rowname) %>%
  separate(value, into=c("start", "stop"), "-") %>%
  pivot_longer(!c(rowname, name), names_to="range", values_transform = list(value = parse_number))%>%
  select(-range)

groups <- unique(dat_tidy$rowname)

ovelaps <- data.frame()
for(i in 1:length(groups)) {

  temp <- dat_tidy %>% filter(rowname == groups[[i]]) %>%
    select(-rowname) %>%
    group_by(name) %>%
    complete(value = min(value):max(value))

  group_1 <- temp %>% filter(name == "group1") %>% pull(value)
  group_2 <- temp %>% filter(name == "group2") %>% pull(value)

  if(all(between(group_1, min(group_2), max(group_2)))) {
    ovelaps <- rbind(ovelaps, data.frame(overlap = groups[[i]]))
  } else if (all(between(group_2, min(group_1), max(group_1)))) {
    ovelaps <- rbind(ovelaps, data.frame(overlap = groups[[i]]))
  }

}

nrow(ovelaps)

# part 2
# identical to above, but switched "all" to "any" in if cases

ovelaps <- data.frame()
for(i in 1:length(groups)) {

  temp <- dat_tidy %>% filter(rowname == groups[[i]]) %>%
    select(-rowname) %>%
    group_by(name) %>%
    complete(value = min(value):max(value))

  group_1 <- temp %>% filter(name == "group1") %>% pull(value)
  group_2 <- temp %>% filter(name == "group2") %>% pull(value)

  if(any(between(group_1, min(group_2), max(group_2)))) {
    ovelaps <- rbind(ovelaps, data.frame(overlap = groups[[i]]))
  } else if (any(between(group_2, min(group_1), max(group_1)))) {
    ovelaps <- rbind(ovelaps, data.frame(overlap = groups[[i]]))
  }

}

nrow(ovelaps)

