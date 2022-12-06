library(tidyverse)

dat <- readLines("./Day 6/input.txt")

# part 1

dat_tidy <- dat

for(i in 1:nchar(dat_tidy)) {

  section <- substr(dat_tidy, i, i + 3)
  section_split <- str_split(section, "", n=4)[[1]]

  if(!any(duplicated(section_split))) {

    df <- data.frame("vals" = section_split, "rowname" = i:(i+3))

    start_char <- strsplit(dat_tidy, "") %>%
      unlist() %>%
      as.data.frame() %>%
      rename(vals = 1) %>%
      rownames_to_column() %>%
      merge(., df, by = c("rowname", "vals")) %>%
      filter(rowname == max(as.numeric(rowname))) %>%
      pull(rowname)

    return(start_char)

    break

  }

}

print(start_char)

# part 2

dat_tidy <- dat

for(i in 1:nchar(dat_tidy)) {

  section <- substr(dat_tidy, i, i + 13)
  section_split <- str_split(section, "", n=14)[[1]]

  if(!any(duplicated(section_split))) {

    df <- data.frame("vals" = section_split, "rowname" = i:(i+13))

    start_char <- strsplit(dat_tidy, "") %>%
      unlist() %>%
      as.data.frame() %>%
      rename(vals = 1) %>%
      rownames_to_column() %>%
      merge(., df, by = c("rowname", "vals")) %>%
      filter(rowname == max(as.numeric(rowname))) %>%
      pull(rowname)

    return(start_char)

    break

  }

}

print(start_char)

