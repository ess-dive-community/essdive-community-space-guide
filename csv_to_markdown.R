# Load necessary libraries
library(tidyverse)

# Import the CSV file
standard_terms <-
  read.csv("~/desktop/standards/CSV_to_markdown_parser/example_data_for_parser.csv",
           header = TRUE)
head(standard_terms)
dim(standard_terms)

## First, create a function that will ultimately put our CSV file into markdown format
csv_to_md_table <- function(df) {
  paste0(
    '|',
    paste(names(df), collapse = '|'),
    '|\n|',
    paste(rep(':---', length(df)), collapse = '|'),
    '|\n|',
    paste(Reduce(function(x, y) {
      paste(x, y, sep = '|')
    }, df), collapse = '|\n|'),
    '|\n',
    '\n'
  )
}

# Then, here are steps for turning 'long' csv file into 'wide' tables for github
number_of_terms <- nrow(standard_terms)
number_of_terms
number_of_columns <- ncol(standard_terms)
mylist <- vector(mode = "list", length = number_of_terms)
markdown_of_terms <- vector(mode = "list", length = number_of_terms)

for (i in 1:all_of(number_of_terms)) {
  mylist[[i]] <- standard_terms[i,] %>%
    pivot_longer(
      cols = 1:all_of(number_of_columns),
      # Can try adding in all_of(number_of_columns) to get column numbers
      names_to = "Field name",
      values_to = standard_terms[i, 1]
    ) %>%
    slice(-1,)
  #  markdown_of_terms[[i]] <- cat(csv_to_md_table(as.data.frame(mylist[[i]])))
  cat(csv_to_md_table(as.data.frame(mylist[[i]])))
}
