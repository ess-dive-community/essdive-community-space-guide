# Load necessary libraries
library(tidyverse)

# Import the CSV file
terms <-
  read.csv("~/desktop/template_for_term_list_parser.csv",
           header = TRUE)
head(terms)
dim(terms)

## First, create a function that will ultimately put our CSV file into markdown format
# csv_to_md_table <- function(df) {
#   paste0(
#     '|',
#     paste(names(df), collapse = '|'),
#     '|\n|',
#     paste(rep(':----------------------------------------------------', length(df)), collapse = '|'),
#     '|\n|',
#     paste(Reduce(function(x, y) {
#       paste(x, y, sep = '|')
#     }, df), collapse = '|\n|'),
#     '|\n',
#     '\n'
#   )
# }

csv_to_md_table_with_bold <- function(df) {
  paste0(
    '|',
    paste(names(df), collapse = '|'),
    '|\n|',
    paste(rep(':----------------------------------------------------', length(df)), collapse = '|'),
    '|\n|',
    paste(Reduce(function(x,y) { # This is where each of the terms starts
      paste0('**',x,'**','|',y) # This paste function makes sure that first element is bold
    }, df), collapse = '|\n|'),
    '|\n',
    '\n'
  )
}


# Then, here are steps for turning 'long' csv file into 'wide' tables for github
number_of_terms <- nrow(terms)
number_of_terms
number_of_columns <- ncol(terms)
number_of_columns

mylist <- vector(mode = "list", length = number_of_terms)
markdown_of_terms <- vector(mode = "list", length = number_of_terms)
markdown_of_terms

for (i in 1:all_of(number_of_terms)) {
  mylist[[i]] <- terms[i,] %>%
    pivot_longer(
      cols = 1:all_of(number_of_columns),
      # Can try adding in all_of(number_of_columns) to get column numbers
      names_to = "Metadata_Element", # This is what you want your first rows called
      values_to = terms[i, 1]
    ) %>%
    slice(-1,)
#  markdown_of_terms[[i]] <- cat(csv_to_md_table(as.data.frame(mylist[[i]])))
  cat(csv_to_md_table_with_bold(as.data.frame(mylist[[i]])))
}

