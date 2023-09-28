library(dplyr)
library(stringr)

# biscoe_data <- read.csv("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv")
# head(biscoe_data)
#
# # are there any NA values?
# anyNA(biscoe_data)
#
# # remove all na information
# biscoe_data <- biscoe_data |>
#   na.omit()
#
# # summarize just bill length
# biscoe_data_means <- biscoe_data |>
#   group_by(species, sex) |>
#   summarise(mean_bill_length = mean(bill_length_mm))
#
# # summarize by all columns end with "mm", start_with() works too
# biscoe_data_means <- biscoe_data |>
#   group_by(species, sex) |>
#   summarise(across(ends_with("mm"), mean))
#
# # summarize by all columns end with "mm" or "g", or is '|'
# biscoe_data_means <- biscoe_data |>
#   group_by(species, sex) |>
#   summarise(across(ends_with("mm") | ends_with("g"), mean))
#
# # add columns that are imperial for the metric that end in mm
# # everything to left, current value *0.03937008
# # create a new name with what was there but add "_in"
# # ~ formula, . original and function
# biscoe_data_means_imperial <- biscoe_data_means |>
#   mutate(across(ends_with("mm"), ~ . *0.03937008, .names = "{.col}_in")) |>
#   mutate(across(ends_with("g"), ~ . *0.002204623, .names = "{.col}_lb"))
#
#
# # rename column name, rename(new, existing)
# biscoe_data_means_imperial <- biscoe_data_means_imperial |>
#   rename(flipper_length_mm_in_new = flipper_length_mm_in)
#
# # rename all "in" to "mm_in" and all "lb" to "g_lb"
# biscoe_data_means_imperial <- biscoe_data_means_imperial |>
#   rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
#   rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))
#
#
# biscoe_data_means_imperial_3 <- biscoe_data_means_imperial |>
#   select(c(where(is.character) |
#            ends_with("in") |
#            ends_with("lb")))

# create a function
# my_function <- function() {
#  return("I need coffee!")
# }
# # call the function, output to console
# my_function()
#
# # fav_bev parameter with default with "coffee"
# # "paste0()" no spaces between elements,"paste()" will put in spaces
# my_function2 <- function(fav_bev = "coffee") {
#   what_to_say <- paste0("I need ", fav_bev, "!")
#   return(what_to_say)
# }
# # say tea
# my_function2("tea")
# # default to say coffee
# my_function2()
#
#
# my_function3 <- function(data_url = "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv") {
#   island_dat <- read.csv(data_url)
#   island_dat <- island_dat |>
#     na.omit()
#
#   return(island_dat)
# }
#
# # call with default function value
# func_out <- my_function3()
# # call with a new file with other data
# func_out2 <- my_function3("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv")
#

#### functions ####

my_function4 <- function(island_to_use = "Biscone") {
  # full url    "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv"
  if (island_to_use == "Biscone") {
    data_url <-
      "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv"
  } else if (island_to_use == "Dream") {
    data_url <-
      "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Dream.csv"
  } else if (island_to_use == "Torgersen") {
    data_url <-
      "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv"
  } else {
    stop("Island to use doesn't exist.")
  }
  
  
  biscoe_data <- read.csv(data_url)
  # remove all na information
  biscoe_data <- biscoe_data |>
    na.omit()
  
  # summarize by all columns end with "mm" or "g", or is '|'
  biscoe_data_means <- biscoe_data |>
    group_by(species, sex) |>
    summarise(across(ends_with("mm") | ends_with("g"), mean))
  
  biscoe_data_means_imperial <- biscoe_data_means |>
    mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in")) |>
    mutate(across(ends_with("g"), ~ . * 0.002204623, .names = "{.col}_lb"))
  
  # rename all "in" to "mm_in" and all "lb" to "g_lb"
  biscoe_data_means_imperial <- biscoe_data_means_imperial |>
    rename_with( ~ stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
    rename_with( ~ stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))
  
  biscoe_data_means_imperial_3 <- biscoe_data_means_imperial |>
    select(c(where(is.character) |
               ends_with("in") |
               ends_with("lb")))
  
  return(biscoe_data_means_imperial_3)
  
}

#### test calls ####
# three different datasets
b <- my_function4() # Biscone default output
d <- my_function4(island_to_use = "Dream")
b <- my_function4(island_to_use = "Biscone")
t <- my_function4(island_to_use = "Torgersen")
t <- my_function4(island_to_use = "Torgerson") # error
