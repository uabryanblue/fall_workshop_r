library(dplyr)
library(stringr)

biscoe_data <- read.csv("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv")
head(biscoe_data)

# are there any NA values?
anyNA(biscoe_data)

# remove all na information
biscoe_data <- biscoe_data |>
  na.omit()

# summarize just bill length
biscoe_data_means <- biscoe_data |>
  group_by(species, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm))
            
# summarize by all columns end with "mm", start_with() works too
biscoe_data_means <- biscoe_data |>
  group_by(species, sex) |>
  summarise(across(ends_with("mm"), mean))

# summarize by all columns end with "mm" or "g", or is '|'
biscoe_data_means <- biscoe_data |>
  group_by(species, sex) |>
  summarise(across(ends_with("mm") | ends_with("g"), mean))

# add columns that are imperial for the metric that end in mm
# everything to left, current value *0.03937008
# create a new name with what was there but add "_in"
# ~ formula, . original and function 
biscoe_data_means_imperial <- biscoe_data_means |>
  mutate(across(ends_with("mm"), ~ . *0.03937008, .names = "{.col}_in")) |>
  mutate(across(ends_with("g"), ~ . *0.002204623, .names = "{.col}_lb"))
  

# rename column name, rename(new, existing)
biscoe_data_means_imperial <- biscoe_data_means_imperial |>
  rename(flipper_length_mm_in_new = flipper_length_mm_in)

# rename all "in" to "mm_in" and all "lb" to "g_lb"
biscoe_data_means_imperial <- biscoe_data_means_imperial |>
  rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
  rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))


biscoe_data_means_imperial_3 <- biscoe_data_means_imperial |>
  select(c(where(is.character) |
           ends_with("in") |
           ends_with("lb")))

#########
# create a function
my_function <- function() {
 return("I need coffee!") 
}
# call the function, output to console
my_function()

# fav_bev parameter with default with "coffee"
# "paste0()" no spaces between elements,"paste()" will put in spaces
my_function2 <- function(fav_bev = "coffee") {
  what_to_say <- paste0("I need ", fav_bev, "!")
  return(what_to_say) 
}
# say tea
my_function2("tea")
# default to say coffee
my_function2()



