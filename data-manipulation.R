library('dplyr')
library('tidyr')
library('palmerpenguins')

penguins_data <- penguins

class(penguins_data)
class(penguins)
head(penguins)
str(penguins)
unique(penguins_data$species)
unique(penguins_data$island)
unique(penguins_data$sex)
levels(penguins_data$island)

paste("Year: ", penguins_data$year)

#.....
str(penguins_data)
# columns
island_year <- select(penguins_data, island, year)
# rows, looking at a values
torgersen_penguins <- filter(penguins_data, island == "Torgersen")

# Torgersen, species and sex columns, look at column labels
torgersen_penguins_sex_species <- select(torgersen_penguins, sex, species)

# code chunk run together with pipe to next command "|>" tidy "%>%"
torgersen_penguins_sample <- filter(penguins_data, island == "Torgersen") |>
  select(sex, species)

# create/change a column in dataframe
torgersen_penguins <- torgersen_penguins |>
  mutate(rounded_bill_length = round(bill_length_mm)) |>
  select(species, sex, rounded_bill_length)

# pivot tables, avg bill length based on species, and sex
torgersen_penguins_summary <- penguins_data |>
  group_by(species, sex) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))


