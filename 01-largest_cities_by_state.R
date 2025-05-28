library(rvest)
library(dplyr)
library(stringr)
library(readr)

url <- "https://en.wikipedia.org/wiki/List_of_largest_cities_of_U.S._states_and_territories_by_population"

# Read the HTML content
page <- read_html(url)

# Extract all tables
tables <- page %>% html_nodes("table")

# Identify the correct table (assuming it's the first one)
city_table <- tables[[2]] %>% html_table(fill = TRUE)

# Clean and select relevant columns
city_data <- city_table |>
  select(
    state = 1,
    state_population = 2,
    largest_city = 3,
    largest_city_population2023 = 4,
    second_largest_city = 5,
    second_largest_city_population2023 = 6
  ) |>
  slice(-1) |>
  filter(
    !(state %in%
      c(
        "American Samoa",
        "Guam",
        "Northern Mariana Islands",
        "Puerto Rico",
        "Virgin Islands (U.S.)"
      ))
  ) |>
  mutate(largest_city = str_replace_all(largest_city, ", D.C.", "")) |>
  # remove wikipedia numbers from largest_city and second_largest_city columns
  mutate(
    largest_city = str_replace_all(largest_city, "\\[\\d+\\]", ""),
    second_largest_city = str_replace_all(
      second_largest_city,
      "\\[\\d+\\]",
      ""
    ),
    largest_city = str_replace_all(
      largest_city,
      "1",
      ""
    ),
    second_largest_city = str_replace_all(
      second_largest_city,
      " 1",
      ""
    ),
    state_population = str_replace_all(state_population, ",", "") |>
      as.numeric(),
    largest_city_population2023 = str_replace_all(
      largest_city_population2023,
      ",",
      ""
    ) |>
      as.numeric(),
    second_largest_city_population2023 = str_replace_all(
      second_largest_city_population2023,
      ",",
      ""
    ) |>
      as.numeric()
  ) |> 
  rename(state_name = state)

write_rds(city_data, "largest_cities_by_state2023.rds")

