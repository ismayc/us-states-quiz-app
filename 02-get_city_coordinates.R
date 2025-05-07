library(tidygeocoder)
library(tidyverse)

states_raw3 <- read_rds("states2023.rds")

# Assuming 'states' is your data frame with city and state information
cities_df <- states_raw3 %>%
  select(state_name, capital, largest_city, second_largest_city) %>%
  select(-geometry) |> 
  pivot_longer(cols = c(capital, largest_city, second_largest_city), names_to = "city_type", values_to = "city") %>%
  distinct(state_name, city, city_type)

# Geocode cities
geocoded_cities <- cities_df %>%
  geocode(city = city, state = state_name, method = "osm", lat = latitude, long = longitude)

# Pivot geocoded data to wide format
geocoded_wide <- geocoded_cities %>%
  pivot_wider(names_from = city_type, values_from = c(city, latitude, longitude))

# Combine with the original states data
states_enriched <- states_raw3 %>%
  left_join(geocoded_wide, by = "state_name")

# Save the enriched data
write_rds(states_enriched, "states_enriched2023.rds")
