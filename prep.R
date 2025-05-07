options(tigris_use_cache = TRUE)

# Load US state shapes (excluding territories)
states_raw <- states(cb = TRUE, resolution = "20m") |>
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) |>
  st_transform(4326) |>
  mutate(state_name = NAME)

# Clean up Alaska (mainland only)
alaska_polygons <- states_raw |> filter(state_name == "Alaska") |> st_cast("POLYGON")
alaska_mainland <- alaska_polygons |>
  mutate(bbox = map(geometry, st_bbox)) |>
  mutate(
    xmin = map_dbl(bbox, ~ .x["xmin"]),
    xmax = map_dbl(bbox, ~ .x["xmax"]),
    ymin = map_dbl(bbox, ~ .x["ymin"]),
    ymax = map_dbl(bbox, ~ .x["ymax"])
  ) |>
  filter(xmax > -168, ymax > 54)
alaska_clean <- alaska_mainland |>
  summarize(geometry = st_union(geometry)) |>
  st_as_sf() |>
  mutate(state_name = "Alaska")
alaska_meta <- states_raw |>
  filter(state_name == "Alaska") |>
  st_drop_geometry() |>
  select(-state_name)
alaska_clean <- bind_cols(alaska_clean, alaska_meta)
states_raw2 <- states_raw |>
  filter(state_name != "Alaska") |>
  bind_rows(alaska_clean)

# Add state capitals
capitals <- tibble::tibble(
  state_name = state.name,
  capital = c("Montgomery", "Juneau", "Phoenix", "Little Rock", "Sacramento",
              "Denver", "Hartford", "Dover", "Tallahassee", "Atlanta", "Honolulu",
              "Boise", "Springfield", "Indianapolis", "Des Moines", "Topeka",
              "Frankfort", "Baton Rouge", "Augusta", "Annapolis", "Boston",
              "Lansing", "Saint Paul", "Jackson", "Jefferson City", "Helena",
              "Lincoln", "Carson City", "Concord", "Trenton", "Santa Fe",
              "Albany", "Raleigh", "Bismarck", "Columbus", "Oklahoma City",
              "Salem", "Harrisburg", "Providence", "Columbia", "Pierre",
              "Nashville", "Austin", "Salt Lake City", "Montpelier", "Richmond",
              "Olympia", "Charleston", "Madison", "Cheyenne")
)

# Need to run 01-largest_cities_by_state.R to get the largest cities
# source("01-largest_cities_by_state.R")
states_raw3 <- left_join(states_raw2, capitals, by = "state_name") |> 
  mutate(
    capital = if_else(state_name == "District of Columbia",
                      "Washington",
                      capital)
  ) |> 
  left_join(y = read_rds("largest_cities_by_state2023.rds"), by = "state_name")

# write_rds(states_raw3, "states2023.rds")

# Need to run 02-get_city_coordinates.R to get the coordinates
# source("02-get_city_coordinates.R")

