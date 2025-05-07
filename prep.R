library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)

options(tigris_use_cache = TRUE)

# Load US state shapes (excluding territories)
states <- states(cb = TRUE, resolution = "20m") |>
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) |>
  st_transform(4326) |>
  mutate(state_name = NAME)

# Clean up Alaska (mainland only)
alaska_polygons <- states |> filter(state_name == "Alaska") |> st_cast("POLYGON")
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
alaska_meta <- states |>
  filter(state_name == "Alaska") |>
  st_drop_geometry() |>
  select(-state_name)
alaska_clean <- bind_cols(alaska_clean, alaska_meta)
states <- states |>
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
states <- left_join(states, capitals, by = "state_name") |> 
  mutate(
    capital = if_else(state_name == "District of Columbia",
                      "Washington",
                      capital)
  ) |> 
  left_join(y = read_rds("largest_cities_by_state2023.rds"), by = "state_name")
