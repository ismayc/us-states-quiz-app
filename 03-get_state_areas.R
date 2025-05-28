libary(tidyverse)

View(state.area)

# https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area

# Just use what is built into R

state_areas <- tibble(
  state_name = state.name,
  area_sq_mi = state.area
) |> 
  add_row(state_name = "District of Columbia", area_sq_mi = 61)

write_rds(state_areas, "state_areas.rds")
