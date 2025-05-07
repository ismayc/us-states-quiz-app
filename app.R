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
states <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(4326) %>%
  mutate(state_name = NAME)

# Clean up Alaska (mainland only)
alaska_polygons <- states %>% filter(state_name == "Alaska") %>% st_cast("POLYGON")
alaska_mainland <- alaska_polygons %>%
  mutate(bbox = map(geometry, st_bbox)) %>%
  mutate(
    xmin = map_dbl(bbox, ~ .x["xmin"]),
    xmax = map_dbl(bbox, ~ .x["xmax"]),
    ymin = map_dbl(bbox, ~ .x["ymin"]),
    ymax = map_dbl(bbox, ~ .x["ymax"])
  ) %>%
  filter(xmax > -168, ymax > 54)
alaska_clean <- alaska_mainland %>%
  summarize(geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  mutate(state_name = "Alaska")
alaska_meta <- states %>%
  filter(state_name == "Alaska") %>%
  st_drop_geometry() %>%
  select(-state_name)
alaska_clean <- bind_cols(alaska_clean, alaska_meta)
states <- states %>%
  filter(state_name != "Alaska") %>%
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
states <- left_join(states, capitals, by = "state_name")

# UI
ui <- fluidPage(
  titlePanel("US State/District Shape Quiz"),
  h4("Created by Chester Ismay"),
  sidebarLayout(
    sidebarPanel(
      plotOutput("state_shape", height = "200px"),
      textInput("state_guess", "Guess the state/district name:"),
      checkboxInput("guess_capital", "Also guess the capital?", value = FALSE),
      conditionalPanel(
        condition = "input.guess_capital",
        textInput("capital_guess", "Guess the capital city:")
      ),
      actionButton("submit", "Submit Guess"),
      actionButton("giveup", "Give Up on This State/District"),
      actionButton("restart", "Restart Quiz"),
      verbatimTextOutput("feedback")
    ),
    mainPanel(
      leafletOutput("us_map", height = "600px"),
      verbatimTextOutput("score")
    )
  )
)

# Server
server <- function(input, output, session) {
  remaining_states <- reactiveVal(sample(states$state_name))
  guessed_states   <- reactiveVal(character())
  score            <- reactiveVal(0)
  current_state    <- reactiveVal(NULL)
  
  # Initialize first state
  observe({
    rs <- remaining_states()
    if (is.null(current_state()) && length(rs) > 0) {
      current_state(rs[1])
    }
  })
  
  # Draw the state shape
  output$state_shape <- renderPlot({
    req(current_state())
    shape <- states %>% filter(state_name == current_state())
    ggplot(shape) +
      geom_sf(fill = "black") +
      theme_void()
  })
  
  # Draw the US map
  output$us_map <- renderLeaflet({
    leaflet(states) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addPolygons(
        layerId = ~state_name,
        data = states,
        fillColor = "#000000",
        fillOpacity = 0.95,
        color = "white",
        weight = 1
      )
  })
  
  # Helpers for normalization
  normalize_state <- function(x) {
    x <- str_to_lower(x) %>%
      str_replace_all("\\.", "") %>%
      str_trim()
    if (x == "dc") "district of columbia" else x
  }
  normalize_capital <- function(x) {
    x <- str_to_lower(x) %>%
      str_replace_all("\\.", "") %>%
      str_replace_all("^st ", "saint ") %>%
      str_trim()
    x
  }
  
  # Submit handler
  observeEvent(input$submit, {
    req(input$state_guess, current_state())
    
    answered <- current_state()
    guess     <- normalize_state(input$state_guess)
    correct   <- normalize_state(answered)
    is_state_correct <- guess == correct
    
    # Capital check
    is_capital_correct <- TRUE
    if (is_state_correct && input$guess_capital) {
      true_cap <- states %>%
        filter(state_name == answered) %>%
        pull(capital)
      is_capital_correct <- normalize_capital(input$capital_guess) ==
        normalize_capital(true_cap)
    }
    
    if (is_state_correct && is_capital_correct) {
      # Update score & lists
      guessed_states(c(guessed_states(), answered))
      score(score() + 1)
      remaining_states(setdiff(remaining_states(), answered))
      
      # Draw guessed polygons
      leafletProxy("us_map") %>%
        clearGroup("guessed") %>%
        addPolygons(
          data = states %>% filter(state_name %in% guessed_states()),
          fillColor = "steelblue", fillOpacity = 0.5,
          color = "white", weight = 1,
          label = ~state_name, group = "guessed"
        )
      
      # Draw the capital marker for the answered state
      coords <- states %>%
        filter(state_name == answered) %>%
        st_centroid() %>%
        st_coordinates()
      capname <- states %>%
        filter(state_name == answered) %>%
        pull(capital)
      
      leafletProxy("us_map") %>%
        clearGroup("capitals") %>%
        addCircleMarkers(
          lng = coords[1], lat = coords[2],
          radius = 4, color = "red", fillOpacity = 1, stroke = FALSE,
          label = paste0(capname, ", ", answered),
          group = "capitals"
        )
      
      # Advance to next state
      next_states <- remaining_states()
      current_state(if (length(next_states) > 0) next_states[1] else NULL)
      
      output$feedback <- renderText(
        paste0("✅ Correct!", if (input$guess_capital) " Capital correct too!" else "")
      )
      updateTextInput(session, "state_guess",  value = "")
      updateTextInput(session, "capital_guess", value = "")
      
    } else if (!is_state_correct) {
      output$feedback <- renderText("❌ Incorrect state guess.")
    } else {
      output$feedback <- renderText("✅ State correct, ❌ but capital incorrect.")
    }
  })
  
  # Give Up handler
  observeEvent(input$giveup, {
    req(current_state())
    given_up <- current_state()
    
    guessed_states(c(guessed_states(), given_up))
    remaining_states(setdiff(remaining_states(), given_up))
    
    # Show polygon for the given-up state
    leafletProxy("us_map") %>%
      clearGroup("guessed") %>%
      addPolygons(
        data = states %>% filter(state_name %in% guessed_states()),
        fillColor = "steelblue", fillOpacity = 0.5,
        color = "white", weight = 1,
        label = ~state_name, group = "guessed"
      )
    
    # Show its capital
    coords <- states %>%
      filter(state_name == given_up) %>%
      st_centroid() %>%
      st_coordinates()
    capname <- states %>%
      filter(state_name == given_up) %>%
      pull(capital)
    leafletProxy("us_map") %>%
      clearGroup("capitals") %>%
      addCircleMarkers(
        lng = coords[1], lat = coords[2],
        radius = 4, color = "red", fillOpacity = 1, stroke = FALSE,
        label = paste0(capname, ", ", given_up),
        group = "capitals"
      )
    
    output$feedback <- renderText(
      paste0("🙈 The correct answer was: ", given_up, " (", capname, ")")
    )
    
    # Advance
    next_states <- remaining_states()
    current_state(if (length(next_states) > 0) next_states[1] else NULL)
    
    updateTextInput(session, "state_guess",  value = "")
    updateTextInput(session, "capital_guess", value = "")
  })
  
  # Restart handler
  observeEvent(input$restart, {
    remaining_states(sample(states$state_name))
    guessed_states(character())
    score(0)
    current_state(NULL)
    
    leafletProxy("us_map") %>%
      clearGroup("guessed") %>%
      clearGroup("capitals")
    
    output$feedback <- renderText("🌀 Quiz restarted! Good luck!")
    updateTextInput(session, "state_guess",  value = "")
    updateTextInput(session, "capital_guess", value = "")
  })
  
  # Score display
  output$score <- renderText({
    paste("Score:", score(), "/", nrow(states))
  })
}

shinyApp(ui, server)
