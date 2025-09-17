# source("prep.R")

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(stringdist)
library(ggstar)

states <- read_rds("states_enriched2023.rds")
state_areas <- read_rds("state_areas.rds") |> 
  arrange(desc(area_sq_mi)) |> 
  rownames_to_column("area_rank")

dist_tolerance <- 2

library(googlesheets4)

googlesheets4::gs4_auth(path = "shiny-sheets-writer-key.json")


# UI
ui <- fluidPage(
  tags$head(
    # your existing CSS
    tags$style(HTML("
      /* 1) Remove the form‐group’s bottom margin entirely */
      .form-group.shiny-input-container {
        margin-bottom: 0 !important;
      }
      /* 2) Collapse the checkbox wrapper (<div class=\"checkbox\">) */
      .form-group.shiny-input-container .checkbox,
      .form-group.shiny-input-container .checkbox-inline {
        margin: 0 !important;
        padding: 0 !important;
      }
      /* 3) Shrink the label inside the checkbox */
      .form-group.shiny-input-container .checkbox label,
      .form-group.shiny-input-container .checkbox-inline label {
        margin: 0 !important;
        padding: 0 !important;
        line-height: 1 !important;
      }
    ")),
    
    # new JS for Enter‐to‐Submit
    tags$script(HTML("
      // When Enter is pressed in any guess‐field, trigger the Submit button
      $(document).on('keydown', '#state_guess, #capital_guess, #largest_guess, #second_guess', function(e) {
        if (e.keyCode === 13) {
          e.preventDefault();
          $('#submit').click();
        }
      });
    "))
  ),

  titlePanel("US State/District Shape Quiz"),
  h4("Created by Chester Ismay"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("question_number"),
      uiOutput("progress_bar"),
      checkboxInput(
        "provide_area",
        "Show the two states/district closest in area"
      ),
      conditionalPanel(
        condition = "input.provide_area",
        #       h5("States closest in area"),
        tableOutput("area_hint")
      ),
      plotOutput("state_shape", height = "200px"),
      textInput("state_guess", "Guess the state/district name:"),
      # tags$hr(
      #   style = "border-top: 2px solid; margin-top:0px; margin-bottom:0px;"
      # ),
      checkboxInput("guess_capital", "Also guess the capital?", value = FALSE),
      conditionalPanel(
        condition = "input.guess_capital",
        textInput("capital_guess", "Guess the capital city:")
      ),
      checkboxInput(
        "guess_largest",
        "Also guess the largest city?",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.guess_largest",
        textInput("largest_guess", "Guess the largest city:")
      ),
      checkboxInput(
        "guess_second",
        "Also guess the second largest city?",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.guess_second",
        textInput("second_guess", "Guess the second largest city:")
      ),
      actionButton("submit", "Submit Guess"),
      actionButton("giveup", "Give Up on This Question"),
      actionButton("restart", "Restart Quiz"),
      verbatimTextOutput("feedback"),
      checkboxInput(
        "save",
        "Log overall quiz results to Google Sheets?",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.save",
        textInput("user_name", "Enter your name:"),
        actionButton("save_results", "Save Results to Google Sheets")
      )
    ),
    mainPanel(
      helpText(
        paste("You can make unlimited guesses for each question by",
              "clicking on 'Submit Guess'.")
      ),
      helpText(
        paste("If you're stuck on a question, click",
              "'Give Up on This Question' to see the answer.")
      ),
      leafletOutput("us_map", height = "600px"),
      verbatimTextOutput("score")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # all_states <- states$state_name
  # test_front <- c("Wyoming", "Colorado")
  # other_states <- setdiff(all_states, test_front)
  # 
  # # a helper that always prefixes WY & CO, then shuffles the rest:
  # make_order <- function() {
  #   c(test_front, sample(other_states))
  # }
  # 
  # # initialize reactiveVal with our special order:
  # remaining_states <- reactiveVal(make_order())
  
  remaining_states <- reactiveVal(sample(states$state_name))
  guessed_states <- reactiveVal(character())
  score <- reactiveVal(0)
  total_attempts <- reactiveVal(0)
  current_state <- reactiveVal(NULL)
  wrong_states <- reactiveVal(character())
  wrong_capitals <- reactiveVal(character())
  wrong_largests <- reactiveVal(character())
  wrong_second_largests <- reactiveVal(character())
  
  # Reactive that finds the two closest‐by‐area states,
  # and *renames* the columns up front:
  area_hint_data <- reactive({
    req(input$provide_area, current_state())
    
    # current state’s area
    cur_area <- state_areas |>
      filter(state_name == current_state()) |>
      pull(area_sq_mi)
    
    # find two with smallest area difference and rename columns
    state_areas |>
      filter(state_name != current_state()) |>
      mutate(area_diff = abs(area_sq_mi - cur_area)) |>
      arrange(area_diff) |>
      slice_head(n = 2) |>
      arrange(area_rank) |> 
      select(
        `Area Rank` = area_rank,
        `State / District`     = state_name,
        `Square Miles` = area_sq_mi,
      )
  })
  
  output$area_hint <- renderTable({
    area_hint_data()
  }, striped = TRUE, hover = TRUE, rownames = FALSE,
  digits      = 0,                       # no decimal places
  format.args = list(big.mark = ",")     # insert commas
  )
  
  
  
  output$question_number <- renderText({
    total_states <- nrow(states)
    attempted <- total_states - length(remaining_states())
    paste0("Question ", min(attempted + 1, total_states), " of ", total_states)
  })
  
  output$progress_bar <- renderUI({
    total <- nrow(states)
    attempted <- total - length(remaining_states())
    percent <- round((attempted / total) * 100)
    
    div(
      style = "margin-top: 5px; margin-bottom: 10px;",
      div("Progress:", style = "font-weight: bold;"),
      tags$div(
        style = "background-color: #e0e0e0; border-radius: 5px; height: 20px;",
        tags$div(
          style = paste0(
            "background-color: #428bca; width: ",
            percent,
            "%; ",
            "height: 100%; border-radius: 5px;"
          )
        )
      ),
      div(
        paste0(percent, "% completed"),
        style = "font-size: 12px; margin-top: 4px;"
      )
    )
  })
  
  # Initialize first state
  observe({
    rs <- remaining_states()
    if (is.null(current_state()) && length(rs) > 0) {
      current_state(rs[1])
    }
  })
  
  # Draw the state shape (with capital star for WY and CO)
  output$state_shape <- renderPlot({
    req(current_state())
    shape <- states |> filter(state_name == current_state())
    
    ggplot(shape) +
      geom_sf(fill = "black") +
      theme_void() -> p
    
    if (current_state() %in% c("Wyoming", "Colorado")) {
      cap_df <- shape |>
        select(capital, longitude_capital, latitude_capital) |>
        rename(lon = longitude_capital, lat = latitude_capital)
      
      p <- p +
        geom_star(
          data      = cap_df,
          aes(x = lon, y = lat),
          # https://cran.r-project.org/web/packages/ggstar/vignettes/ggstar.html
          starshape = 1,     # 5-pointed star (pentagram)
          fill      = "white",
          colour    = NA,
          size      = 4      # numeric size in mm
        )
    }
    
    p
  })
  
  
  
  # Draw the US map
  output$us_map <- renderLeaflet({
    leaflet(states) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 37.8, zoom = 4) |>
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
    x <- str_to_lower(x) |>
      str_replace_all("\\.", "") |>
      str_trim()
    if (x == "dc") "district of columbia" else x
  }
  normalize_capital <- function(x) {
    x <- str_to_lower(x) |>
      str_replace_all("\\.", "") |>
      str_replace_all("^st ", "saint ") |>
      str_trim()
    x
  }
  normalize_city <- function(x) {
    str_to_lower(x) |> str_replace_all("\\.", "") |> str_trim()
  }
  
  add_city_markers <- function(state_name) {
    state_data <- states |> filter(state_name == !!state_name)
    
    cap <- state_data$capital
    cap_lat <- state_data$latitude_capital
    cap_lon <- state_data$longitude_capital
    
    largest <- state_data$largest_city
    largest_lat <- state_data$latitude_largest_city
    largest_lon <- state_data$longitude_largest_city
    largest_pop <- state_data$largest_city_population2023
    
    second <- state_data$second_largest_city
    second_lat <- state_data$latitude_second_largest_city
    second_lon <- state_data$longitude_second_largest_city
    second_pop <- state_data$second_largest_city_population2023
    
    # Determine if capital is also a largest/second-largest city
    cap_role <- ""
    if (cap == largest) {
      cap_role <- paste0(
        " (Capital & Largest, Pop: ",
        format(largest_pop, big.mark = ","),
        ")"
      )
    } else if (cap == second) {
      cap_role <- paste0(
        " (Capital & Second Largest, Pop: ",
        format(second_pop, big.mark = ","),
        ")"
      )
    }
    
    proxy <- leafletProxy("us_map") |>
      clearGroup("capitals") |>
      clearGroup("largest") |>
      clearGroup("second_largest")
    
    if (!is.na(cap_lat) && !is.na(cap_lon)) {
      proxy <- proxy |>
        addAwesomeMarkers(
          lng = cap_lon,
          lat = cap_lat,
          icon = awesomeIcons(
            icon = 'star',
            markerColor = 'red',
            iconColor = 'white',
            library = 'fa'
          ),
          label = paste0("Capital: ", cap, ", ", state_name, cap_role),
          group = "capitals"
        )
    }
    
    if (!is.na(largest_lat) && !is.na(largest_lon) && cap != largest) {
      proxy <- proxy |>
        addCircleMarkers(
          lng = largest_lon,
          lat = largest_lat,
          radius = 5,
          color = "pink",
          fillColor = "pink",
          fillOpacity = 1,
          stroke = FALSE,
          label = paste0(
            "Largest: ",
            largest,
            " (",
            format(largest_pop, big.mark = ","),
            ")"
          ),
          group = "largest"
        )
    }
    
    if (!is.na(second_lat) && !is.na(second_lon) && cap != second) {
      proxy <- proxy |>
        addCircleMarkers(
          lng = second_lon,
          lat = second_lat,
          radius = 5,
          color = "green",
          fillColor = "green",
          fillOpacity = 1,
          stroke = FALSE,
          label = paste0(
            "Second: ",
            second,
            " (",
            format(second_pop, big.mark = ","),
            ")"
          ),
          group = "second_largest"
        )
    }
  }
  
  # Submit handler
  observeEvent(input$submit, {
    req(input$state_guess, current_state())
    total_attempts(total_attempts() + 1)
    
    answered       <- current_state()
    feedback_parts <- character()
    
    # --- STATE: strict + fuzzy check ---
    guess_state      <- normalize_state(input$state_guess)
    true_state       <- normalize_state(answered)
    dist_state       <- stringdist(guess_state, true_state, method = "lv")
    is_strict_state  <- guess_state == true_state
    
    # --- Always grab truth data for this state once ---
    state_info   <- states |> dplyr::filter(state_name == answered)
    true_cap     <- state_info$capital
    true_largest <- state_info$largest_city
    true_second  <- state_info$second_largest_city
    
    if (is_strict_state) {
      # exact correct!
      feedback_parts <- c(feedback_parts, paste0("✅ ", answered, " is the correct state!"))
    } else if (dist_state <= dist_tolerance) {
      # close but wrong
      feedback_parts <- c(
        feedback_parts,
        paste0("❗ Misspelling: You are off by ", dist_state,
               " character", ifelse(dist_state > 1, "s", ""))
      )
      wrong_states(c(wrong_states(), answered))
    } else {
      # plain wrong
      feedback_parts <- c(feedback_parts, "❌ State incorrect")
      wrong_states(c(wrong_states(), answered))
    }
    
    # --- CAPITAL: only if state was strict correct ---
    is_strict_cap <- TRUE
    if (is_strict_state && input$guess_capital) {
      true_cap       <- states |> filter(state_name == answered) |> pull(capital)
      guess_cap      <- normalize_capital(input$capital_guess)
      dist_cap       <- stringdist(guess_cap, normalize_capital(true_cap), method = "lv")
      is_strict_cap  <- guess_cap == normalize_capital(true_cap)
      
      if (is_strict_cap) {
        feedback_parts <- c(feedback_parts, paste0("✅ ", true_cap, " is the correct capital!"))
      } else if (dist_cap <= dist_tolerance) {
        feedback_parts <- c(
          feedback_parts,
          paste0("❗ Misspelling (capital): off by ", dist_cap, 
                 " character", ifelse(dist_cap>1,"s",""))
        )
        wrong_capitals(c(wrong_capitals(), answered))
      } else {
        feedback_parts <- c(feedback_parts, "❌ Capital incorrect")
        wrong_capitals(c(wrong_capitals(), answered))
      }
    }
    
    # --- LARGEST CITY: only if state was strict correct ---
    is_strict_largest <- TRUE
    if (is_strict_state && input$guess_largest) {
      true_largest       <- states |> filter(state_name == answered) |> pull(largest_city)
      guess_largest      <- normalize_city(input$largest_guess)
      dist_largest       <- stringdist(guess_largest, normalize_city(true_largest), method = "lv")
      is_strict_largest  <- guess_largest == normalize_city(true_largest)
      
      if (is_strict_largest) {
        feedback_parts <- c(feedback_parts, paste0("✅ ", true_largest, " is the correct largest city!"))
      } else if (dist_largest <= dist_tolerance) {
        feedback_parts <- c(
          feedback_parts,
          paste0("❗ Misspelling (largest): off by ", dist_largest, 
                 " character", ifelse(dist_largest>1,"s",""))
        )
        wrong_largests(c(wrong_largests(), answered))
      } else {
        feedback_parts <- c(feedback_parts, "❌ Largest city incorrect")
        wrong_largests(c(wrong_largests(), answered))
      }
    }
    
    # --- SECOND LARGEST CITY: only if state was strict correct ---
    is_strict_second <- TRUE
    if (is_strict_state && input$guess_second) {
      true_second       <- states |> filter(state_name == answered) |> pull(second_largest_city)
      guess_second      <- normalize_city(input$second_guess)
      dist_second       <- stringdist(guess_second, normalize_city(true_second), method = "lv")
      is_strict_second  <- guess_second == normalize_city(true_second)
      
      if (is_strict_second) {
        feedback_parts <- c(feedback_parts, paste0("✅ ", true_second, " is the correct second largest city!"))
      } else if (dist_second <= dist_tolerance) {
        feedback_parts <- c(
          feedback_parts,
          paste0("❗ Misspelling (second): off by ", dist_second, 
                 " character", ifelse(dist_second>1,"s",""))
        )
        wrong_second_largests(c(wrong_second_largests(), answered))
      } else {
        feedback_parts <- c(feedback_parts, "❌ Second largest city incorrect")
        wrong_second_largests(c(wrong_second_largests(), answered))
      }
    }
    
    # Build FYI only for items NOT selected to be guessed
    fyi_lines <- character()
    if (!isTRUE(input$guess_capital)) {
      fyi_lines <- c(fyi_lines, paste0("🏛️ Capital: ", true_cap))
    }
    if (!isTRUE(input$guess_largest)) {
      fyi_lines <- c(fyi_lines, paste0("🌆 Largest city: ", true_largest))
    }
    if (!isTRUE(input$guess_second)) {
      fyi_lines <- c(fyi_lines, paste0("🏙️ Second largest city: ", true_second))
    }
    
    # Append FYI block only when the state is strictly correct and there’s something to show
    if (is_strict_state && length(fyi_lines) > 0) {
      feedback_parts <- c(
        feedback_parts,
        "",
        "🤔 In case you were wondering...",
        fyi_lines
      )
    }
    
    # render feedback text
    output$feedback <- renderText(paste(feedback_parts, collapse = "\n"))
    
    # advance only when **all** strict checks passed
    if (is_strict_state && is_strict_cap && is_strict_largest && is_strict_second) {
      guessed_states(c(guessed_states(), answered))
      score(score() + 1)
      remaining_states(setdiff(remaining_states(), answered))
      
      # rebuild labels and update map
      guessed_df <- states |> filter(state_name %in% guessed_states())
      labels <- paste0(
        guessed_df$state_name,
        "<br>Population (2023): ",
        format(guessed_df$state_population, big.mark = ",")
      ) |> lapply(htmltools::HTML)
      
      leafletProxy("us_map") |>
        clearGroup("guessed") |>
        addPolygons(
          data        = guessed_df,
          fillColor   = "steelblue",
          fillOpacity = 0.5,
          color       = "white",
          weight      = 1,
          group       = "guessed",
          label       = labels
        )
      add_city_markers(answered)
      
      # next state
      next_states <- remaining_states()
      current_state(if (length(next_states) > 0) next_states[1] else NULL)
      
      # clear inputs
      updateTextInput(session, "state_guess",   value = "")
      updateTextInput(session, "capital_guess", value = "")
      updateTextInput(session, "largest_guess", value = "")
      updateTextInput(session, "second_guess",  value = "")
    }
  })
  
  # Give Up handler
  observeEvent(input$giveup, {
    req(current_state())
    
    total_attempts(total_attempts() + 1)
    
    given_up <- current_state()
    
    guessed_states(c(guessed_states(), given_up))
    remaining_states(setdiff(remaining_states(), given_up))
    
    # Show polygon for the given-up state
    leafletProxy("us_map") |>
      clearGroup("guessed") |>
      addPolygons(
        data = states |> filter(state_name %in% guessed_states()),
        fillColor = "steelblue",
        fillOpacity = 0.5,
        color = "white",
        weight = 1,
        label = ~ paste0(
          state_name,
          "<br>Population (2023): ",
          format(state_population, big.mark = ",")
        ) |>
          lapply(htmltools::HTML)
      )
    
    # Show its capital, largest city, and second largest city
    add_city_markers(given_up)
    
    # Extract capital, largest, and second largest cities for display
    state_info <- states |> filter(state_name == given_up)
    
    capname <- state_info$capital
    largest <- state_info$largest_city
    second <- state_info$second_largest_city
    
    output$feedback <- renderText(
      paste0(
        "🙈 The correct answer was: ",
        given_up,
        "\n",
        "🏛️ Capital: ",
        capname,
        "\n",
        "🌆 Largest city: ",
        largest,
        "\n",
        "🏙️ Second largest city: ",
        second
      )
    )
    
    # Advance
    next_states <- remaining_states()
    current_state(if (length(next_states) > 0) next_states[1] else NULL)
    
    updateTextInput(session, "state_guess", value = "")
    updateTextInput(session, "capital_guess", value = "")
    updateTextInput(session, "largest_guess", value = "")
    updateTextInput(session, "second_guess", value = "")
  })
  # Restart handler
  observeEvent(input$restart, {
    if (input$save) {
      save_quiz_results()
    }
    # reset your quiz state
    total_attempts(0)
    remaining_states(sample(states$state_name))
    guessed_states(character())
    score(0)
    current_state(NULL)
    wrong_states(character())
    wrong_capitals(character())
    wrong_largests(character())
    wrong_second_largests(character())
    
    # reset the map: remove every shape/marker group, then re‑add the all‑black polygons
    leafletProxy("us_map") |>
      clearShapes() |>            # removes all polygons (including guessed blue ones)
      clearGroup("capitals") |>    # removes any capital markers
      clearGroup("largest") |>     # removes any largest‑city markers
      clearGroup("second_largest") |> # removes second‑largest markers
      addPolygons(
        data = states,
        layerId = ~state_name,
        fillColor = "#000000",
        fillOpacity = 0.95,
        color = "white",
        weight = 1
      )
    
    output$feedback <- renderText("🌀 Quiz restarted! Good luck!")
    
    # clear out the inputs
    updateTextInput(session, "state_guess", value = "")
    updateTextInput(session, "capital_guess", value = "")
    updateTextInput(session, "largest_guess", value = "")
    updateTextInput(session, "second_guess", value = "")
  })
  
  
  # Score display
  output$score <- renderText({
    paste("Score:", score(), "/", total_attempts())
  })
  
  save_quiz_results <- function() {
    if (input$user_name == "") {
      showNotification(
        "⚠️ Please enter your name before saving.",
        type = "warning"
      )
      return()
    }
    
    result_row <- data.frame(
      date = as.character(Sys.Date()),
      name = input$user_name,
      score = score(),
      wrong_states = paste(wrong_states(), collapse = "; "),
      wrong_capitals = if (input$guess_capital)
        paste(wrong_capitals(), collapse = "; ") else NA,
      wrong_largests = if (input$guess_largest)
        paste(wrong_largests(), collapse = "; ") else NA,
      wrong_second_largests = if (input$guess_second)
        paste(wrong_second_largests(), collapse = "; ") else NA,
      stringsAsFactors = FALSE
    )
    
    sheet_url <- paste0(
      "https://docs.google.com/spreadsheets/d/",
      "13T0IL4pZO2RNwT-kWqbc7O4Trv2uRBkew8occILbWz8"
    )
    
    tryCatch(
      {
        googlesheets4::sheet_append(sheet_url, result_row)
        showNotification("✅ Results saved to Google Sheets!", type = "message")
      },
      error = function(e) {
        showNotification(paste("❌ Save failed:", e$message), type = "error")
      }
    )
  }
  
  observeEvent(input$save_results, {
    save_quiz_results()
  })
}

shinyApp(ui, server)
