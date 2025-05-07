# source("prep.R")

library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(tidyverse)

states <- read_rds("states_enriched2023.rds")

library(googlesheets4)

googlesheets4::gs4_auth(path = "shiny-sheets-writer-key.json")


# UI
ui <- fluidPage(
  titlePanel("US State/District Shape Quiz"),
  h4("Created by Chester Ismay"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("question_number"),
      uiOutput("progress_bar"),
      plotOutput("state_shape", height = "200px"),
      textInput("state_guess", "Guess the state/district name:"),
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
      helpText(
        paste("You can make unlimited guesses for each question by",
        "clicking on 'Submit Guess'.")
      ),
      helpText(
        paste("If you're stuck on a question, click",
        "'Give Up on This Question' to see the answer.")
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
      leafletOutput("us_map", height = "600px"),
      verbatimTextOutput("score")
    )
  )
)

# Server
server <- function(input, output, session) {
  remaining_states <- reactiveVal(sample(states$state_name))
  guessed_states <- reactiveVal(character())
  score <- reactiveVal(0)
  total_attempts <- reactiveVal(0)
  current_state <- reactiveVal(NULL)
  wrong_states <- reactiveVal(character())
  wrong_capitals <- reactiveVal(character())
  wrong_largests <- reactiveVal(character())
  wrong_second_largests <- reactiveVal(character())

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

  # Draw the state shape
  output$state_shape <- renderPlot({
    req(current_state())
    shape <- states |> filter(state_name == current_state())
    ggplot(shape) +
      geom_sf(fill = "black") +
      theme_void()
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

    answered <- current_state()
    guess <- normalize_state(input$state_guess)
    correct <- normalize_state(answered)
    is_state_correct <- guess == correct

    # Capital check
    is_capital_correct <- TRUE
    if (is_state_correct && input$guess_capital) {
      true_cap <- states |>
        filter(state_name == answered) |>
        pull(capital)
      is_capital_correct <- normalize_capital(input$capital_guess) ==
        normalize_capital(true_cap)
    }

    is_largest_correct <- TRUE
    if (is_state_correct && input$guess_largest) {
      true_largest <- states |>
        filter(state_name == answered) |>
        pull(largest_city)
      is_largest_correct <- normalize_city(input$largest_guess) ==
        normalize_city(true_largest)
    }

    is_second_correct <- TRUE
    if (is_state_correct && input$guess_second) {
      true_second <- states |>
        filter(state_name == answered) |>
        pull(second_largest_city)
      is_second_correct <- normalize_city(input$second_guess) ==
        normalize_city(true_second)
    }

    feedback_parts <- c()

    if (is_state_correct) {
      feedback_parts <- c(feedback_parts, "✅ State correct")
    } else {
      feedback_parts <- c(feedback_parts, "❌ State incorrect")
      wrong_states(c(wrong_states(), current_state()))
    }

    if (input$guess_capital) {
      if (is_capital_correct) {
        feedback_parts <- c(feedback_parts, "✅ Capital correct")
      } else {
        feedback_parts <- c(feedback_parts, "❌ Capital incorrect")
        wrong_capitals(c(wrong_capitals(), current_state()))
      }
    }

    if (input$guess_largest) {
      if (is_largest_correct) {
        feedback_parts <- c(feedback_parts, "✅ Largest city correct")
      } else {
        feedback_parts <- c(feedback_parts, "❌ Largest city incorrect")
        wrong_largests(c(wrong_largests(), current_state()))
      }
    }

    if (input$guess_second) {
      if (is_second_correct) {
        feedback_parts <- c(feedback_parts, "✅ Second largest city correct")
      } else {
        feedback_parts <- c(feedback_parts, "❌ Second largest city incorrect")
        wrong_second_largests(c(wrong_second_largests(), current_state()))
      }
    }

    output$feedback <- renderText(paste(feedback_parts, collapse = "\n"))

    if (
      is_state_correct &&
        is_capital_correct &&
        is_largest_correct &&
        is_second_correct
    ) {
      guessed_states(c(guessed_states(), answered))
      score(score() + 1)
      remaining_states(setdiff(remaining_states(), answered))

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

      add_city_markers(answered)

      next_states <- remaining_states()
      current_state(if (length(next_states) > 0) next_states[1] else NULL)

      updateTextInput(session, "state_guess", value = "")
      updateTextInput(session, "capital_guess", value = "")
      updateTextInput(session, "largest_guess", value = "")
      updateTextInput(session, "second_guess", value = "")
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
    total_attempts(0)
    remaining_states(sample(states$state_name))
    guessed_states(character())
    score(0)
    current_state(NULL)
    wrong_states(character())
    wrong_capitals(character())
    wrong_largests(character())
    wrong_second_largests(character())

    leafletProxy("us_map") |>
      clearGroup("guessed") |>
      clearGroup("capitals")

    output$feedback <- renderText("🌀 Quiz restarted! Good luck!")
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
