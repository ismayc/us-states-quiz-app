source("prep.R")

library(googlesheets4)

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
      actionButton("submit", "Submit Guess"),
      actionButton("giveup", "Give Up on This State/District"),
      actionButton("restart", "Restart Quiz"),
      verbatimTextOutput("feedback"),
      checkboxInput("save", "Log results to Google Sheets?", value = FALSE),
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
  guessed_states   <- reactiveVal(character())
  score            <- reactiveVal(0)
  current_state    <- reactiveVal(NULL)
  wrong_states     <- reactiveVal(character())
  wrong_capitals   <- reactiveVal(character())
  
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
            "background-color: #428bca; width: ", percent, "%; ",
            "height: 100%; border-radius: 5px;"
          )
        )
      ),
      div(paste0(percent, "% completed"), style = "font-size: 12px; margin-top: 4px;")
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
      true_cap <- states |>
        filter(state_name == answered) |>
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
      leafletProxy("us_map") |>
        clearGroup("guessed") |>
        addPolygons(
          data = states |> filter(state_name %in% guessed_states()),
          fillColor = "steelblue", fillOpacity = 0.5,
          color = "white", weight = 1,
          label = ~state_name, group = "guessed"
        )
      
      # Draw the capital marker for the answered state
      coords <- states |>
        filter(state_name == answered) |>
        st_centroid() |>
        st_coordinates()
      capname <- states |>
        filter(state_name == answered) |>
        pull(capital)
      
      leafletProxy("us_map") |>
        clearGroup("capitals") |>
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
      wrong_states(c(wrong_states(), current_state()))
      output$feedback <- renderText("❌ Incorrect state guess.")
      
    } else if (!is_capital_correct) {
      wrong_capitals(c(wrong_capitals(), current_state()))
      output$feedback <- renderText("✅ State correct, ❌ but capital is incorrect.")
    }
  })
  
  # Give Up handler
  observeEvent(input$giveup, {
    req(current_state())
    given_up <- current_state()
    
    guessed_states(c(guessed_states(), given_up))
    remaining_states(setdiff(remaining_states(), given_up))
    
    # Show polygon for the given-up state
    leafletProxy("us_map") |>
      clearGroup("guessed") |>
      addPolygons(
        data = states |> filter(state_name %in% guessed_states()),
        fillColor = "steelblue", fillOpacity = 0.5,
        color = "white", weight = 1,
        label = ~state_name, group = "guessed"
      )
    
    # Show its capital
    coords <- states |>
      filter(state_name == given_up) |>
      st_centroid() |>
      st_coordinates()
    capname <- states |>
      filter(state_name == given_up) |>
      pull(capital)
    leafletProxy("us_map") |>
      clearGroup("capitals") |>
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
    if (input$save) {
      save_quiz_results()
    }
    remaining_states(sample(states$state_name))
    guessed_states(character())
    score(0)
    current_state(NULL)
    wrong_states(character())
    wrong_capitals(character())
    
    leafletProxy("us_map") |>
      clearGroup("guessed") |>
      clearGroup("capitals")
    
    output$feedback <- renderText("🌀 Quiz restarted! Good luck!")
    updateTextInput(session, "state_guess", value = "")
    updateTextInput(session, "capital_guess", value = "")
  })
  
  
  # Score display
  output$score <- renderText({
    paste("Score:", score(), "/", nrow(states))
  })
  
  save_quiz_results <- function() {
    if (input$user_name == "") {
      showNotification("⚠️ Please enter your name before saving.", type = "warning")
      return()
    }
    
    result_row <- data.frame(
      date = as.character(Sys.Date()),
      name = input$user_name,
      score = score(),
      wrong_states = paste(wrong_states(), collapse = "; "),
      wrong_capitals = if (input$guess_capital) paste(wrong_capitals(), collapse = "; ") else NA,
      stringsAsFactors = FALSE
    )
    
    sheet_url <- "https://docs.google.com/spreadsheets/d/13T0IL4pZO2RNwT-kWqbc7O4Trv2uRBkew8occILbWz8"
    
    tryCatch({
      googlesheets4::sheet_append(sheet_url, result_row)
      showNotification("✅ Results saved to Google Sheets!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message), type = "error")
    })
  }
  
  observeEvent(input$save_results, {
    save_quiz_results()
  })
  
  
}



shinyApp(ui, server)
