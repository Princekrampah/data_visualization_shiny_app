library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)

League = read.csv("datasets/League.csv")
Team = read.csv("datasets/Team.csv")
Match = read.csv("datasets/Match.csv")
team_lookup = Team %>% select(team_api_id, team_long_name)
league_lookup = League %>% select(country_id, name)

Cleaned_Match = Match %>%
  left_join(team_lookup, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name) %>%
  
  left_join(team_lookup, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name) %>% 
  
  left_join(league_lookup, by = c("league_id" = "country_id")) %>%
  rename(league_name = name)

Cleaned_Match = Cleaned_Match %>% select(season, league_name, home_team_name, away_team_name, home_team_goal, away_team_goal)
all_seasons = c("2008/2009", "2009/2010", "2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", "2015/2016")

teams_in_league = function(league, season_range) {
  
  relevant_matches = Cleaned_Match %>% filter(season %in% season_range, league_name==league)
  return(unique(c(relevant_matches$home_team_name, relevant_matches$away_team_name)))
  
}

calculate_team_rates = function(selected_teams, selected_seasons, side = "Both") {
  
  result = Cleaned_Match %>%
    filter(season %in% selected_seasons) %>%
    filter(home_team_name %in% selected_teams, away_team_name %in% selected_teams) %>%
    bind_rows(
      rename(., team = home_team_name, opponent = away_team_name, 
             goals_for = home_team_goal, goals_against = away_team_goal) %>% mutate(actual_side = "Home"),
      rename(., team = away_team_name, opponent = home_team_name, 
             goals_for = away_team_goal, goals_against = home_team_goal) %>% mutate(actual_side = "Away")
    ) %>%
    # Filter based on the toggle
    filter(if(side == "Both") TRUE else actual_side == side) %>%
    mutate(outcome = case_when(
      goals_for > goals_against ~ "Win",
      goals_for < goals_against ~ "Loss",
      TRUE ~ "Draw"
    )) %>%
    group_by(team) %>%
    summarise(
      matches_played = n(),
      win_rate  = sum(outcome == "Win") / matches_played,
      draw_rate = sum(outcome == "Draw") / matches_played,
      loss_rate = sum(outcome == "Loss") / matches_played,
      .groups = "drop"
    )
  
  return(na.omit(result) %>% arrange(desc(win_rate)))
}

calculate_team_rates_against = function(selected_teams, selected_seasons, against_team, side = "Both") {
  selected_teams_without_against <- selected_teams[! selected_teams == against_team]
  
  result = Cleaned_Match %>%
    filter(season %in% selected_seasons) %>%
    filter(
      (home_team_name %in% selected_teams_without_against & away_team_name == against_team) |
        (away_team_name %in% selected_teams_without_against & home_team_name == against_team)
    ) %>%
    bind_rows(
      rename(., team = home_team_name, opponent = away_team_name, 
             goals_for = home_team_goal, goals_against = away_team_goal) %>% mutate(actual_side = "Home"),
      rename(., team = away_team_name, opponent = home_team_name, 
             goals_for = away_team_goal, goals_against = home_team_goal) %>% mutate(actual_side = "Away")
    ) %>%
    # Filter for the selected side AND ensure we only look at the selected teams' perspective
    filter(team %in% selected_teams_without_against) %>%
    filter(if(side == "Both") TRUE else actual_side == side) %>%
    mutate(outcome = case_when(
      goals_for > goals_against ~ "Win",
      goals_for < goals_against ~ "Loss",
      TRUE ~ "Draw"
    )) %>%
    group_by(team) %>%
    summarise(
      matches_played = n(),
      win_rate  = sum(outcome == "Win") / matches_played,
      draw_rate = sum(outcome == "Draw") / matches_played,
      loss_rate = sum(outcome == "Loss") / matches_played,
      .groups = "drop"
    )
  
  result = na.omit(result) %>% arrange(desc(win_rate))
  result = result %>% add_row(team = against_team, matches_played=NA, win_rate=NA, draw_rate=NA, loss_rate=NA)
  return(result)
}

match_plot = function(selected_teams, selected_seasons, side="Both") {
  team_rates = calculate_team_rates(selected_teams, selected_seasons, side)
  long_team_rates = team_rates %>%
    pivot_longer(
      cols = ends_with("rate"),   # Select columns ending in 'rate' to pivot
      names_to = "result_type",    # Name for the new category column
      values_to = "percentage"    # Name for the new value column
    ) %>% mutate(result_clean = case_when(
      result_type == "win_rate" ~ "Win",
      result_type == "draw_rate" ~ "Draw",
      result_type == "loss_rate" ~ "Loss"
    ))
  
  long_team_rates$team = factor(long_team_rates$team, levels = rev(team_rates$team))
  long_team_rates$result_clean = factor(long_team_rates$result_clean, levels = c("Loss", "Draw", "Win"))
  
  # Pre-calculate rounded percentage
  long_team_rates$perc_label = round(as.numeric(long_team_rates$percentage) * 100, 1)
  
  # 3. Create the ggplot with a custom 'text' aesthetic for plotly
  p = ggplot(long_team_rates, aes(
    x = team, 
    y = perc_label, 
    fill = result_clean,
    # This 'text' aesthetic is what plotly uses for the hover
    text = paste0("Team: ", team, 
                  "<br>Result: ", result_clean, 
                  "<br>Rate: ", perc_label, "%")
  )) +
    geom_bar(position = "stack", stat = "identity", col = "black") + 
    coord_flip() + 
    theme_bw() +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#de8e08", "Draw" = "#ece134", "Win" = "#138f60")) +
    labs(y = "Percentage (%)", x = "Team")
  
  # 4. Convert to plotly and tell it to ONLY use the 'text' aesthetic for the tooltip
  return(ggplotly(p, tooltip = "text"))
}

match_plot_against = function(selected_teams, selected_seasons, against_team,side="Both") {
  team_rates = calculate_team_rates(selected_teams, selected_seasons,side) # To keep same ordering
  team_rates_against = calculate_team_rates_against(selected_teams, selected_seasons, against_team,side)
  long_team_rates = team_rates_against %>%
    pivot_longer(
      cols = ends_with("rate"),   # Select columns ending in 'rate' to pivot
      names_to = "result_type",    # Name for the new category column
      values_to = "percentage"    # Name for the new value column
    ) %>% mutate(result_clean = case_when(
      result_type == "win_rate" ~ "Win",
      result_type == "draw_rate" ~ "Draw",
      result_type == "loss_rate" ~ "Loss"
    )) 
  long_team_rates$team = factor(long_team_rates$team, levels = rev(team_rates$team))
  long_team_rates$result_clean = factor(long_team_rates$result_clean, levels = c("Loss", "Draw", "Win"))
  
  # Pre-calculate rounded percentage
  long_team_rates$perc_label = round(as.numeric(long_team_rates$percentage) * 100, 1)
  
  # 3. Create the ggplot with a custom 'text' aesthetic for plotly
  p = ggplot(long_team_rates, aes(
    x = team, 
    y = perc_label, 
    fill = result_clean,
    # This 'text' aesthetic is what plotly uses for the hover
    text = paste0("Team: ", team, 
                  "<br>Result: ", result_clean, 
                  "<br>Rate: ", perc_label, "%")
  )) +
    geom_bar(position = "stack", stat = "identity", col = "black") + 
    coord_flip() + 
    theme_bw() +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#de8e08", "Draw" = "#ece134", "Win" = "#138f60")) +
    labs(y = "Percentage (%)", x = "Team")
  
  # 4. Convert to plotly and tell it to ONLY use the 'text' aesthetic for the tooltip
  return(ggplotly(p, tooltip = "text"))
}

# Define the UI
ui <- fluidPage(
  titlePanel("League Performance Analysis"),
  
  fluidRow(
    column(4, selectInput("chosen_league", "Select League", League$name)),
    column(4, selectInput("chosen_team", "Select Main Team (for 'Against' plot)", choices = NULL)),
    column(4, sliderTextInput(
      inputId = "season_rng",
      label = "Select Season Range", 
      choices = all_seasons,
      selected = c("2008/2009", "2015/2016"),
      grid = TRUE
    )),
    column(4, radioButtons("side_toggle", "Match Side", 
                           choices = c("Both", "Home", "Away"), 
                           selected = "Both", inline = TRUE)),
    column(4, 
           pickerInput(
             inputId = "selected_teams_subset",
             label = "Filter Teams to Display", 
             choices = NULL, # Will be updated by server
             multiple = TRUE,
             options = list(`actions-box` = TRUE) # Adds Select All/Deselect All buttons
           )
    )
  ),
  
  hr(), # Horizontal line for visual separation
  
  fluidRow(
    column(6, 
           h4("Overall Performance in League"),
           plotlyOutput("plot1")
    ),
    column(6, 
           h4(textOutput("against_title")), # Dynamic title
           plotlyOutput("plot2")
    )
  )
)

# Define the server code
server <- function(input, output, session) {
  
  # 1. Reactive for the season range (converts slider input to a vector of strings)
  selected_seasons <- reactive({
    req(input$season_rng)
    start_idx = which(all_seasons == input$season_rng[1])
    end_idx = which(all_seasons == input$season_rng[2])
    all_seasons[start_idx:end_idx]
  })
  
  # 2. Reactive for all teams available in the selected league/seasons
  league_teams <- reactive({
    req(input$chosen_league)
    teams_in_league(input$chosen_league, selected_seasons())
  })
  
  # 3. Update UI Inputs (Team Picker and "Against" Team dropdown)
  observeEvent(league_teams(), {
    # Update the "Brushing" filter (Picker)
    updatePickerInput(session, "selected_teams_subset", 
                      choices = sort(league_teams()),
                      selected = league_teams()) 
    
    # Update the target team for comparison
    updateSelectInput(session, "chosen_team", 
                      choices = sort(league_teams()))
  })
  
  # 4. Render Plot 1: Overall performance of the selected subset
  output$plot1 = renderPlotly({
    # req() ensures the plot doesn't crash if inputs are empty
    req(input$selected_teams_subset, input$side_toggle)
    
    match_plot(
      selected_teams = input$selected_teams_subset, 
      selected_seasons = selected_seasons(), 
      side = input$side_toggle
    )
  })
  
  # 5. Render Plot 2: Performance of the subset AGAINST a specific team
  output$plot2 = renderPlotly({
    req(input$selected_teams_subset, input$chosen_team, input$side_toggle)
    
    match_plot_against(
      selected_teams = input$selected_teams_subset, 
      selected_seasons = selected_seasons(), 
      against_team = input$chosen_team, 
      side = input$side_toggle
    )
  })
  
  # 6. Dynamic title for the second plot
  output$against_title <- renderText({
    req(input$chosen_team)
    paste("Performance Against:", input$chosen_team)
  })
}
# Return a Shiny app object
shinyApp(ui = ui, server = server)