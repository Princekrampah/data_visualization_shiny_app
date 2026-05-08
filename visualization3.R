library(tidyverse)
library(ggplot2)
library(ggpath)
library(ggimage)
library(plotly)
library(ggiraph)
library(shiny)
library(shinyWidgets)

League = read.csv("datasets/League.csv")
Team = read.csv("datasets/Team.csv")
Match = read.csv("datasets/Match.csv")
Possession = read.csv("datasets/Match_Possesion.csv")
team_lookup = Team %>% select(team_api_id, team_long_name)
league_lookup = League %>% select(country_id, name)

Final_Possession = Possession %>% filter(elapsed==90)
Cleaned_Match = Match %>%
  left_join(team_lookup, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name) %>%
  
  left_join(team_lookup, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name) %>% 
  
  left_join(league_lookup, by = c("league_id" = "country_id")) %>%
  rename(league_name = name)

Cleaned_Match = left_join(Cleaned_Match, Final_Possession,  by = c("id" = "match_id"))

Cleaned_Match = Cleaned_Match %>% select(season, league_name, home_team_name, away_team_name, home_team_goal, away_team_goal, homepos, awaypos)
all_seasons = c("2008/2009", "2009/2010", "2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", "2015/2016")

teams_in_league = function(league, season_range) {
  
  relevant_matches = Cleaned_Match %>% filter(season %in% season_range, league_name==league)
  return(unique(c(relevant_matches$home_team_name, relevant_matches$away_team_name)))
  
}

stats_per_team = function(selected_teams, selected_seasons, side="Both") {
  
  home_stats = Cleaned_Match %>% filter(home_team_name %in% selected_teams, season %in% selected_seasons) %>%
    select(team = home_team_name, scored=home_team_goal, conceded=away_team_goal, pos=homepos)
  
  away_stats = Cleaned_Match %>% filter(away_team_name %in% selected_teams, season %in% selected_seasons) %>%
    select(team = away_team_name, scored=away_team_goal, conceded=home_team_goal, pos=awaypos)
  
  if (side=="Both") {
    total_stats = rbind(home_stats, away_stats)
  } else if (side=="Home") {
    total_stats = home_stats
  } else if (side=="Away") {
    total_stats = away_stats
  }
  average_stats = total_stats %>% group_by(team) %>% summarise(
    avg_goals_scored = mean(scored, na.rm = TRUE),
    avg_goals_conceded = mean(conceded, na.rm = TRUE),
    avg_possession = mean(pos, na.rm = TRUE)
  )
  return(average_stats)
}
average_stats_plot = function(selected_teams, selected_seasons, side="Both") {
  
  average_stats = stats_per_team(selected_teams, selected_seasons, side) %>%
    mutate(
      logo_path = paste0("logos/", team, ".png"),
      # Create a display version of possession for the tooltip
      pos_display = ifelse(is.na(avg_possession), "No Data", paste0(round(avg_possession, 1), "%")),
      
      # Create a visual sizing variable: use 50% as a 'regular size' fallback
      pos_visual = ifelse(is.na(avg_possession), 50, avg_possession),
      
      tooltip = paste0("Team: ", team, 
                       "\nPossession: ", pos_display,
                       "\nAvg Goals Scored: ", round(avg_goals_scored, 2),
                       "\nAvg Goals Conceded: ", round(avg_goals_conceded, 2))
    )
  
  p <- ggplot(average_stats, aes(x = avg_goals_scored, y = avg_goals_conceded)) +
    # Use the fallback 'pos_visual' for size and width
    geom_point_interactive(
      aes(size = pos_visual, tooltip = tooltip, data_id = team), 
      color = "black", fill = "white", shape = 21, stroke = 1.5
    ) +
    geom_from_path(
      aes(path = logo_path, width = pos_visual / 1500)
    ) + 
    geom_abline(slope = 1, linetype = "dashed", alpha = 0.4) +
    theme_bw() + 
    labs(
      x = "Average goals scored",
      y = "Average goals conceded", 
      size = "Average possession rate"
    )
  girafe(ggobj = p, options = list(opts_sizing(rescale = TRUE)))
}

ui <- fluidPage(
  titlePanel("Overall average statistics of teams"),
  
  fluidRow(
    column(4, selectInput("chosen_league", "Select League", League$name)),
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
    column(6, offset=3, 
           h4("Average possession, goals scored, goals conceded rate"),
           girafeOutput("plot1")
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
  output$plot1 = renderGirafe({
    # req() ensures the plot doesn't crash if inputs are empty
    req(input$selected_teams_subset, input$side_toggle)
    
    average_stats_plot(
      selected_teams = input$selected_teams_subset, 
      selected_seasons = selected_seasons(), 
      side = input$side_toggle
    )
  })
}
# Return a Shiny app object
shinyApp(ui = ui, server = server)
