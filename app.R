library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(ggpath)
library(ggimage)
library(ggiraph)

# ── Data Loading ──
League = read.csv("datasets/League.csv")
Team = read.csv("datasets/Team.csv")
Match = read.csv("datasets/Match.csv")
Possession = read.csv("datasets/Match_Possesion.csv")
team_lookup = Team %>% select(team_api_id, team_long_name)
league_lookup = League %>% select(country_id, name)

Final_Possession = Possession %>% filter(elapsed == 90)

Cleaned_Match = Match %>%
  left_join(team_lookup, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name) %>%
  left_join(team_lookup, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name) %>%
  left_join(league_lookup, by = c("league_id" = "country_id")) %>%
  rename(league_name = name)

Cleaned_Match = left_join(Cleaned_Match, Final_Possession, by = c("id" = "match_id"))

Cleaned_Match = Cleaned_Match %>%
  select(season, league_name, home_team_name, away_team_name,
         home_team_goal, away_team_goal, homepos, awaypos)

all_seasons = c("2008/2009", "2009/2010", "2010/2011", "2011/2012",
                "2012/2013", "2013/2014", "2014/2015", "2015/2016")

# ── Helper Functions ──
teams_in_league = function(league, season_range) {
  relevant_matches = Cleaned_Match %>% filter(season %in% season_range, league_name == league)
  unique(c(relevant_matches$home_team_name, relevant_matches$away_team_name))
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
    filter(if (side == "Both") TRUE else actual_side == side) %>%
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
  na.omit(result) %>% arrange(desc(win_rate))
}

calculate_team_rates_against = function(selected_teams, selected_seasons, against_team, side = "Both") {
  selected_teams_without_against <- selected_teams[!selected_teams == against_team]

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
    filter(team %in% selected_teams_without_against) %>%
    filter(if (side == "Both") TRUE else actual_side == side) %>%
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
  result %>% add_row(team = against_team, matches_played = NA, win_rate = NA, draw_rate = NA, loss_rate = NA)
}

match_plot = function(selected_teams, selected_seasons, side = "Both") {
  team_rates = calculate_team_rates(selected_teams, selected_seasons, side)
  long_team_rates = team_rates %>%
    pivot_longer(cols = ends_with("rate"), names_to = "result_type", values_to = "percentage") %>%
    mutate(result_clean = case_when(
      result_type == "win_rate" ~ "Win",
      result_type == "draw_rate" ~ "Draw",
      result_type == "loss_rate" ~ "Loss"
    ))
  long_team_rates$team = factor(long_team_rates$team, levels = rev(team_rates$team))
  long_team_rates$result_clean = factor(long_team_rates$result_clean, levels = c("Loss", "Draw", "Win"))
  long_team_rates$perc_label = round(as.numeric(long_team_rates$percentage) * 100, 1)

  p = ggplot(long_team_rates, aes(
    x = team, y = perc_label, fill = result_clean,
    text = paste0("Team: ", team, "<br>Result: ", result_clean, "<br>Rate: ", perc_label, "%")
  )) +
    geom_bar(position = "stack", stat = "identity", col = "black") +
    coord_flip() +
    theme_bw() +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#de8e08", "Draw" = "#ece134", "Win" = "#138f60")) +
    labs(y = "Percentage (%)", x = "Team")
  ggplotly(p, tooltip = "text")
}

match_plot_against = function(selected_teams, selected_seasons, against_team, side = "Both") {
  team_rates = calculate_team_rates(selected_teams, selected_seasons, side)
  team_rates_against = calculate_team_rates_against(selected_teams, selected_seasons, against_team, side)
  long_team_rates = team_rates_against %>%
    pivot_longer(cols = ends_with("rate"), names_to = "result_type", values_to = "percentage") %>%
    mutate(result_clean = case_when(
      result_type == "win_rate" ~ "Win",
      result_type == "draw_rate" ~ "Draw",
      result_type == "loss_rate" ~ "Loss"
    ))
  long_team_rates$team = factor(long_team_rates$team, levels = rev(team_rates$team))
  long_team_rates$result_clean = factor(long_team_rates$result_clean, levels = c("Loss", "Draw", "Win"))
  long_team_rates$perc_label = round(as.numeric(long_team_rates$percentage) * 100, 1)

  p = ggplot(long_team_rates, aes(
    x = team, y = perc_label, fill = result_clean,
    text = paste0("Team: ", team, "<br>Result: ", result_clean, "<br>Rate: ", perc_label, "%")
  )) +
    geom_bar(position = "stack", stat = "identity", col = "black") +
    coord_flip() +
    theme_bw() +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#de8e08", "Draw" = "#ece134", "Win" = "#138f60")) +
    labs(y = "Percentage (%)", x = "Team")
  ggplotly(p, tooltip = "text")
}

stats_per_team = function(selected_teams, selected_seasons, side = "Both") {
  home_stats = Cleaned_Match %>%
    filter(home_team_name %in% selected_teams, season %in% selected_seasons) %>%
    select(team = home_team_name, scored = home_team_goal, conceded = away_team_goal, pos = homepos)

  away_stats = Cleaned_Match %>%
    filter(away_team_name %in% selected_teams, season %in% selected_seasons) %>%
    select(team = away_team_name, scored = away_team_goal, conceded = home_team_goal, pos = awaypos)

  total_stats = if (side == "Home") home_stats else if (side == "Away") away_stats else rbind(home_stats, away_stats)

  total_stats %>% group_by(team) %>% summarise(
    avg_goals_scored = mean(scored, na.rm = TRUE),
    avg_goals_conceded = mean(conceded, na.rm = TRUE),
    avg_possession = mean(pos, na.rm = TRUE)
  )
}

average_stats_plot = function(selected_teams, selected_seasons, side = "Both") {
  average_stats = stats_per_team(selected_teams, selected_seasons, side) %>%
    mutate(
      logo_path = paste0("logos/", team, ".png"),
      pos_display = ifelse(is.na(avg_possession), "No Data", paste0(round(avg_possession, 1), "%")),
      pos_visual = ifelse(is.na(avg_possession), 50, avg_possession),
      tooltip = paste0("Team: ", team,
                       "\nPossession: ", pos_display,
                       "\nAvg Goals Scored: ", round(avg_goals_scored, 2),
                       "\nAvg Goals Conceded: ", round(avg_goals_conceded, 2))
    )

  p <- ggplot(average_stats, aes(x = avg_goals_scored, y = avg_goals_conceded)) +
    geom_point_interactive(
      aes(size = pos_visual, tooltip = tooltip, data_id = team),
      color = "black", fill = "white", shape = 21, stroke = 1.5
    ) +
    geom_from_path(aes(path = logo_path, width = pos_visual / 1500)) +
    geom_abline(slope = 1, linetype = "dashed", alpha = 0.4) +
    theme_bw() +
    labs(x = "Average goals scored", y = "Average goals conceded", size = "Average possession rate")
  girafe(ggobj = p, options = list(opts_sizing(rescale = TRUE)))
}

# ── UI ──
ui <- navbarPage(
  title = "European Football Analytics",
  theme = NULL,

  tabPanel("League Performance",
    fluidRow(
      column(3, selectInput("perf_league", "Select League", League$name)),
      column(3, selectInput("perf_against_team", "Compare Against Team", choices = NULL)),
      column(3, sliderTextInput(
        inputId = "perf_season_rng", label = "Select Season Range",
        choices = all_seasons, selected = c("2008/2009", "2015/2016"), grid = TRUE
      )),
      column(3, radioButtons("perf_side", "Match Side",
                             choices = c("Both", "Home", "Away"), selected = "Both", inline = TRUE))
    ),
    fluidRow(
      column(12, pickerInput(
        inputId = "perf_teams", label = "Filter Teams to Display",
        choices = NULL, multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ))
    ),
    hr(),
    fluidRow(
      column(6,
        h4("Overall Performance in League"),
        plotlyOutput("perf_plot_overall")
      ),
      column(6,
        h4(textOutput("perf_against_title")),
        plotlyOutput("perf_plot_against")
      )
    )
  ),

  tabPanel("Team Statistics",
    fluidRow(
      column(4, selectInput("stats_league", "Select League", League$name)),
      column(4, sliderTextInput(
        inputId = "stats_season_rng", label = "Select Season Range",
        choices = all_seasons, selected = c("2008/2009", "2015/2016"), grid = TRUE
      )),
      column(4, radioButtons("stats_side", "Match Side",
                             choices = c("Both", "Home", "Away"), selected = "Both", inline = TRUE))
    ),
    fluidRow(
      column(12, pickerInput(
        inputId = "stats_teams", label = "Filter Teams to Display",
        choices = NULL, multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ))
    ),
    hr(),
    fluidRow(
      column(8, offset = 2,
        h4("Average Possession, Goals Scored & Conceded"),
        girafeOutput("stats_plot")
      )
    )
  )
)

# ── Server ──
server <- function(input, output, session) {

  # ── League Performance tab ──
  perf_seasons <- reactive({
    req(input$perf_season_rng)
    start_idx = which(all_seasons == input$perf_season_rng[1])
    end_idx = which(all_seasons == input$perf_season_rng[2])
    all_seasons[start_idx:end_idx]
  })

  perf_league_teams <- reactive({
    req(input$perf_league)
    teams_in_league(input$perf_league, perf_seasons())
  })

  observeEvent(perf_league_teams(), {
    updatePickerInput(session, "perf_teams",
                      choices = sort(perf_league_teams()),
                      selected = perf_league_teams())
    updateSelectInput(session, "perf_against_team",
                      choices = sort(perf_league_teams()))
  })

  output$perf_plot_overall = renderPlotly({
    req(input$perf_teams, input$perf_side)
    match_plot(input$perf_teams, perf_seasons(), input$perf_side)
  })

  output$perf_plot_against = renderPlotly({
    req(input$perf_teams, input$perf_against_team, input$perf_side)
    match_plot_against(input$perf_teams, perf_seasons(), input$perf_against_team, input$perf_side)
  })

  output$perf_against_title <- renderText({
    req(input$perf_against_team)
    paste("Performance Against:", input$perf_against_team)
  })

  # ── Team Statistics tab ──
  stats_seasons <- reactive({
    req(input$stats_season_rng)
    start_idx = which(all_seasons == input$stats_season_rng[1])
    end_idx = which(all_seasons == input$stats_season_rng[2])
    all_seasons[start_idx:end_idx]
  })

  stats_league_teams <- reactive({
    req(input$stats_league)
    teams_in_league(input$stats_league, stats_seasons())
  })

  observeEvent(stats_league_teams(), {
    updatePickerInput(session, "stats_teams",
                      choices = sort(stats_league_teams()),
                      selected = stats_league_teams())
  })

  output$stats_plot = renderGirafe({
    req(input$stats_teams, input$stats_side)
    average_stats_plot(input$stats_teams, stats_seasons(), input$stats_side)
  })
}

shinyApp(ui = ui, server = server)
