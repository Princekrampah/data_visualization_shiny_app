library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(ggpath)
library(ggimage)
library(ggiraph)
library(bslib)


# Data Loading
load("data.RData")
min_age = min(Player_Data$age,na.rm=T)
max_age = max(Player_Data$age,na.rm=T)
ages = min_age:max_age
# Plot Theme
dark_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "#233545", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#8fa8be"),
    axis.title = element_text(color = "#c5d0db"),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.text = element_text(color = "#c5d0db"),
    legend.title = element_text(color = "#e0e6ed")
  )

plotly_dark_layout <- function(p) {
  p %>% layout(
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent",
    font = list(color = "#c5d0db"),
    xaxis = list(gridcolor = "#233545", zerolinecolor = "#233545"),
    yaxis = list(gridcolor = "#233545", zerolinecolor = "#233545"),
    legend = list(font = list(color = "#c5d0db"))
  )
}

# Helper Functions
teams_in_league = function(league, season_range) {
  relevant_matches = Full_Match %>% filter(season %in% season_range, league_name == league)
  unique(c(relevant_matches$home_team_name, relevant_matches$away_team_name))
}

calculate_team_rates = function(selected_teams, selected_seasons, side = "Both") {
  result = Full_Match %>%
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

  result = Full_Match %>%
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
    geom_bar(position = "stack", stat = "identity", col = "#0f1923", linewidth = 0.3) +
    coord_flip() +
    dark_theme +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#e74c3c", "Draw" = "#f39c12", "Win" = "#1abc54")) +
    labs(y = "Percentage (%)", x = "Team")
  ggplotly(p, tooltip = "text") %>% plotly_dark_layout()
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
    geom_bar(position = "stack", stat = "identity", col = "#0f1923", linewidth = 0.3) +
    coord_flip() +
    dark_theme +
    scale_fill_manual(name = "Result",
                      values = c("Loss" = "#e74c3c", "Draw" = "#f39c12", "Win" = "#1abc54")) +
    labs(y = "Percentage (%)", x = "Team")
  ggplotly(p, tooltip = "text") %>% plotly_dark_layout()
}

stats_per_team = function(selected_teams, selected_seasons, side = "Both") {
  home_stats = Full_Match %>%
    filter(home_team_name %in% selected_teams, season %in% selected_seasons) %>%
    select(team = home_team_name, scored = home_team_goal, conceded = away_team_goal, pos = homepos)

  away_stats = Full_Match %>%
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
      # Create a status flag for the border color
      has_possession = ifelse(is.na(avg_possession), "No Data", "Has Data"),
      pos_display = ifelse(is.na(avg_possession), "No Data", paste0(round(avg_possession, 1), "%")),
      # Use 50 as a neutral baseline size for "No Data" teams
      pos_visual = ifelse(is.na(avg_possession), 50, avg_possession),
      tooltip = paste0("Team: ", team,
                       "\nPossession: ", pos_display,
                       "\nAvg Goals Scored: ", round(avg_goals_scored, 2),
                       "\nAvg Goals Conceded: ", round(avg_goals_conceded, 2))
    )
  
  p = ggplot(average_stats, aes(x = avg_goals_scored, y = avg_goals_conceded)) +
    geom_point_interactive(
      aes(size = pos_visual,
          tooltip = tooltip,
          data_id = team,
          color = has_possession),
      fill = "#172633",
      shape = 21,
      stroke = 2.5
    ) +
    geom_from_path(aes(path = logo_path, width = pos_visual / 1500)) +
    geom_abline(slope = 1, linetype = "dashed", alpha = 0.3, color = "#5e7a90") +
    dark_theme +
    scale_color_manual(values = c("Has Data" = "#1abc54", "No Data" = "#e74c3c")) +
    scale_x_continuous(expand = expansion(mult = 0.15)) +
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    labs(x = "Average goals scored",
         y = "Average goals conceded",
         size = "Average possession rate",
         color = "Data Status")
  
  girafe(
    ggobj = p, 
    width_svg = 8, 
    height_svg = 6,
    options = list(
      opts_zoom(max = 5),
      opts_selection(type = "none"), 
      
      # This part disables the rectangular zoom and pan buttons 
      # but keeps the "Reset" and "Download" buttons
      opts_toolbar(
        position = "topright", 
        saveaspng = TRUE,
        hidden = c("zoom_rect", "zoom_in", "zoom_out", "pan")
      ),
      
      opts_sizing(rescale = TRUE, width = 1.0)
    )
  )
}
improvement_plot = function(teams="all", age_range=min_age:max_age,min_improvement=5) {
  Subset_Player_Data = Player_Data %>% filter(age %in% age_range, improvement_vs_2015 >= min_improvement | is.na(improvement_vs_2015))
  if (!identical(teams, "all")) {
    Subset_Player_Data = Subset_Player_Data %>% filter(team_name %in% teams)
  }
  p = ggplot(Subset_Player_Data, aes(x=age,
                                     y=overall_rating,
                                     size=improvement_vs_2015,
                                     shape=position,
                                     col=position,
                                     key = player_id,
                                     text = paste0(
                                       "Player: ", player_name,
                                       "\nRating: ", overall_rating,
                                       "\nAge: ", age,
                                       "\nImprovement: ", improvement_vs_2015,
                                       "\nPosition: ", position,
                                       "\nTeam: ", team_name
                                     ))) +
    geom_jitter(alpha=0.75) + dark_theme +
    labs(col="Position", x="Age", y="Overall rating", shape="",size="") +
    scale_color_manual(values= c("Attacker" ="#f1c40f",
                                 "Midfielder" = "#e67e22",
                                 "Defender" ="#1abc54",
                                 "Goalkeeper"="#3498db")) +
    guides(size = guide_legend(override.aes = list(shape = 16)))
  return(ggplotly(p,tooltip = "text", source = "scatter") %>% plotly_dark_layout())
}
improvement_line_plot = function(player_ids) {
  player_ids = as.numeric(player_ids)
  Subset_Player_Data = Player_Data %>% filter(player_id %in% player_ids) %>%
    select(player_id, player_name, team_name, player_name_and_team, position)
  Subset_Player_Attributes = Player_Attributes %>% filter(player_api_id %in% player_ids) %>%
    select(player_fifa_api_id, player_api_id, date, overall_rating, potential)
  Player_Improvement = left_join(Subset_Player_Attributes, Subset_Player_Data,
                                       by = c("player_api_id" = "player_id"))
  p = ggplot(Player_Improvement, aes(x = date, 
                                     y = overall_rating, 
                                     col = player_name_and_team,
                                     group = player_name_and_team, 
                                     text = paste0(
                                       "Player: ", player_name,
                                       "\nDate: ", date,
                                       "\nOverall Rating: ", overall_rating,
                                       "\nPosition: ", position
                                     ))) +
    geom_line(linewidth = 1, alpha=0.9) + geom_point(size=1,alpha = 0.9) +
    dark_theme +
    labs(col = "Player", x="Date", y="Overall rating")

  return(ggplotly(p, tooltip = "text") %>% plotly_dark_layout())
}

radar_plot = function(player_ids) {
  plot_data = Player_Data %>% 
    filter(player_id %in% player_ids) %>%
    select(player_name, team_name, 
           `Short Passing` = short_passing, 
           `Long Passing` = long_passing, 
           Stamina = stamina, 
           Crossing = crossing, 
           `Ball Control` = ball_control)
  
  plot_data = plot_data %>%
    rowwise() %>%
    mutate(avg_score = round(mean(c_across(`Short Passing`:`Ball Control`)), 1)) %>%
    ungroup()
  
  plot_data_long = plot_data %>%
    pivot_longer(cols = -c(player_name, team_name, avg_score), 
                 names_to = "attribute", 
                 values_to = "value")
  
  min_val = min(50, round(min(plot_data_long$value) / 10) * 10)
  
  p = plot_ly(type = "scatterpolar")
  
  colors = c("#1abc54", "#3498db", "#f39c12")
  players = unique(plot_data_long$player_name)

  for(i in 1:length(players)) {
    player_subset = plot_data_long %>% filter(player_name == players[i])
    player_subset = rbind(player_subset, player_subset[1,])

    p = p %>% add_trace(
      r = player_subset$value,
      theta = player_subset$attribute,
      name = paste0(players[i], " (Avg: ", player_subset$avg_score[1], ")"),
      line = list(color = colors[i], width = 3),
      marker = list(color = colors[i]),
      fillcolor = paste0(colors[i], "33"),
      fill = "toself",
      text = paste0("Player: ", player_subset$player_name,
                    "\nTeam: ", player_subset$team_name,
                    "\nAttribute: ", player_subset$attribute,
                    "\nValue: ", player_subset$value),
      hoverinfo = "text"
    )
  }

  p = p %>% layout(
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent",
    font = list(color = "#c5d0db"),
    polar = list(
      bgcolor = "transparent",
      radialaxis = list(
        visible = TRUE,
        range = c(min_val, 100),
        gridcolor = "#233545",
        color = "#8fa8be"
      ),
      angularaxis = list(
        gridcolor = "#233545",
        color = "#c5d0db"
      )
    ),
    legend = list(font = list(color = "#c5d0db")),
    showlegend = TRUE
  )

  return(p)
}

# Custom CSS
app_css <- "
body {
  background-color: #0f1923;
  color: #e0e6ed;
}

.navbar {
  border-bottom: 2px solid #1abc54 !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.4);
}

.navbar-brand {
  font-weight: 700 !important;
  letter-spacing: 0.5px;
}

.card, .bslib-card {
  background-color: #172633 !important;
  border: 1px solid #233545 !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 16px rgba(0,0,0,0.25);
}

.card-header {
  background-color: #1c3040 !important;
  border-bottom: 1px solid #233545 !important;
  font-weight: 600;
  letter-spacing: 0.3px;
  border-radius: 12px 12px 0 0 !important;
}

.filter-panel {
  background: linear-gradient(135deg, #1c3040 0%, #172633 100%);
  border: 1px solid #233545;
  border-radius: 12px;
  padding: 20px 24px;
  margin-bottom: 20px;
}

.filter-panel label {
  color: #8fa8be !important;
  font-size: 0.82rem;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  font-weight: 600;
  margin-bottom: 6px;
}

.form-control, .selectize-input, .selectize-dropdown {
  background-color: #0f1923 !important;
  border: 1px solid #2d4458 !important;
  color: #e0e6ed !important;
  border-radius: 8px !important;
}

.selectize-input.focus {
  border-color: #1abc54 !important;
  box-shadow: 0 0 0 2px rgba(26,188,84,0.2) !important;
}

.selectize-dropdown-content .option {
  color: #e0e6ed !important;
}

.selectize-dropdown-content .active {
  background-color: #1abc54 !important;
  color: #fff !important;
}

.btn-default, .dropdown-toggle {
  background-color: #0f1923 !important;
  border: 1px solid #2d4458 !important;
  color: #e0e6ed !important;
  border-radius: 8px !important;
}

.bootstrap-select .dropdown-menu {
  background-color: #172633 !important;
  border: 1px solid #2d4458 !important;
}

.bootstrap-select .dropdown-menu li a {
  color: #e0e6ed !important;
}

.bootstrap-select .dropdown-menu li.selected a,
.bootstrap-select .dropdown-menu li a:hover {
  background-color: #1abc54 !important;
  color: #fff !important;
}

.radio-inline, .radio label, .form-check-label {
  color: #c5d0db !important;
}

.btn-check:checked + .btn {
  background-color: #1abc54 !important;
  border-color: #1abc54 !important;
}

.irs--shiny .irs-bar {
  background: #1abc54 !important;
  border-top: 1px solid #1abc54 !important;
  border-bottom: 1px solid #1abc54 !important;
}

.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background-color: #1abc54 !important;
}

.irs--shiny .irs-handle {
  border: 2px solid #1abc54 !important;
  background-color: #172633 !important;
}

.irs--shiny .irs-line {
  background-color: #233545 !important;
  border: none !important;
}

.irs--shiny .irs-grid-text {
  color: #5e7a90 !important;
}

.section-title {
  color: #1abc54;
  font-size: 1.05rem;
  font-weight: 600;
  letter-spacing: 0.3px;
  margin-bottom: 12px;
  padding-bottom: 8px;
  border-bottom: 2px solid #233545;
  display: flex;
  align-items: center;
  gap: 8px;
}

.section-title .icon {
  font-size: 1.1rem;
}

.attr-panel {
  background: linear-gradient(135deg, #1c3040 0%, #172633 100%);
  border: 1px solid #233545;
  border-radius: 12px;
  padding: 24px;
}

.attr-panel h5 {
  color: #1abc54;
  font-weight: 700;
  margin-bottom: 16px;
  padding-bottom: 8px;
  border-bottom: 2px solid #233545;
}

.attr-panel ul {
  list-style: none;
  padding-left: 0;
}

.attr-panel li {
  padding: 8px 0;
  border-bottom: 1px solid rgba(35,53,69,0.6);
  font-size: 0.9rem;
  line-height: 1.5;
  color: #b0c4d8;
}

.attr-panel li:last-child {
  border-bottom: none;
}

.attr-panel li strong {
  color: #e0e6ed;
}

.attr-panel .note {
  margin-top: 14px;
  padding: 10px 14px;
  background-color: rgba(26,188,84,0.08);
  border-left: 3px solid #1abc54;
  border-radius: 0 8px 8px 0;
  font-size: 0.85rem;
  color: #8fa8be;
}

input[type='number'] {
  background-color: #0f1923 !important;
  border: 1px solid #2d4458 !important;
  color: #e0e6ed !important;
  border-radius: 8px !important;
}

input[type='number']:focus {
  border-color: #1abc54 !important;
  box-shadow: 0 0 0 2px rgba(26,188,84,0.2) !important;
}

.tab-content > .tab-pane {
  padding: 24px 8px;
}

.nav-link {
  font-weight: 500;
  letter-spacing: 0.3px;
  transition: color 0.2s;
}

.nav-link:hover {
  color: #1abc54 !important;
}

.modebar {
  background: transparent !important;
}

.well {
  background-color: #172633 !important;
  border: 1px solid #233545 !important;
  border-radius: 12px !important;
}

hr {
  border-color: #233545 !important;
  opacity: 0.6;
}

.bootstrap-select .filter-option-inner-inner {
  color: #e0e6ed !important;
}

.bs-actionsbox .btn-group .btn {
  background-color: #1c3040 !important;
  color: #1abc54 !important;
  border-color: #233545 !important;
}

.selectize-dropdown .optgroup-header {
  background-color: #1c3040 !important;
  color: #1abc54 !important;
  font-weight: 700;
  font-size: 0.8rem;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  padding: 8px 12px !important;
  border-top: 1px solid #233545;
}

.selectize-dropdown .optgroup:first-child .optgroup-header {
  border-top: none;
}

.story-intro {
  background: linear-gradient(135deg, #14202c 0%, #172633 100%);
  border-left: 4px solid #1abc54;
  padding: 16px 20px;
  margin-bottom: 20px;
  border-radius: 10px;
  color: #c5d0db;
  font-size: 0.95rem;
  line-height: 1.6;
  box-shadow: 0 3px 10px rgba(0,0,0,0.2);
}

.story-intro strong {
  color: #1abc54;
  font-weight: 700;
}
"

app_theme <- bs_theme(
  version = 5,
  bg = "#0f1923",
  fg = "#e0e6ed",
  primary = "#1abc54",
  secondary = "#233545",
  success = "#1abc54",
  info = "#48a4e3",
  warning = "#de8e08",
  danger = "#e74c3c",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.95,
  "navbar-bg" = "#14202c"
)

ui <- page_navbar(
  title = tags$span(
    tags$strong("European Football Analytics")
  ),
  theme = app_theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css))),

  nav_panel(
    title = "Player Improvement",
    icon = icon("chart-line"),
    div(class = "story-intro",
        HTML("<strong>Player Improvement Analysis:</strong>
         This section helps identify young players with strong growth potential
         by analysing age, overall ratings, positional roles, and performance
         improvement over time. Users can interactively compare player development trajectories.")
    ),
    
    div(class = "filter-panel",
      fluidRow(
        column(4, sliderTextInput(
          inputId = "improv_age_rng", label = "Age Range",
          choices = ages, selected = c(min_age, max_age), grid = TRUE
        )),
        column(4, selectInput("improv_league", "League",
                              choices = c("All", League$name),
                              selected = "All")),
        column(4, numericInput("improv_min", "Min. Rating Increase", value = 5, min = -100, max = 100))
      )
    ),
    fluidRow(
      column(7,
        card(
          card_header(class = "section-title", "Player Ratings Overview"),
          card_body(plotlyOutput("improv_improvement_plot", height = "500px"))
        )
      ),
      column(5,
        card(
          card_header(class = "section-title", "Improvement Over Time"),
          card_body(
            tags$p(class = "text-muted", style = "font-size:0.85rem; margin-bottom:10px;",
                   "Use the Lasso or Box Select tool on the left chart to select players."),
            plotlyOutput("improv_line_plot", height = "460px")
          )
        )
      )
    )
  ),

  nav_panel(
    title = "League Performance",
    icon = icon("trophy"),
    
    div(class = "story-intro",
        HTML("<strong>League Performance Analysis:</strong>
         This section compares team competitiveness across leagues and seasons
         using win, draw, and loss percentages. Users can also evaluate how teams
         perform against specific opponents under different match conditions.")
    ),
    
    div(class = "filter-panel",
      fluidRow(
        column(3, selectInput("perf_league", "League", League$name)),
        column(3, selectInput("perf_against_team", "Compare Against", choices = NULL)),
        column(3, sliderTextInput(
          inputId = "perf_season_rng", label = "Season Range",
          choices = all_seasons, selected = c("2008/2009", "2015/2016"), grid = TRUE
        )),
        column(3, radioButtons("perf_side", "Match Side",
                               choices = c("Both", "Home", "Away"), selected = "Both", inline = TRUE))
      ),
      fluidRow(
        column(12, pickerInput(
          inputId = "perf_teams", label = "Filter Teams",
          choices = NULL, multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ))
      )
    ),
    fluidRow(
      column(6,
        card(
          card_header(class = "section-title", "Overall Performance in League"),
          card_body(plotlyOutput("perf_plot_overall", height = "500px"))
        )
      ),
      column(6,
        card(
          card_header(class = "section-title", textOutput("perf_against_title")),
          card_body(plotlyOutput("perf_plot_against", height = "500px"))
        )
      )
    )
  ),

  nav_panel(
    title = "Team Statistics",
    icon = icon("futbol"),
    
    div(class = "story-intro",
        HTML("<strong>Team Statistics Analysis:</strong>
         This section evaluates possession dominance, attacking efficiency,
         and defensive performance across teams. The visualisation helps identify
         balanced teams and tactical play styles.")
    ),
    
    div(class = "filter-panel",
      fluidRow(
        column(4, selectInput("stats_league", "League", League$name)),
        column(4, sliderTextInput(
          inputId = "stats_season_rng", label = "Season Range",
          choices = all_seasons, selected = c("2008/2009", "2015/2016"), grid = TRUE
        )),
        column(4, radioButtons("stats_side", "Match Side",
                               choices = c("Both", "Home", "Away"), selected = "Both", inline = TRUE))
      ),
      fluidRow(
        column(12, pickerInput(
          inputId = "stats_teams", label = "Filter Teams",
          choices = NULL, multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ))
      )
    ),
    fluidRow(
      column(8, offset = 2,
        card(
          card_header(class = "section-title", "Average Possession, Goals Scored & Conceded"),
          card_body(girafeOutput("stats_plot", height = "550px"))
        )
      )
    )
  ),

  nav_panel(
    title = "Player Comparison",
    icon = icon("users"),
    
    div(class = "story-intro",
        HTML("<strong>Player Attribute Comparison:</strong>
         This section enables direct comparison of football players using
         FIFA performance attributes such as passing, stamina, crossing,
         and ball control. The radar chart highlights player strengths and weaknesses.")
    ),
    
    div(class = "filter-panel",
      fluidRow(
        column(4, selectizeInput("radar_player_A", "Player A",
                                 choices = NULL,
                                 options = list(placeholder = 'Type to search...'))),
        column(4, selectizeInput("radar_player_B", "Player B",
                                 choices = NULL,
                                 options = list(placeholder = 'Type to search...'))),
        column(4, selectizeInput("radar_player_C", "Player C",
                                 choices = NULL,
                                 options = list(placeholder = 'Type to search...')))
      )
    ),
    fluidRow(
      column(8,
        card(
          card_header(class = "section-title", "Skill Radar Comparison"),
          card_body(plotlyOutput("radar_comparison_plot", height = "500px"))
        )
      ),
      column(4,
        div(class = "attr-panel",
          tags$h5("Attribute Definitions"),
          tags$ul(
            tags$li(tags$strong("Short Passing:"), " Accuracy and speed of passes over short distances."),
            tags$li(tags$strong("Long Passing:"), " Accuracy and speed of passes over long distances."),
            tags$li(tags$strong("Stamina:"), " The rate at which a player tires during a match."),
            tags$li(tags$strong("Crossing:"), " Accuracy of balls played from areas outside the box into the box."),
            tags$li(tags$strong("Ball Control:"), " Ability to keep the ball under control when pressured.")
          ),
          div(class = "note", "Values are based on FIFA attributes (0–100 scale).")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Player Improvement tab
  improv_age <- reactive({
    req(input$improv_age_rng)
    start_idx = which(ages == input$improv_age_rng[1])
    end_idx = which(ages == input$improv_age_rng[2])
    ages[start_idx:end_idx]
  })
  
  improv_filtered_teams <- reactive({
    if (input$improv_league == "All") {
      return("all")
    } else {
      return(teams_in_league(input$improv_league, all_seasons))
    }
  })
  
  
  selected_player_ids <- reactive({
    ed = event_data("plotly_selected", source = "scatter")
    
    if (is.null(ed)) return(NULL)
    
    return(ed$key) 
  })
  
  output$improv_improvement_plot = renderPlotly({
    req(input$improv_min, input$improv_age_rng)
    
    p <- improvement_plot(
      teams = improv_filtered_teams(),
      age_range = improv_age(),
      min_improvement = input$improv_min
    )
    
    p %>% event_register("plotly_selected")
  })
  
  output$improv_line_plot = renderPlotly({
    pids = selected_player_ids()
    
    if (is.null(pids)) {
      return(
        plot_ly(type = "scatter", mode = "markers") %>%
          layout(
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              text = "Use the Lasso or Box Select tool<br>to select multiple players",
              showarrow = FALSE, x = 0.5, y = 0.5, xref = 'paper', yref = 'paper',
              font = list(color = "#5e7a90", size = 14)
            )
          )
      )
    }
    improvement_line_plot(pids)
  })
  
  # League Performance tab
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

  # Team Statistics tab
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
  
  # Radar chart tab
  grouped_players <- Player_Data %>%
    distinct(player_name_and_team, league_name) %>%
    arrange(player_name_and_team) %>%
    split(.$league_name) %>%
    lapply(function(df) sort(df$player_name_and_team))
  grouped_players <- grouped_players[sort(names(grouped_players))]

  radar_defaults <- c("Lionel Messi (BAR)", "Cristiano Ronaldo (REA)", "Neymar (BAR)")
  radar_ids <- c("radar_player_A", "radar_player_B", "radar_player_C")
  for (i in seq_along(radar_ids)) {
    updateSelectizeInput(session, radar_ids[i], selected = radar_defaults[i],
      choices = grouped_players, options = list(placeholder = 'Type to search...'))
  }
  
  output$radar_comparison_plot = renderPlotly({
    selected = c(input$radar_player_A, input$radar_player_B, input$radar_player_C)
    
    selected = selected[selected != "" & !is.na(selected)]
    
    req(length(selected) > 0)
    
    pids <- Player_Data %>%
      filter(player_name_and_team %in% selected) %>%
      pull(player_id)
    
    radar_plot(pids)
  })
}

shinyApp(ui = ui, server = server)
