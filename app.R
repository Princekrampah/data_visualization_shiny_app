library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(ggpath)
library(ggimage)
library(ggiraph)


# ── Data Loading ──
load("data.RData")
min_age = min(Player_Data$age,na.rm=T)
max_age = max(Player_Data$age,na.rm=T)
ages = min_age:max_age
# ── Helper Functions ──
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
    # Map 'has_possession' to the color aesthetic (the border)
    geom_point_interactive(
      aes(size = pos_visual, 
          tooltip = tooltip, 
          data_id = team,
          color = has_possession), # Mapping color here
      fill = "white", 
      shape = 21, 
      stroke = 2.5 # Increased thickness to make the red/black stand out
    ) +
    geom_from_path(aes(path = logo_path, width = pos_visual / 1500)) +
    geom_abline(slope = 1, linetype = "dashed", alpha = 0.4) +
    theme_bw() +
    # Define the colors: Black for valid data, Red for "No Data"
    scale_color_manual(values = c("Has Data" = "black", "No Data" = "red")) +
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
    geom_jitter(alpha=0.7) + theme_bw() +
    labs(col="Position", x="Age", y="Overall rating", shape="",size="") +
    scale_color_manual(values= c("Attacker" ="#ece134",
                                 "Midfielder" = "#de8e08",
                                 "Defender" ="#138f60",
                                 "Goalkeeper"="#48a4e3")) +
    guides(size = guide_legend(override.aes = list(shape = 16)))
  return(ggplotly(p,tooltip = "text", source = "scatter"))
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
    theme_bw() + 
    labs(col = "Player", x="Date", y="Overall rating")
  
  return(ggplotly(p, tooltip = "text"))
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
  
  plot_data_long = plot_data %>%
    pivot_longer(cols = -c(player_name, team_name), 
                 names_to = "attribute", 
                 values_to = "value")
  
  min_val = min(50,round(min(plot_data_long$value) / 10)*10)
  
  p = plot_ly(type = "scatterpolar")
  
  colors = c("#ece134", "#de8e08", "#138f60")
  players = unique(plot_data_long$player_name)
  
  for(i in 1:length(players)) {
    player_subset = plot_data_long %>% filter(player_name == players[i])
    
    player_subset = rbind(player_subset, player_subset[1,])
    
    p = p %>% add_trace(
      r = player_subset$value,
      theta = player_subset$attribute,
      name = players[i],
      line = list(color = colors[i]),
      marker = list(color = colors[i]),
      text = paste0("Player: ", player_subset$player_name, 
                    "\nTeam: ", player_subset$team_name, 
                    "\nAttribute: ", player_subset$attribute,
                    "\nValue: ", player_subset$value),
      hoverinfo = "text"
    )
  }
  
  p = p %>% layout(
    polar = list(radialaxis = list(visible = T,range = c(min_val, 100))),
    showlegend = TRUE)
  
  return(p)
}

# ── UI ──
ui <- navbarPage(
  title = "European Football Analytics",
  theme = NULL,
  tabPanel("Player Improvement",
           fluidRow(
             column(3, sliderTextInput(
               inputId = "improv_age_rng", label = "Select Age Range",
               choices = ages, selected = c(min_age, max_age), grid = TRUE)
               ),
             column(3, selectInput("improv_league", "Select League", 
                                   choices = c("All", League$name), 
                                   selected = "All")),
             column(3, numericInput("improv_min", "Minimum Rating Increase", value = 5, min = -100, max = 100))
             ),
           hr(),
           fluidRow(
             column(7, 
                    h4("Player Ratings Overview"),
                    plotlyOutput("improv_improvement_plot")
             ),
             column(5, 
                    h4("Improvement over time (Select players)"),
                    plotlyOutput("improv_line_plot")
             )
           )
           ),
  
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
      column(6, offset=3,
        h4("Average Possession, Goals Scored & Conceded"),
        girafeOutput("stats_plot")
      )
    )
  ),
  tabPanel("Player Comparison Radar",
           fluidRow(
             column(4, selectizeInput("radar_player_A", "Search Player A", 
                                      choices = NULL, 
                                      options = list(placeholder = 'Type to search...'))),
             column(4, selectizeInput("radar_player_B", "Search Player B", 
                                      choices = NULL, 
                                      options = list(placeholder = 'Type to search...'))),
             column(4, selectizeInput("radar_player_C", "Search Player C", 
                                      choices = NULL, 
                                      options = list(placeholder = 'Type to search...')))
           ),
           hr(),
           plotlyOutput("radar_comparison_plot")
  )
)

# ── Server ──
server <- function(input, output, session) {
  # ── Player Improvement tab ──
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
          layout(annotations = list(
            text = "Use the Lasso or Box Select tool<br>to select multiple players", 
            showarrow = FALSE, x = 0.5, y = 0.5, xref='paper', yref='paper'
          ))
      )
    }
    improvement_line_plot(pids)
  })
  
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
  
  # ── Radar chart tab ──
  player_choices <- sort(unique(Player_Data$player_name_and_team))
  
  updateSelectizeInput(session, "radar_player_A",selected = "", choices = player_choices, server = TRUE)
  updateSelectizeInput(session, "radar_player_B",selected = "", choices = player_choices, server = TRUE)
  updateSelectizeInput(session, "radar_player_C",selected = "", choices = player_choices, server = TRUE)
  
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
