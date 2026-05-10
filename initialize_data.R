library(tidyverse)
library(lubridate)

League = read.csv("datasets/League.csv")
Team = read.csv("datasets/Team.csv")
Match = read.csv("datasets/Match.csv")
Match$date = as.Date(Match$date, format = "%d/%m/%Y %H:%M")
Possession = read.csv("datasets/Match_Possesion.csv")
PositionReference = read.csv("datasets/PositionReference.csv")
Player = read.csv("datasets/Player.csv")
Player$birthday = as.Date(Player$birthday, format = "%Y-%m-%d %H:%M")
Player_Attributes = read.csv("datasets/Player_Attributes.csv")
Player_Attributes$date = as.Date(Player_Attributes$date, format = "%Y-%m-%d %H:%M")

# Every player get assigned a rating at the beginning even if they weren't playing
#So we remove these attributes
Player_Attributes = Player_Attributes %>% filter(date !=min(Player_Attributes$date)) 
team_lookup = Team %>% select(team_api_id, team_long_name, team_short_name)
league_lookup = League %>% select(country_id, name)
player_lookup = Player %>% select("player_api_id","player_name", "birthday")

Final_Possession = Possession %>% filter(elapsed == 90)
Final_Possession = Final_Possession %>% 
  group_by(match_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

Full_Match = Match %>%
  left_join(team_lookup, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name, home_team_name_short = team_short_name) %>%
  left_join(team_lookup, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name, away_team_name_short = team_short_name) %>%
  left_join(league_lookup, by = c("league_id" = "country_id")) %>%
  rename(league_name = name)

Full_Match = left_join(Full_Match, Final_Possession, by = c("id" = "match_id"))


all_seasons = c("2008/2009", "2009/2010", "2010/2011", "2011/2012",
                "2012/2013", "2013/2014", "2014/2015", "2015/2016")
##### Getting data regarding improvement #####
player_yearly_ratings <- Player_Attributes %>%
  # 1. Ensure date is in Date format and extract the year
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  # 2. Group by player AND year
  group_by(player_api_id, year) %>%
  # 3. Pick the latest update for that specific player in that specific year
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  # 4. Ungroup and select relevant columns
  ungroup() %>%
  select(player_api_id, year, date, overall_rating, potential)



##### Getting full player data #####
current_players_in_match = Full_Match %>% filter(season=="2015/2016") %>%
  # 1. Pivot the ID, X, and Y columns into a long format
  pivot_longer(
    cols = matches("(home|away)_player_(X|Y)?[0-9]+"),
    names_to = c("side", "type", "index"),
    # This regex handles the inconsistent 'X1' vs '1' naming
    names_pattern = "(home|away)_player_(X|Y)?([0-9]+)"
  ) %>%
  # 2. Label the ID columns (which have an empty 'type') as "player_id"
  mutate(type = case_when(
    type == "X" ~ "X",
    type == "Y" ~ "Y",
    TRUE ~ "player_id"
  )) %>%
  # 3. Spread X, Y, and player_id into their own columns
  pivot_wider(names_from = type, values_from = value) %>%
  # 4. Create the team_id and team_name columns based on the 'side'
  mutate(
    team_id = if_else(side == "home", home_team_api_id, away_team_api_id),
    team_name = if_else(side == "home", home_team_name, away_team_name),
    team_name_short = if_else(side == "home", home_team_name_short, away_team_name_short)
    
  ) %>%
  # 5. Clean up: keep only the columns you asked for
  select(player_id, league_name, team_id, team_name, team_name_short, date, side, X, Y) %>%
  filter(!is.na(player_id)) # Remove empty slots

last_player_match = current_players_in_match %>%
  group_by(player_id) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

Player_Data = left_join(last_player_match,player_lookup, by=c("player_id"="player_api_id")) %>%
  select(player_id, player_name, birthday, league_name, team_id, team_name, team_name_short, X, Y) %>%
  mutate(player_name_and_team = paste(Player_Data$player_name, " (",Player_Data$team_name_short,")",sep=""),
         position = case_when(
    Y == 1 ~ "Goalkeeper",
    Y >= 2 & Y <= 4 ~ "Defender",
    Y >= 5 & Y <= 8 ~ "Midfielder",
    Y >= 9 ~ "Attacker",
    TRUE ~ "Unknown"  # Safety net for unexpected data
  ))
Player_Data$position = factor(Player_Data$position, levels=c("Attacker","Midfielder","Defender","Goalkeeper"))
Player_Data = Player_Data %>%
  mutate(age = floor(as.numeric(difftime(as.Date("2016-12-31"), birthday, units = "weeks")) / 52.1775)) %>%
  select(-birthday)

latest_attributes <- Player_Attributes %>%
  # Ensure the date column is in the correct Date format
  mutate(date = as.Date(date)) %>%
  group_by(player_api_id) %>%
  # Select the row with the most recent date
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

Player_Data = Player_Data %>%
  left_join(latest_attributes, by = c("player_id" = "player_api_id")) %>%
  select(-c(id, date, X, Y))


##### Adding player image urls #####
Player_Data = Player_Data %>%
  mutate(
    # 1. Ensure ID is a 6-character string with leading zeros
    padded_id = str_pad(player_fifa_api_id, width = 6, pad = "0"),
    
    # 2. Extract the two parts of the ID
    id_part1 = str_sub(padded_id, 1, 3),
    id_part2 = str_sub(padded_id, 4, 6),
    
    # 3. Construct the URL (using 16_120.png as requested)
    player_img_url = paste0(
      "https://cdn.sofifa.net/players/", 
      id_part1, "/", id_part2, "/16_120.png"
    )
  ) %>% select(-c(id_part1, id_part2, padded_id))

# 1. Start with your yearly ratings dataframe
player_improvement <- player_yearly_ratings %>%
  # 1. Group by player and ensure they are in chronological order
  group_by(player_api_id) %>%
  arrange(year, .by_group = TRUE) %>%
  
  # 2. Calculate the difference (Current Year - Previous Year)
  mutate(
    yearly_improvement = overall_rating - lag(overall_rating)
  ) %>%
  
  # 3. Instead of filtering for 2016, grab the latest year available for each player
  slice_max(order_by = year, n = 1) %>% 
  
  # 4. Optional: Handle cases where a player only has 1 year of data
  # If lag() is NA, it means they are a "New Entry" (improvement = 0 or NA)
  mutate(yearly_improvement = coalesce(yearly_improvement, 0)) %>%
  
  rename(improvement_vs_2015 = yearly_improvement) %>%
  ungroup()

# 5. Join this back to your Player_Data
Player_Data = Player_Data %>%
  left_join(
    select(player_improvement, player_api_id, improvement_vs_2015), 
    by = c("player_id" = "player_api_id")
  )



save(League, Full_Match, Player_Data, Player_Attributes, all_seasons, file="data.RData")

