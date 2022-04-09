

# insert download directory here
download_dir <- '~/Downloads/'

# download moneypuck data
temp <- tempfile()
download.file("https://peter-tanner.com/moneypuck/downloads/shots_2007-2020.zip", temp)
unzip(temp, exdir = download_dir)

# create analysis set
shots <- 
  read_csv(paste0(download_dir, "shots_2007-2020.csv")) %>%
  clean_names %>%
  mutate(is_ot = ifelse(time > 3600, 1, 0)) %>%
  filter(season >= 2015, is_playoff_game == 0, (is_ot == 1 | (home_skaters_on_ice == 5 & away_skaters_on_ice == 5))) %>%
  select(season, game_id, time, is_ot, home_team_won, home_team_code, away_team_code, team, goal, x_goal)

