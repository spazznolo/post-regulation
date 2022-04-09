

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


chunjs <-
  shots %>%
  filter(goal == 0) %>%
  arrange(season, game_id, time) %>%
  mutate(game_chunk =  cut(time, 
                          breaks = seq(0, 3900, 300),
                          include.lowest = TRUE)
         ) %>%
  group_by(season, game_id, game_chunk) %>%
  summarize(home_xg_pct = 100*(sum(x_goal*(team == 'HOME'))/sum(x_goal)) - 50, .groups = 'drop') %>%
  group_by(season, game_id) %>%
  mutate(home_xg_lead = lead(home_xg_pct)) %>%
  ungroup

summary(lm(home_xg_lead ~ home_xg_pct, data = chunjs))


chunjs %>%
  ggplot +
  geom_point(aes(x = home_xg_pct, y = home_xg_lead), alpha = 0.01)


overtime_shots <-
  shots %>%
  filter(is_ot == 1) %>%
  ungroup 

overtime_summary <-
  overtime_shots %>%
  group_by(season, game_id, is_ot, team, home_team_won) %>%
  summarize(x_goals = sum(x_goal)) %>%
  group_by(season, game_id, is_ot) %>%
  mutate(x_goals_pct = 100*x_goals/sum(x_goals)) %>%
  ungroup

form <-
  overtime_summary %>%
  pivot_wider(id_cols = c(season, game_id, team), names_from = is_ot, values_from = x_goals_pct) %>%
  filter(team == 'HOME', `1` < 100)

form %>%
  ggplot +
  geom_point(aes(x = `0`, y = `1`), col = 'yellow', alpha = 0.7) +
  labs(x = '\nRegulation', y = 'Overtime\n') +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 10, y = 10, col = 'white',
           label = paste('r = ',round(cor(form$`0`, form$`1`),3),sep=' '), size = 3)

summary(lm(`1` ~ `0`, data = form))

# this means one of two things: 
# either the overtime is setup in a way which induces near complete randomness in outcomes
# or the overtime is sufficiently different to regulation that it rewards different skill sets and strategies
# to the point where it is nearly a different sport.

summary(glm(home_team_won ~ x_goals_pct + reg_xg, family = 'binomial', 
            data = overtime_summary %>% 
              filter(team == 'HOME') %>%
              mutate(x_goals_pct = x_goals_pct*100, reg_xg = lag(x_goals_pct)) %>% 
              filter(is_ot == 1)))

