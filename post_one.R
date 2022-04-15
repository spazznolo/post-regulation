

team_ot_points <-
  all_shots %>%
  filter(is_ot == 1) %>%
  mutate(team = ifelse(team == 'HOME', home_team_code, away_team_code)) %>%
  count(season, game_id, team, wt = goal) %>%
  group_by(season, game_id) %>%
  mutate(points = case_when(
    min(n) == max(n) ~ 0.5,
    n == max(n) ~ 1, 
    TRUE ~ 0)) %>%
  ungroup %>%
  group_by(season, team) %>%
  summarize(
    points_pct = sum(points)/n(),
    .groups = 'drop'
  )

team_ot_summary <-
  all_shots %>%
  filter(is_ot == 0) %>%
  mutate(team = ifelse(team == 'HOME', home_team_code, away_team_code)) %>%
  count(season, game_id, team, wt = goal) %>%
  group_by(season, game_id) %>%
  mutate(points = case_when(
    min(n) == max(n) ~ 1,
    n == max(n) ~ 2, 
    TRUE ~ 0)) %>%
  ungroup %>%
  group_by(season, team) %>%
  summarize(
    ot = mean(points == 1),
    points = sum(points)/n(),
    .groups = 'drop'
  ) %>%
  left_join(team_ot_points, by = c('season', 'team'))

team_ot_summary %>%
  ggplot +
  geom_point(aes(x = points, y = points_pct), col = single_color, alpha = 0.7) +
  labs(x = '\nPre-OT Standing Points Per Game', y = 'Standing Points Per OT\n') +
  dark_theme() +
  annotate("text", x = 0.55, y = 0.3, col = 'white',
           label = paste('r = ',round(cor(team_ot_summary$points, team_ot_summary$points_pct),3),sep=' '), size = 3)

ggsave(filename = 'post-regulation-one-onee.png', path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(points_pct ~ points, data = team_ot_summary %>% mutate(points = points - 1)))


# home team has about 51.6% of xGF%, all else equal
# for every 10% increase in xGF% in regulation, there's about a 2% increase in xGF% in overtime

reg_to_ot <-
  shots %>%
  group_by(season, game_id, is_ot, team, home_team_won) %>%
  summarize(x_goals = sum(x_goal), shots = n()) %>%
  group_by(season, game_id, is_ot) %>%
  mutate(x_goals_pct = (100*x_goals/sum(x_goals)) - 50, shots = sum(shots)) %>%
  ungroup %>%
  pivot_wider(id_cols = c(season, game_id, team), names_from = is_ot, values_from = x_goals_pct) %>%
  filter(team == 'HOME', `1` < 50)

reg_to_ot %>%
  ggplot +
  geom_point(aes(x = `0`, y = `1`), col = single_color, alpha = 0.7) +
  labs(x = '\nRegulation', y = 'Overtime\n') +
  scale_x_continuous(limits = c(-40, 40)) +
  scale_y_continuous(limits = c(-50, 50)) +
  dark_theme() +
  annotate("text", x = -35, y = -40, col = 'white',
           label = paste('r = ',round(cor(form$`0`, form$`1`),3),sep=' '), size = 3)

summary(lm(`1` ~ `0`, data = reg_to_ot))

ggsave(filename = 'post-regulation-one-twoo.png', path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)


# 5 minute chunks are not very predictive
chunks <-
  shots %>%
  #filter(goal == 0) %>%
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

summary(lm(home_xg_lead ~ home_xg_pct, data = chunks))


# this means one of two things: 
# either the overtime is setup in a way which induces near complete random in outcomes
# or the overtime is sufficiently different to regulation that it rewards different skill sets and strategies
# to the point where it is nearly a different sport.

# 5 minute chunks are not very predictive
chunks <-
  shots %>%
  filter(is_ot == 1) %>%
  arrange(season, game_id, time) %>%
  mutate(x_goal = ifelse(goal == 1, 0, x_goal*100)) %>%
  add_count(season, game_id, wt = goal) %>%
  group_by(season, game_id, team) %>%
  summarize(
    goals = mean(n), 
    goal = sum(goal), 
    x_goal = sum(x_goal), 
    .groups = 'drop') %>%
  group_by(season, game_id) %>%
  mutate(
    xg_pct = 100*((x_goal/sum(x_goal)) - 0.5),
    pts = case_when(
      goal > 0 ~ 100,
      goal == 0 & goals == 0 ~ 50,
      TRUE ~ 0)
    ) %>%
  filter(team == 'HOME')

summary(lm(pts ~ xg_pct, chunks))


chunks %>%
  ungroup %>%
  drop_na %>%
  arrange(xg_pct) %>%
  mutate(
    n_row = 1:n(),
    xg_chunk =  cut(n_row, breaks = seq(0, n(), 10), include.lowest = TRUE)
  ) %>%
  group_by(xg_chunk) %>%
  summarize(
    pts_pct = mean(pts),
    xg_pctle = 100*mean(n_row)/1362
    ) %>%
  ungroup %>%
  ggplot +
  geom_point(aes(xg_pctle, pts_pct), col = single_color) +
  labs(x = '\nxGF% Percentile', y = 'OT Points %\n') +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  dark_theme()




team_summary <-
  all_shots %>%
  filter(is_ot == 0) %>%
  mutate(team = ifelse(team == 'HOME', home_team_code, away_team_code)) %>%
  count(season, game_id, team, wt = goal) %>%
  group_by(season, game_id) %>%
  mutate(points = case_when(
    min(n) == max(n) ~ 1,
    n == max(n) ~ 2, 
    TRUE ~ 0)) %>%
  ungroup %>%
  group_by(season, team) %>%
  summarize(
    ot = sum(points == 1),
    points = sum(points),
    .groups = 'drop'
  ) %>%
  mutate(points_orig = points + (ot/2))

ok <- replicate(1000, team_summary$points + rbinom(184, team_summary$ot, 0.5))



