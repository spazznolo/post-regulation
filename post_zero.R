

source('libraries.R')
source('functions.R')
source('data.R')

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
  )

ggplot(data = team_ot_summary, aes(x=points, y=ot)) +
  geom_point(col = single_color, alpha = 0.7) +
  labs(x = '\nStanding Points Per Game', y = 'OT Games\n') +
  scale_y_continuous(labels = percent) +
  dark_theme()

ggsave(filename = 'post-regulation-zero-one.png', path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(ot ~ points_2, team_ot_summary %>% filter(points > 0.6) %>% mutate(points_2 = (points - 1)))) 

