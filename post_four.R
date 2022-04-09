
source('libraries.R')
source('functions.R')
source('data.R')

tibble(
    pos = c(rep(1,1000), rep(2,1000), rep(3,1000),rep(1,1000), rep(2,1000), rep(3,1000)),
    game = c(rep(1:1000, 3), rep(1:1000, 3)),
    team = c(rep('A', 3000), rep('B', 3000)),
    goal = c(rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), 
             rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.6))) %>% 
  arrange(game, pos, team) %>%
  group_by(game, team) %>%
  mutate(goals = cumsum(goal)) %>%
  ungroup %>%
  mutate(
    result = case_when(
      goals > lag(goals) + (3 - pos) & team == 'B'  ~ 1,
      pos == 3 & goals > lag(goals) & team == 'B' ~ 1,
      pos == 3 & goals == lag(goals) & team == 'B'  ~ 0.5,
      TRUE ~ 0)
  ) %>%
  count(game, wt = result) %>%
  mutate(result = ifelse(n > 1, 1, n)) %>%
  summarize(win_pct = mean(result)) 








tibble(
  pos = c(rep(1,1000), rep(2,1000), rep(3,1000),rep(1,1000), rep(2,1000), rep(3,1000)),
  game = c(rep(1:1000, 3), rep(1:1000, 3)),
  team = c(rep('A', 3000), rep('B', 3000)),
  goal = c(rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), 
           rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.5), rbinom(1000, 1, 0.6))) %>% 
  arrange(game, pos, team) %>%
  group_by(game, team) %>%
  mutate(goals = cumsum(goal)) %>%
  ungroup %>%
  group_by(game) %>%
  summarize(
    result = case_when(
      pos == 3 & team == 'B' & lag(goals, 1) > (lag(goals, 2) + 1) ~ 0,
      pos == 3 & team == 'B' & goals > lag(goals) ~ 1,
      pos == 3 & team == 'B' & goals == lag(goals) ~ 0.5,
      TRUE ~ 0),
    .groups = 'drop'
  ) %>%
  summarize(win_pct = sum(result)/1000) 

outcomes_shoot <- c((rbinom(10000, 1, 0.5) + rbinom(10000, 2, 0.5)) - c(rbinom(10000, 3, 0.3)))

(sum(outcomes_shoot > 0) + (0.5*sum(outcomes_shoot == 0)))/10000

outcomes_even <- c(c(rbinom(10000, 3, 0.3)) - c(rbinom(10000, 3, 0.3)))

(sum(outcomes_even > 0) + (0.5*sum(outcomes_even == 0)))/10000
