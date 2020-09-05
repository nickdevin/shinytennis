library(dplyr)
library(tidyr)
library(ggplot2)

matchstats = read.csv(file = './charting-m-stats-Overview.csv')

matchstats = matchstats %>% 
  separate(., col = match_id,
           into = c('match_date',
                    'mens',
                    'tourney_name',
                    'round',
                    'player_1',
                    'player_2'),
           sep = '-') %>% 
  filter(., set != 'Total')

matchstats$player_1 =
  sapply(matchstats$player_1, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$player_2 =
  sapply(matchstats$player_2, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name =
  sapply(matchstats$tourney_name, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name = 
  sapply(matchstats$tourney_name, tolower)

matchstats$year = sapply(matchstats$match_date, function(x) {
  substr(x, 1, 4)
})

matchstats = matchstats %>% 
  select(., year, tourney_name, player_1, player_2, player, winners, unforced, set)

player1stats = matchstats %>% 
  filter(., player == 1) %>% 
  select(., year,
         tourney_name,
         player_1,
         player_2,
         player_1_winners = winners,
         player_1_unforced = unforced,
         set)

player2stats = matchstats %>% 
  filter(., player == 2) %>% 
  select(., year,
         tourney_name,
         player_1,
         player_2,
         player_2_winners = winners,
         player_2_unforced = unforced,
         set)

playerstats =
  inner_join(player1stats, player2stats, by = c('year',
                                                'tourney_name',
                                                'player_1',
                                                'player_2',
                                                'set'))

playerstats$set = as.numeric(playerstats$set)

#----------------------------

  
DF = read.csv(file = paste('./atp_matches_2019.csv', sep = ''),
              header = TRUE)
DF$year = sapply(DF$tourney_date, function(x) {
  substr(x, 1, 4)
})

DF = mutate(DF, tourney_name = tolower(tourney_name))

DF2 = DF %>%
  separate(
    .,
    score,
    sep = ' ',
    into = c('set1', 'set2', 'set3', 'set4', 'set5', 'set6'),
    fill = 'right'
  )

DF_set1 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set1) %>%
  mutate(., set = 1)

DF_set2 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set2) %>%
  mutate(., set = 2)

DF_set3 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set3) %>%
  mutate(., set = 3)

DF_set4 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set4) %>%
  mutate(., set = 4)

DF_set5 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set5) %>%
  mutate(., set = 5)

DF_set6 = DF2 %>%
  select(., year,
         tourney_name,
         winner_name,
         loser_name,
         setscore = set6) %>%
  mutate(., set = 6)

DF_sets = bind_rows(DF_set1, DF_set2, DF_set3, DF_set4, DF_set5, DF_set6) %>%
  filter(
    .,!grepl("[a-z]|]|\\[", setscore, ignore.case = TRUE),
    setscore != '',!is.na(setscore)
  ) %>%
  mutate(., setscore = sub(
    pattern = '\\(.*\\)',
    replacement = '',
    x = setscore)) %>%
  separate(.,
           setscore,
           into = c('games1', 'games2'),
           sep = '-') %>%
  mutate(
    .,
    games1 = as.numeric(games1),
    games2 = as.numeric(games2),
    winner_name2 = ifelse(games1 > games2,
                          yes = winner_name,
                          no = loser_name),
    loser_name2 = ifelse(games1 < games2,
                         yes = winner_name,
                         no = loser_name)
  ) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name = winner_name2,
    loser_name = loser_name2,
    games1,
    games2,
    set
  )

DF_sets

master


playerstats
placeholder1 = inner_join(
  DF_sets,
  playerstats,
  by = c(
    'year',
    'tourney_name',
    'winner_name' = 'player_1',
    'loser_name' = 'player_2',
    'set'
  )
) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name,
    loser_name,
    winner_winners = player_1_winners,
    winner_unforced = player_1_unforced,
    loser_winners = player_2_winners,
    loser_unforced = player_2_unforced,
    games1,
    games2
  )

placeholder2 = inner_join(
  DF_sets,
  playerstats,
  by = c(
    'year',
    'tourney_name',
    'winner_name' = 'player_2',
    'loser_name' = 'player_1',
    'set'
  )
) %>%
  select(
    .,
    year,
    tourney_name,
    winner_name,
    loser_name,
    winner_winners = player_2_winners,
    winner_unforced = player_2_unforced,
    loser_winners = player_1_winners,
    loser_unforced = player_1_unforced,
    games1,
    games2
  )

placeholder1

master = bind_rows(placeholder1, placeholder2) %>% 
  mutate(., winner_games = pmax(games1, games2), 
         loser_games = pmin(games1, games2)
  ) %>% 
  select(., -games1, -games2)

master

master_W

master_W = master %>%
  select(
    .,
    name = winner_name,
    winners = winner_winners,
    unforced = winner_unforced,
    games_won = winner_games,
    games_lost = loser_games
  )

master_allsets
  
master_L = master %>% 
  select(
    .,
    name = loser_name,
    winners = loser_winners,
    unforced = loser_unforced,
    games_won = loser_games,
    games_lost = winner_games
  )

master_allsets = bind_rows(master_W, master_L)
master_allsets

# means = master_allsets %>%
#   summarise(
#     .,
#     mean_per_game = sum(winners+unforced) / (sum(games1 + games2)),
#     winners_to_unforced_ratio = sum(winners) / sum(unforced)
#   )
# 
# #mean winners/unforced per game overall among all players:
# mwpg = means[1, 1]
# wtur = means[1, 2]

master_means = master_allsets %>%
  group_by(., name) %>%
  summarise(
    .,
    mean_per_game = sum(winners+unforced) / sum(games_won + games_lost),
    winners_to_unforced_ratio = sum(winners) / (sum(unforced))
  )

master_allsets


master_means




master_means %>% 
  summarise(.,
            median(winners_to_unforced_ratio),
            median(winners_to_unforced_ratio))

ggplot(master_means, aes(x = mean_per_game)) +
  geom_density() #symmetric

ggplot(master_means, aes(x = winners_to_unforced_ratio)) +
  geom_density() #skewed right


# plot(master_means$winners_to_unforced_ratio ~ master_means$mean_per_game)
fit = lm(master_means$winners_to_unforced_ratio ~master_means$mean_per_game)
# abline(fit)
resi =  resid(fit)

# 
# master_means$consistency =
#   case_when(
#     resi >= 0  ~ 'consistent',
#     resi < 0 ~ 'inconsistent'
#   )

master_means

mmpg = median(master_means$mean_per_game)
mmpg

mwur = median(master_means$winners_to_unforced_ratio)
mwur

master_means$aggression = 
  case_when(
    master_means$mean_per_game >= mmpg ~ 'aggressive',
    master_means$mean_per_game < mmpg ~ 'defensive')

master_means$consistency = 
  case_when(
    master_means$winners_to_unforced_ratio >= mwur ~ 'consistent',
    master_means$winners_to_unforced_ratio < mwur ~ 'inconsistent')

style_counts =  master_means %>%
   group_by(., consistency, aggression) %>%
   summarise(., n())
style_counts

master_means %>% 
  filter(., name == 'Venus Williams')
style_counts
master_means %>% 
  ggplot(., aes(x = mean_per_game, y = winners_to_unforced_ratio)) +
  geom_point(aes(color = consistency)) +
  geom_smooth(method ="lm")

cor(master_means$mean_per_game, master_means$winners_to_unforced_ratio)

just_styles = master_means %>%
  select(., name, consistency, aggression)
just_styles

just_styles %>% 
  filter(., name == 'Ricardas Berankis')

master

WL_by_style


WL_by_style =  inner_join(master, just_styles, by = c('winner_name' = 'name')) %>%
  rename(.,
         winner_aggression = aggression,
         winner_consistency = consistency) %>%
  inner_join(., just_styles, by = c('loser_name' = 'name')) %>%
  rename(.,
         loser_aggression = aggression,
         loser_consistency = consistency)

W_UE_by_style = inner_join(master_allsets,
                           just_styles,
                           by = 'name')

master_allsets

master
DF_sets

WL_by_style
W_UE_by_style

style_matchups = WL_by_style %>%
  group_by(.,
           winner_aggression,
           winner_consistency,
           loser_aggression,
           loser_consistency) %>%
  summarise(
    .,
    winner_games = sum(winner_games),
    loser_games = sum(loser_games)
  ) %>%
  filter(., (winner_aggression != loser_aggression) |
           (winner_consistency != loser_consistency)) %>% 
  ungroup(.)

style_matchups

WL_by_style
style_matchups
style_matchups %>%  ungroup(.)

barf1 = style_matchups %>%
  select(., aggression = winner_aggression,
         consistency = winner_consistency,
         games_for = winner_games,
         games_against = loser_games)

barf2 = style_matchups %>%
  select(., aggression = loser_aggression,
         consistency = loser_consistency,
         games_for = loser_games,
         games_against = winner_games)

style_win_pcts = bind_rows(barf1, barf2) %>%
  group_by(., aggression, consistency) %>% 
  summarise(., games_won = sum(games_for), games_lost = sum(games_against)) %>% 
  mutate(.,
         win_percent = games_won / (games_won + games_lost),
         year = 2019)

style_win_pcts

# style_matchups$win_percent =
#   win_pct(style_matchups$winner_style, style_matchups$loser_style)
# 
# style_matchups = style_matchups %>%
#   mutate(., year = as.numeric(s))
# style_matchups
#
#   return(list('overall mean winners per game' = mwpg,
#               'overall mean unforced per game' = mupg,
#               'mean W/UE and style per player' = master_means,
#               'style matchups' = style_matchups))


master_W
blah1 = WL_by_style %>% 
  filter(.,
         winner_aggression == loser_aggression,
         winner_consistency == loser_consistency) %>% 
  mutate(.,
         aggression = winner_aggression,
         consistency = winner_consistency) %>% 
  select(.,
         -winner_aggression,
         -loser_aggression,
         -winner_consistency,
         -loser_consistency
  ) %>% 
  group_by(., aggression, consistency, name = winner_name) %>% 
  summarise(., games_won= sum(winner_games), games_lost = sum(loser_games)) %>% 
  ungroup(.)

blah2 = WL_by_style %>% 
  filter(.,
         winner_aggression == loser_aggression,
         winner_consistency == loser_consistency) %>% 
  mutate(.,
         aggression = winner_aggression,
         consistency = winner_consistency) %>% 
  select(.,
         -winner_aggression,
         -loser_aggression,
         -winner_consistency,
         -loser_consistency
  ) %>% 
  group_by(., aggression, consistency, name = loser_name) %>% 
  summarise(., games_won = sum(loser_games), games_lost = sum(winner_games)) %>% 
  ungroup(.)

blah3 = bind_rows(blah1, blah2) %>% 
  group_by(., aggression, consistency, name) %>% 
  summarise(., games_won = sum(games_won), games_lost = sum(games_lost)) %>% 
  mutate(., win_percent = games_won/(games_won + games_lost))
blah1

just_styles %>% 
  filter(., name == 'Stefanos Tsitsipas')


style_matchups
barf1 = style_matchups %>%
  select(., aggression = winner_aggression,
         consistency = winner_consistency,
         games_for = winner_games,
         games_against = loser_games)
barf1

barf2 = style_matchups %>%
  select(., aggression = loser_aggression,
         consistency = loser_consistency,
         games_for = loser_games,
         games_against = winner_games)
barf2

bind_rows(barf1, barf2) %>%
  group_by(., aggression, consistency) %>% 
  summarise(., games_won = sum(games_for), games_lost = sum(games_against)) %>% 
  mutate(., win_percent = games_won / (games_won + games_lost))


style_win_pcts



############################################

aggressive.consistent = just_styles %>% 
  filter(., aggression == 'aggressive', consistency == 'consistent')

aggressive.inconsistent = just_styles %>% 
  filter(., aggression == 'aggressive', consistency == 'inconsistent')

defensive.consistent = just_styles %>% 
  filter(., aggression == 'defensive', consistency == 'consistent')

defensive.inconsistent = just_styles %>% 
  filter(., aggression == 'defensive', consistency == 'inconsistent')

inner_join(master_allsets, aggressive.consistent, by = 'name') %>% 
  inner_join()

inner_join(master_allsets, just_styles, by = 'name')

WL_by_style

WL_by_style %>% 
  filter(.,
         winner_aggression == loser_aggression,
         winner_consistency == loser_consistency) %>% 
  group_by(., aggression, consistency)

inner_join(.,
           master,
           WL_by_style,
           by = c('year, tourney_name'))
  

WL_by_style
W_UE_by_style %>% 
  filter(., name == 'Novak Djokovic')

W_UE_by_style

style_win_pcts = W_UE_by_style %>% 
  group_by(., consistency, aggression) %>% 
  summarise(.,
            sum(games_won),
            sum(games_lost),
            sum(games_won) / sum(games_won + games_lost))

WL_by_style
W_UE_by_style %>% 
  filter(.,
         winner_aggression == loser_aggression,
         winner_consistency == loser_consistency) %>% 
  select(.,
         name,
         aggression = winner_aggression,
         consistency = winner_consistency,
         winner_games,
         loser_games,
         winner_winners,
         winner_unforced,
         loser_winners,
         loser_unforced) %>% 
  group_by(., aggression, consistency, name) %>% 
  summarise(.,
            win_percent = sum(winner_games) / (sum(winner_games + loser_games))) %>% 
    arrange(., desc(win_percent))
  




style_matchups

x=c()
for (i in 1:nrow(style_matchups)) {
  for (j in 1:nrow(style_matchups)) {
    if (style_matchups$winner_aggression[i] ==  style_matchups$loser_aggression[j] &
        style_matchups$winner_consistency[i] ==  style_matchups$loser_consistency[j] &
        style_matchups$winner_aggression[j] ==  style_matchups$loser_aggression[i] &
        style_matchups$winner_consistency[j] ==  style_matchups$loser_consistency[i]) {
      x[i] = (style_matchups$winner_games[i] + style_matchups$loser_games[j]) /
        (style_matchups$winner_games[i] + style_matchups$loser_games[j] +
           style_matchups$winner_games[j] + style_matchups$loser_games[i])
    }
  }
}

style_matchups$win_percent = x
style_matchups = style_matchups %>% 
  select(., -winner_games, -loser_games) %>% 
  unite(.,
        col = winner_style,
        winner_aggression,
        winner_consistency,
        sep = ', ') %>% 
  unite(.,
        col = loser_style,
        loser_aggression,
        loser_consistency,
        sep = ', ')
W_UE_by_style
WL_by_style
W_UE_by_style %>% 
  group_by(., consistency, aggression, name) %>% 
  summarise(., win_percent = sum(games_won) / (sum(games_won) + sum(games_lost)))

style_matchups

W_UE_by_style
