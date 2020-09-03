library(dplyr)
library(tidyr)
library(ggplot2)

matchstats = read.csv(file = './charting-w-stats-Overview.csv')

matchstats = matchstats %>% 
  separate(., col = match_id,
           into = c('match_date',
                    'womens',
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

style_by_year = function(s) {

  DF = read.csv(file = paste('./wta_matches_', s, '.csv', sep = ''), header = TRUE)
  DF$year = sapply(DF$tourney_date, function(x) {
    substr(x, 1, 4)
  })
  
  DF2 = DF %>% 
    filter(., !grepl('RET', score, fixed = T)) %>%
    filter(., !grepl('W/O', score, fixed = T)) %>%
    filter(., !grepl('J', score, fixed = T)) %>%
    separate(
      .,
      score,
      sep = ' ',
      into = c('set1', 'set2', 'set3', 'set4'),
      fill = 'right'
    ) %>% 
    filter(., is.na(set4) == TRUE) %>% 
    select(., -set4)
  
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
  
  DF_sets = bind_rows(DF_set1, DF_set2, DF_set3) %>% 
    filter(., setscore != 'DEF',
           is.na(setscore) == F,
           setscore != '') %>% 
    mutate(., setscore = sub(pattern = '\\(.*\\)', replacement = '', x = setscore))
  
  DF_sets = DF_sets %>% 
    separate(., setscore, into = c('games1', 'games2'), sep = '-') %>% 
    mutate(., games1 = as.numeric(games1),
           games2 = as.numeric(games2),
           winner_name2 = ifelse(games1 > games2,
                                 yes = winner_name,
                                 no = loser_name),
           loser_name2 = ifelse(games1 < games2,
                                yes = winner_name,
                                no = loser_name)) %>% 
    select(., year,
           tourney_name,
           winner_name = winner_name2,
           loser_name = loser_name2,
           games1,
           games2,
           set)
  
  DF_sets$tourney_name = tolower(x = DF_sets$tourney_name)
  
  placeholder1 = inner_join(DF_sets,
                            playerstats,
                            by = c('year',
                                   'tourney_name',
                                   'winner_name' = 'player_1',
                                   'loser_name' = 'player_2',
                                   'set')) %>% 
    select(., year,
           tourney_name,
           winner_name,
           loser_name,
           winner_winners = player_1_winners,
           winner_unforced = player_1_unforced,
           loser_winners = player_2_winners,
           loser_unforced = player_2_unforced,
           games1,
           games2)
  
  placeholder2 = inner_join(DF_sets,
                            playerstats,
                            by = c('year',
                                   'tourney_name',
                                   'winner_name' = 'player_2',
                                   'loser_name' = 'player_1',
                                   'set')) %>% 
    select(., year,
           tourney_name,
           winner_name,
           loser_name,
           winner_winners = player_2_winners,
           winner_unforced = player_2_unforced,
           loser_winners = player_1_winners,
           loser_unforced = player_1_unforced,
           games1,
           games2)
    
  master = bind_rows(placeholder1, placeholder2)

  master_W = master %>% 
    select(., name = winner_name,
           winners = winner_winners,
           unforced = winner_unforced,
           games1,
           games2)

  master_L = master %>% 
    select(., name = loser_name,
           winners = loser_winners,
           unforced = loser_unforced,
           games1,
           games2)

  master_allsets = bind_rows(master_W, master_L)
  
  means = master_allsets %>%
    summarise(
      .,
      mean_winners_per_game = sum(winners) / (sum(games1) + sum(games2)),
      winners_to_unforced_ratio = sum(winners) / (sum(unforced))
    )
  
  #mean winners/unforced per game overall among all players:
  mwpg = means[1, 1]
  wtur = means[1, 2]
  
  master_means = master_allsets %>%
    group_by(., name) %>%
    summarise(
      .,
      mean_winners_per_game = sum(winners) / (sum(games1) + sum(games2)),
      winners_to_unforced_ratio = sum(winners) / (sum(unforced))
    )
  
  master_means$consistency =
    case_when(
      master_means$winners_to_unforced_ratio >= wtur  ~ 'consistent',
      master_means$winners_to_unforced_ratio < wtur ~ 'inconsistent'
    )
  
  master_means$aggression = 
    case_when(
      master_means$mean_winners_per_game >= mwpg ~ 'aggressive',
      master_means$mean_winners_per_game < mwpg ~ 'defensive')
  
  style_counts =  master_means %>%
    group_by(., consistency, aggression) %>%
    summarise(., n())

  just_styles = master_means %>%
    select(., name, consistency, aggression)
  just_styles
  
  WL_by_style =  inner_join(DF_sets, just_styles, by = c('winner_name' = 'name')) %>%
    rename(.,
           winner_aggression = aggression,
           winner_consistency = consistency) %>%
    inner_join(., just_styles, by = c('loser_name' = 'name')) %>%
    rename(.,
           loser_aggression = aggression,
           loser_consistency = consistency) %>%
    select(.,-set) %>%
    mutate(.,
           winner_games = pmax(games1, games2),
           loser_games = pmin(games1, games2)) %>%
    select(.,-games1,-games2)
  
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
           year = as.numeric(s))
  
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
  
  WL_within_type = bind_rows(blah1, blah2) %>% 
    group_by(., aggression, consistency, name) %>% 
    summarise(., games_won = sum(games_won), games_lost = sum(games_lost)) %>% 
    mutate(.,
           win_percent = games_won/(games_won + games_lost),
           year = as.numeric(s))
  
  return(
    list(
      master_means,
      style_win_pcts,
      style_counts,
      WL_within_type
      )
    )
  
  # 
  # temp1 = style_matchups %>%
  #   group_by(., winner_style) %>%
  #   summarise(., games_won = sum(winning_style_games),
  #             games_lost = sum(losing_style_games)) %>%
  #   select(style = winner_style, games_won, games_lost)
  # 
  # temp2 = style_matchups %>%
  #   group_by(., loser_style) %>%
  #   summarise(., games_won = sum(losing_style_games),
  #             games_lost = sum(winning_style_games)) %>%
  #   select(style = loser_style, games_won, games_lost)
  # 
  # style_win_percent = bind_rows(temp1, temp2) %>%
  #   group_by(style) %>%
  #   summarise(., games_won = sum(games_won), games_lost = sum(games_lost)) %>%
  #   mutate(., win_percent = games_won/(games_won + games_lost))
  # 
  # return(list('winner to unforced ratio' = wtur,
  #             'overall mean unforced per game' = mupg,
  #             'number of players per style' = style_counts,
  #             'mean W/UE and style per player' = master_means,
  #             'style matchups' = style_matchups,
  #             'win percent by style' = style_win_percent),
  #             ' ' = WL_by_style)

}

style_by_year('2020')

# 
# Mwpg = function(s) return(style_by_year(s)[[1]])
# 
# Mupg = function(s) return(style_by_year(s)[[2]])
# 
# Style_counts = function(s) return(style_by_year(s)[[3]])
# 
# Master_means = function(s) return(style_by_year(s)[[4]])
# 
# Style_matchups = function(s) return(style_by_year(s)[[5]])
# 
# Style_win_percent = function(s) {
#   return(style_by_year(s)[[6]]) %>% 
#     mutate(., year = as.numeric(s))
# }
# 
# all_win_percents = bind_rows(Style_win_percent('2006'),
#           Style_win_percent('2007'),
#           Style_win_percent('2008'),
#           Style_win_percent('2009'),
#           Style_win_percent('2010'),
#           Style_win_percent('2011'),
#           Style_win_percent('2012'),
#           Style_win_percent('2013'),
#           Style_win_percent('2014'),
#           Style_win_percent('2015'),
#           Style_win_percent('2016'),
#           Style_win_percent('2017'),
#           Style_win_percent('2018'),
#           Style_win_percent('2019'),
#           Style_win_percent('2020'))
# all_win_percents %>% 
#   ggplot(., aes(x = year, y = win_percent)) +
#   geom_line(aes(color = style))


#### rework styles:
## aggressive: num winners >= avg.
## defensive: num winners < avg.
## consistent: ratio winners/errors >= avg.
## erratic: ratio winners/errors < avg.