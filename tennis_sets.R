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
    filter(., ! grepl('RET', score, fixed = T)) %>% 
    separate(., score, sep = ' ',
             into = c('set1', 'set2', 'set3'),
             fill = 'right')
  
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
    filter(., setscore != 'W/O', is.na(setscore) == F) %>% 
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
    summarise(., mean_winners_per_game = sum(winners)/(sum(games1)+sum(games2)),
              mean_unforced_per_game = sum(unforced)/(sum(games1)+sum(games2)))
  
  #mean winners/unforced per game overall among all players:
  mwpg = means[1,1]
  mupg = means[1,2]
  
  master_means = master_allsets %>% 
    group_by(., name) %>%
    summarise(., mean_winners_per_game = sum(winners)/(sum(games1)+sum(games2)),
                 mean_unforced_per_game = sum(unforced)/(sum(games1)+sum(games2)))
  
  master_means$style = 
    case_when(master_means$mean_unforced_per_game >= mupg &
                master_means$mean_winners_per_game >= mwpg ~ 'HRI_HRE',
              master_means$mean_unforced_per_game >= mupg &
                master_means$mean_winners_per_game < mwpg ~ 'HRI_LRE',
              master_means$mean_unforced_per_game < mupg &
                master_means$mean_winners_per_game >= mwpg ~ 'LRI_HRE',
              master_means$mean_unforced_per_game < mupg &
                master_means$mean_winners_per_game < mwpg ~ 'LRI_LRE')
  master_means %>% 
    group_by(., style) %>% 
    summarise(., n())
  
  master_means %>% 
    ggplot(., aes(x = mean_winners_per_game, y = mean_unforced_per_game)) +
    geom_point(aes(color = style))
  
  just_styles = master_means %>% 
    select(., name, style)
  
  WL_by_style =  inner_join(DF_sets, just_styles, by = c('winner_name' = 'name')) %>% 
    rename(., winner_style = style) %>% 
    inner_join(., just_styles, by = c('loser_name' = 'name')) %>% 
    rename(., loser_style = style) %>% 
    select(., -games1, -games2, -set)
  
  style_matchups = WL_by_style %>% 
    group_by(., winner_style, loser_style) %>% 
    summarise(., matches = n()) %>% 
    filter(., winner_style != loser_style)
  
  win_pct = function(x, y) {
    res = c()
    for (i in 1:12) {
      for (j in 1:12) {
        if (x[i] == y[j] & x[j] == y[i]) {
          res[i] = style_matchups$matches[i]/(style_matchups$matches[i] + style_matchups$matches[j])
        }
      }
    }
    return(res)
  }
  
  style_matchups$win_percent =
    win_pct(style_matchups$winner_style, style_matchups$loser_style)
  
  return(list('overall mean winners per game' = mwpg,
              'overall mean unforced per game' = mupg,
              'mean W/UE and style per player' = master_means,
              'style matchups' = style_matchups))
}

for(s in as.character(2005:2020)) {
  d = data.frame()
}

