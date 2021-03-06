library(dplyr)
library(tidyr)
library(ggplot2)

matchstats = read.csv(file = './charting-w-stats-Overview.csv')

matchstats = matchstats %>%
  separate(
    .,
    col = match_id,
    into = c(
      'match_date',
      'womens',
      'tourney_name',
      'round',
      'player_1',
      'player_2'
    ),
    sep = '-'
  ) %>%
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
  select(.,
         year,
         tourney_name,
         player_1,
         player_2,
         player,
         winners,
         unforced,
         set)

player1stats = matchstats %>%
  filter(., player == 1) %>%
  select(
    .,
    year,
    tourney_name,
    player_1,
    player_2,
    player_1_winners = winners,
    player_1_unforced = unforced,
    set
  )

player2stats = matchstats %>%
  filter(., player == 2) %>%
  select(
    .,
    year,
    tourney_name,
    player_1,
    player_2,
    player_2_winners = winners,
    player_2_unforced = unforced,
    set
  )

playerstats =
  inner_join(
    player1stats,
    player2stats,
    by = c('year',
           'tourney_name',
           'player_1',
           'player_2',
           'set')
  )

playerstats$set = as.numeric(playerstats$set)

#----------------------------

style_by_year = function(s) {
  DF = read.csv(file = paste('./wta_matches_', s, '.csv', sep = ''),
                header = TRUE)
  DF$year = sapply(DF$tourney_date, function(x) {
    substr(x, 1, 4)
  })
  
  DF2 = DF %>%
    separate(
      .,
      score,
      sep = ' ',
      into = c('set1', 'set2', 'set3', 'set4'),
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
  
  DF_sets = bind_rows(DF_set1, DF_set2, DF_set3, DF_set4) %>%
    filter(
      .,
      !grepl("[a-z]|]|\\[", setscore, ignore.case = TRUE),
      setscore != '',
      !is.na(setscore)
    ) %>%
    mutate(.,
           setscore = sub(
             pattern = '\\(.*\\)',
             replacement = '',
             x = setscore
           )) %>%
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
  
  DF_sets$tourney_name = tolower(x = DF_sets$tourney_name)
  
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
  
  master = bind_rows(placeholder1, placeholder2) %>%
    mutate(
      .,
      winner_games = pmax(games1, games2),
      loser_games = pmin(games1, games2)
    ) %>%
    select(., -games1, -games2)
  
  #test
  master
  
  master_W = master %>%
    select(
      .,
      name = winner_name,
      winners = winner_winners,
      unforced = winner_unforced,
      games_won = winner_games,
      games_lost = loser_games
    )
  
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
  
  #test
  master_allsets
  
  
  master_means = master_allsets %>%
    group_by(., name) %>%
    summarise(
      .,
      mean_per_game = sum(winners + unforced) / sum(games_won + games_lost),
      winners_to_unforced_ratio = sum(winners) / (sum(unforced))
    )
  
  #test
  master_means
  
  
  mmpg = mean(master_means$mean_per_game)
  
  mwur = median(master_means$winners_to_unforced_ratio)
  
  master_means$aggression =
    case_when(
      master_means$mean_per_game >= mmpg ~ 'aggressive',
      master_means$mean_per_game < mmpg ~ 'defensive'
    )
  
  master_means$consistency =
    case_when(
      master_means$winners_to_unforced_ratio >= mwur ~ 'consistent',
      master_means$winners_to_unforced_ratio < mwur ~ 'inconsistent'
    )
  
  #test
  master_means
  
  master_means = master_means %>%
    unite(., style, aggression, consistency, sep = ', ')
  
  #test
  master_means
  
  style_counts =  master_means %>%
    group_by(., style) %>%
    summarise(., percent = n() / nrow(master_means))
  
  #test
  style_counts
  
  just_styles = master_means %>%
    select(., name, style)
  
  #test
  just_styles
  
  W_UE_by_style = inner_join(master_allsets,
                             just_styles,
                             by = 'name')
  
  #test
  W_UE_by_style
  
  WL_by_style =  inner_join(master, just_styles, by = c('winner_name' = 'name')) %>%
    rename(.,
           winner_style = style) %>%
    inner_join(., just_styles, by = c('loser_name' = 'name')) %>%
    rename(.,
           loser_style = style)
  
  #test
  WL_by_style
  
  style_matchups = WL_by_style %>%
    group_by(.,
             winner_style,
             loser_style) %>%
    summarise(.,
              winner_games = sum(winner_games),
              loser_games = sum(loser_games)) %>%
    filter(.,
           (winner_style != loser_style))
  
  #test
  style_matchups
  
  test_1 = style_matchups %>%
    select(
      style = winner_style,
      opponent = loser_style,
      p1_games = winner_games,
      p2_games = loser_games
    )
  
  test_2 = style_matchups %>%
    select(
      style = loser_style,
      opponent = winner_style,
      p1_games = loser_games,
      p2_games = winner_games
    )
  
  style_matchups = bind_rows(test_1, test_2) %>%
    group_by(., style, opponent) %>%
    summarise(., win_percent = sum(p1_games) / sum(p1_games + p2_games))
  
  
  style_win_pcts = W_UE_by_style %>%
    group_by(., style) %>%
    summarise(.,
              games_won = sum(games_won),
              games_lost = sum(games_lost),
              win_percent = sum(games_won) / sum(games_won + games_lost))
  
  
  return(
    list(
      master_means,
      style_win_pcts,
      style_counts,
      style_matchups,
      W_UE_by_style
    )
  )
}


players.ratios.style = bind_rows(lapply(2012:2020, function(x) {
  D = style_by_year(x)[[1]] %>%
    mutate(., year = x)
}))

win.pct.by.style = bind_rows(lapply(2012:2020, function(x) {
  D = style_by_year(x)[[2]] %>%
    mutate(., year = x)
}))

style.counts = bind_rows(lapply(2012:2020, function(x) {
  D = style_by_year(x)[[3]] %>%
    mutate(., year = x) %>%
    select(., year, style, percent)
}))


style.matchups = bind_rows(lapply(2012:2020, function(x) {
  D = style_by_year(x)[[4]] %>%
    mutate(., year = x)
}))

by.style = bind_rows(lapply(2012:2020, function(x) {
  D = style_by_year(x)[[5]] %>%
    mutate(., year = x)
}))


year = rep(2012:2020, c(4, 4, 4, 4, 4, 4, 4, 4, 4))
style = rep(
  c(
    'aggressive, consistent',
    'aggressive, inconsistent',
    'defensive, consistent',
    'defensive, inconsistent'
  ),
  9
)


correlation_df = by.style %>% 
  group_by(., name) %>% 
  summarise(.,
            mmpg = sum(winners + unforced) / sum(games_won + games_lost),
            wtur = sum(winners) / sum(unforced),
            mwpg = sum(winners) / sum(games_won + games_lost),
            mupg = sum(unforced) / sum(games_won + games_lost),
            win_percent = sum(games_won) / (sum(games_won + games_lost))) %>% 
  inner_join(., just_styles, by = 'name') %>%
  ungroup(.) %>% 
  group_by(style) %>%
  summarise(cor.mwpg = cor(win_percent, mwpg),
            cor.mupg = cor(win_percent, mupg),
            cor.mmpg = cor(win_percent, mmpg),
            cor.wtur = cor(win_percent, wtur)) %>% 
  gather(., corr.between, corr.coef, 2:5)


#frequencies of playing styles by year
style.counts %>%
  ggplot(aes(x = year, y = percent)) +
  geom_col(aes(fill = style), position = 'fill')



with_GS = players.ratios.style %>%
  mutate(
    .,
    GS = case_when(((year == 2012) & (
      name %in% c('Victoria Azarenka',
                  'Maria Sharapova',
                  'Serena Williams')
    )) ~ 'yes',
    ((year == 2012) & (
      name %in% c('Serena Williams',
                  'Maria Sharapova',
                  'Victoria Azarenka')
    )) ~ 'yes',
    ((year == 2013) & (
      name %in% c('Victoria Azarenka',
                  'Serena Williams',
                  'Marion Bartoli')
    )) ~ 'yes',
    ((year == 2014) & (
      name %in% c('Na Li',
                  'Maria Sharapova',
                  'Petra Kvitova',
                  'Serena Williams')
    )) ~ 'yes',
    ((year == 2015) & (
      name %in% c('Serena Williams',
                  'Flavia Pennetta')
    )) ~ 'yes',
    ((year == 2016) & (
      name %in% c('Angelique Kerber',
                  'Garbine Muguruza',
                  'Serena Williams')
    )) ~ 'yes',
    ((year == 2017) & (
      name %in% c(
        'Serena Williams',
        'Jelena Ostapenko',
        'Garbine Muguruza',
        'Sloane Stephens'
      )
    )) ~ 'yes',
    ((year == 2018) & (
      name %in% c(
        'Caroline Wozniacki',
        'Simona Halep',
        'Angelique Kerber',
        'naomi Osaka'
      )
    )) ~ 'yes',
    ((year == 2019) & (
      name %in% c(
        'Naomi Osaka',
        'Ashleigh Barty',
        'Simona Halep',
        'Bianca Andreescu'
      )
    )) ~ 'yes',
    ((year == 2020) & (name == 'Sofia Kenin')) ~ 'yes',
    TRUE ~ 'no'
    )
  )



#a plot of grand slam winners among all players, highlighted by style
with_GS %>%
  ggplot(aes(x = mean_per_game, y = winners_to_unforced_ratio)) +
  geom_point(data = with_GS %>% filter(., GS == 'no'),
             aes(color = style),
             size = 0.5) +
  scale_color_brewer(palette = "RdGy") +
  geom_point(
    data = with_GS %>% filter(., GS == 'yes'),
    color = 'green',
    size = 1
  ) +
  facet_wrap( ~ year)


#win percent of each style against other styles by year
style.matchups %>% 
  ggplot(., aes(x = style, y = win_percent)) +
  geom_col(aes(fill = opponent), position = 'dodge') +
  scale_fill_brewer(palette = 'RdGy') +
  facet_wrap(~year)


#overall win percents for each style by year

win.pct.by.style %>% 
  ggplot(., aes(x = style, y = win_percent)) + 
  geom_col(aes(fill = style)) +
  scale_fill_brewer(palette = 'RdGy') +
  facet_wrap(~ year)




##################



correlation_df %>% 
  ggplot(aes(x = style, y = corr.between,)) +
  geom_tile(aes(fill = corr.coef, ), colour = 'black') +
  scale_fill_gradient2(
    low = "darkred", high = "darkblue", mid = "white", midpoint = 0)