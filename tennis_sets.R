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

playerstats

#----------------------------

style_by_year = function(s) {

  DF = read.csv(file = paste('./wta_matches_', s, '.csv', sep = ''), header = TRUE)
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
    
  master = bind_rows(placeholder1, placeholder2) %>% 
    mutate(., winner_games = pmax(games1, games2), 
           loser_games = pmin(games1, games2)
    ) %>% 
    select(., -games1, -games2)

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
  
  
  master_means = master_allsets %>%
    group_by(., name) %>%
    summarise(
      .,
      mean_per_game = sum(winners+unforced) / sum(games_won + games_lost),
      winners_to_unforced_ratio = sum(winners) / (sum(unforced))
    )

  
  mmpg = median(master_means$mean_per_game)
  
  mwur = median(master_means$winners_to_unforced_ratio)
  
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

  just_styles = master_means %>%
    select(., name, consistency, aggression)
  just_styles
  
  W_UE_by_style = inner_join(master_allsets,
                             just_styles,
                             by = 'name')
  
  WL_by_style =  inner_join(master, just_styles, by = c('winner_name' = 'name')) %>%
    rename(.,
           winner_aggression = aggression,
           winner_consistency = consistency) %>%
    inner_join(., just_styles, by = c('loser_name' = 'name')) %>%
    rename(.,
           loser_aggression = aggression,
           loser_consistency = consistency)
  
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
             (winner_consistency != loser_consistency))
  
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
  
  
  style_matchups %>% 
    select(., -winner_games, -loser_games)
  
  
  style_win_pcts = W_UE_by_style %>% 
    group_by(., consistency, aggression) %>% 
    summarise(.,
              sum(games_won),
              sum(games_lost),
              sum(games_won) / sum(games_won + games_lost))
  
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
  
  
  return(
    list(
      master_means %>% 
        unite(.,
              col = style,
              aggression,
              consistency,
              sep = ', '),
      style_win_pcts %>% 
        unite(.,
              col = style,
              aggression,
              consistency,
              sep = ', '),
      style_counts %>% 
        unite(.,
              col = style,
              aggression,
              consistency,
              sep = ', '),
      style_matchups %>% 
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
              sep = ', '),
      W_UE_by_style %>% 
        unite(.,
              col = style,
              aggression,
              consistency,
              sep = ', ')
      )
    )
}


style_by_year(2002)

players.ratios.style = bind_rows(lapply(2011:2020, function(x) {
  D = style_by_year(x)[[1]] %>%
    mutate(., year = x)
}))

win.pct.by.style = bind_rows(lapply(2011:2020, function(x) {
  D = style_by_year(x)[[2]] %>%
    mutate(., year = x)
}))

style.counts = bind_rows(lapply(2011:2020, function(x) {
  D = style_by_year(x)[[3]] %>%
    mutate(., year = x) %>% 
    select(., year, style, number = `n()`)
}))


style.matchups = bind_rows(lapply(2011:2020, function(x) {
  D = style_by_year(x)[[4]] %>%
    mutate(., year = x)
}))

by.style = bind_rows(lapply(2011:2020, function(x) {
  D = style_by_year(x)[[5]] %>%
    mutate(., year = x)
}))


stats_within_type = function(s) {
  return(
    style_by_year(s)[[5]] %>% 
      group_by(style, name) %>% 
      summarise(.,
                win_pct_within_group = sum(games_won) / sum(games_won + games_lost),
                mean_per_game = sum(winners+unforced)/sum(games_won + games_lost),
                winner_to_unforced_ratio = sum(winners) / sum(unforced),
                mean_winners_per_game = sum(winners)/sum(games_won + games_lost),
                mean_unforced_per_game = sum(unforced)/sum(games_won + games_lost),)
  ) %>% 
    ungroup(.)
}
stats_within_type(2019) %>% 
  ggplot(aes(x = win_pct_within_group, y = mean_winners_per_game)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~style)

stats_within_type(2019) %>% 
  ggplot(aes(x = win_pct_within_group, y = mean_unforced_per_game)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~style)


correlation_mupg = function(y, s) {
  D = stats_within_type(y) %>% 
    filter(., style == s) %>% 
    select(x = win_pct_within_group, y = mean_unforced_per_game)
  return(cor(D$x, D$y))
}

correlation_mwpg = function(y, s) {
  D = stats_within_type(y) %>% 
    filter(., style == s) %>% 
    select(x = win_pct_within_group, y = mean_winners_per_game)
  return(cor(D$x, D$y))
}

correlation_mupg(2017, 'aggressive, consistent')
correlation_mwpg(2018, 'defensive, inconsistent')

correlation_df = data.frame(
  year = 2011:2020,
  mupg.agg.con = 
    sapply(2011:2020, function(x) correlations_mupg(x, 'aggressive, consistent')),
  mupg.agg.inc = 
    sapply(2011:2020, function(x) correlations_mupg(x, 'aggressive, inconsistent')),
  mupg.def.con = 
    sapply(2011:2020, function(x) correlations_mupg(x, 'defensive, consistent')),
  mupg.def.inc = 
    sapply(2011:2020, function(x) correlations_mupg(x, 'defensive, inconsistent')),
  mwpg.agg.con = 
    sapply(2011:2020, function(x) correlations_mwpg(x, 'aggressive, consistent')),
  mwpg.agg.inc = 
    sapply(2011:2020, function(x) correlations_mwpg(x, 'aggressive, inconsistent')),
  mwpg.def.con = 
    sapply(2011:2020, function(x) correlations_mwpg(x, 'defensive, consistent')),
  mwpg.def.inc = 
    sapply(2011:2020, function(x) correlations_mwpg(x, 'defensive, inconsistent'))
)

year = rep(2011:2020, c(4,4,4,4,4,4,4,4,4,4))
style = rep(c('aggressive, consistent',
              'aggressive, inconsistent',
              'defensive, consistent',
              'defensive, inconsistent'),
            10)
correlation_df = data.frame(year = year, style = style, stringsAsFactors = FALSE)
corr.mupg = c()
corr.mwpg = c()
for (i in 1:40) {
  corr.mupg[i] = correlation_mupg(dddd$year[i], dddd$style[i])
  corr.mwpg[i] = correlation_mwpg(dddd$year[i], dddd$style[i])
}
correlation_df$corr.mupg = corr.mupg
correlation_df$corr.mwpg = corr.mwpg
correlation_df
correlation_df %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = corr.mupg), color = 'red') +
  facet_wrap(~ style) +
  ggtitle('Linear correlation between win percent and mean winners per game')

correlation_df %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = corr.mwpg), color = 'blue') +
  facet_wrap(~ style) +
  ggtitle('Linear correlation between win percent and mean winners per game')


style.counts %>% 
  ggplot(aes(x = year, y = number)) +
  geom_col(aes(fill = style), position = 'fill')

players.ratios.style %>% 
  ggplot(aes(x = mean_per_game, y = winners_to_unforced_ratio)) +
  geom_point(aes(color = style), size = 0.5) +
  scale_color_brewer(palette="RdGy") +
  facet_wrap(~year)




with_GS = players.ratios.style %>%
  mutate(.,
         GS = case_when(((year == 2011) & (
           name %in% c('Kim Clijsters',
                       'Na Li',
                       'Petra Kvitova',
                       'Samantha Stosur')
         )) ~ 'yes',
         ((year == 2012) & (
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
           name %in% c('Serena Williams',
                       'Jelena Ostapenko',
                       'Garbine Muguruza',
                       'Sloane Stephens')
         )) ~ 'yes',
         ((year == 2018) & (
           name %in% c('Caroline Wozniacki',
                       'Simona Halep',
                       'Angelique Kerber',
                       'naomi Osaka')
         )) ~ 'yes',
         ((year == 2019) & (
           name %in% c('Naomi Osaka',
                       'Ashleigh Barty',
                       'Simona Halep',
                       'Bianca Andreescu')
         )) ~ 'yes',
         ((year == 2020) & (
           name == 'Sofia Kenin'
         )) ~ 'yes',
         TRUE ~ 'no'))
with_GS %>% 
  ggplot(aes(x = mean_per_game, y = winners_to_unforced_ratio)) +
  geom_point(data = with_GS %>% filter(., GS == 'no'),
             aes(color = style), size = 0.5) +
  scale_color_brewer(palette="RdGy") +
  geom_point(data = with_GS %>% filter(., GS == 'yes'),
             color = 'green', size = 1) +
  facet_wrap(~year)
