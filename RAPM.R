library(tidyverse)
library(ggplot2)
library(GGally)
library(glmnet)
library(gt)
library(gtExtras)

source('/home/francesco/Projects/sdeng/readers/stats/stats.R')
source('/home/francesco/Projects/sdeng/readers/import/R/csv.R')

data <- get_data('analyses/RAPM/data')
ign <- list2env(data, envir = .GlobalEnv)

df <-
	actions %>%
		inner_join(games, by = c('season_id', 'edition_id', 'game_id'), suffix = c('', '_game')) %>%
		group_by(game_id) %>%
		mutate(
			team = case_when(
				team_id == home_team_id ~ 'home',
				team_id == away_team_id ~ 'away',
				TRUE ~ NA_character_),
			   FT_PE = grepl('.*2/2.*', details) | grepl('.*3/3.*', details),
			   type = ifelse(type %in% c('FTA', 'FTM') & FT_PE, paste0(type, '_PE'), type),
			   home_difference = home_score - lag(home_score, 1, 0),
			   away_difference = away_score - lag(away_score, 1, 0),
		)

df <- select(df, game_id, type, team, home_difference, away_difference, h1:a5) %>%
	group_by(h1, h2, h3, h4, h5, a1, a2, a3, a4, a5) %>%
	summarise(
		home_possessions = sum(team == 'home' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'home' & type == 'OREB'),
		away_possessions = sum(team == 'away' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'away' & type == 'OREB'),
		possessions = (home_possessions + away_possessions) / 2,
		home_points = sum(home_difference),
		away_points = sum(away_difference),
		home_PPP = home_points / home_possessions,
		away_PPP = away_points / away_possessions,
		net_rating = 100 * (home_PPP - away_PPP)
	) %>%
	ungroup()

df <- filter(df, possessions >= 2) %>%
	mutate(stint = row_number()) %>%
	select(possessions, stint, h1:a5, net_rating) %>%
	pivot_longer(cols = c(h1:a5), values_to = 'player_id', names_to = 'position') %>%
	mutate(value = ifelse(startsWith(position, 'h'), 1, -1))

df <- select(df, -position, -contains('PPP')) %>%
		filter(is.finite(net_rating)) %>%
		distinct() %>%
		pivot_wider(names_from = player_id, values_fill = 0) %>%
		select(-stint)

possessions <- df$possessions
net_rating <- df$net_rating

players_matrix <-
	df %>%
		select(-net_rating, -possessions) %>%
		data.matrix()

apm <-
	lm(formula = net_rating ~ . -possessions, data=df, weights = possessions) %>%
		broom::tidy() %>%
		rename(
			player_id = term,
			APM = estimate
		) %>%
		mutate(player_id = gsub("`", "", player_id)) %>%
		slice(-1) %>%
		select(player_id, APM)


lambda <- cv.glmnet(players_matrix, net_rating, alpha=0, weights=possessions, nfolds=10, type.measure = 'deviance')
lambda.min <- lambda$lambda.min

ridge <- glmnet(players_matrix, net_rating,
				family='gaussian',
				weights=possessions, alpha=0,lambda=lambda.min) #run the ridge regression. x is the matrix of independent variables, Marg is the dependent variable, Poss are the weights. alpha=0 indicates the ridge penalty.


# rapm <- MASS::lm.ridge(formua = net_rating ~ - possessions., data=df, lambda=seq(0, 20000, 200))
# rapm_tidy <- broom::tidy(rapm)
# glimpse <- tibble::glimpse(rapm_tidy)
#
# ggplot(rapm_tidy, aes(lambda, GCV)) +
# 	geom_line() +
# 	geom_vline(xintercept = glimpse$GCV)

boxscore_stats <-
	boxscores %>%
		get_stats_from_boxscores(groups = 'player_id') %>%
	select(player_id, GAMES, W, L)

stats <-
	actions %>%
		get_players_stats_from_actions_efficiently(groups = 'player_id') %>%
		select(player_id, MIN, POSS, PM, IMPACT, NETRTG_tm_on) %>%
		inner_join(boxscore_stats, by = 'player_id')

rapm <-
	coef(ridge,s=lambda.min) %>%
	as.matrix() %>%
	as.data.frame() %>%
	rownames_to_column('player_id') %>%
	as_tibble() %>%
	slice(-1) %>%
	rename(RAPM = s1)

results <-
	inner_join(rapm, stats) %>%
		inner_join(apm) %>%
		mutate(RECORD = W / GAMES) %>%
		arrange(desc(RAPM))

stats
# %>%
# 	filter(POSS >= 1500) %>%
	# view('RAPM')

# toy_actions <- read_csv('analyses/RAPM/test.csv')
# toy_games <- read_csv('analyses/RAPM/test_games.csv')
#
# toy_actions
# toy_actions %>%
# 		inner_join(toy_games, by = c('season_id', 'edition_id', 'game_id'), suffix = c('', '_game')) %>%
# 		group_by(game_id) %>%
# 		mutate(team = case_when(
# 			team_id == home_team_id ~ 'home',
# 			team_id == away_team_id ~ 'away',
# 			TRUE ~ NA_character_
# 			),
# 			   FT_PE = grepl('.*2/2.*', details) | grepl('.*3/3.*', details),
# 			   type = ifelse(type %in% c('FTA', 'FTM') & FT_PE, paste0(type, '_PE'), type),
# 			   # score_difference = home_score - away_score - lag(home_score - away_score, default = 0),
# 			   home_difference = home_score - lag(home_score, 1, 0),
# 			   away_difference = away_score - lag(away_score, 1, 0),
# 		) %>%
# 		select(game_id, FT_PE, type, team, home_difference, away_difference, h1:a3) %>%
# 		group_by(h1, h2, h3, a1, a2, a3) %>%
# 		summarise(
# 			home_possessions = sum(team == 'home' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'home' & type == 'OREB'),
# 			away_possessions = sum(team == 'away' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'away' & type == 'OREB'),
# 			possessions = (home_possessions + away_possessions) / 2,
# 			home_points = sum(home_difference),
# 			away_points = sum(away_difference),
# 			home_PPP = 100 * home_points / home_possessions,
# 			away_PPP = 100 * away_points / away_possessions,
# 			net_rating = home_PPP - away_PPP
# 		) %>%
# 		ungroup() %>%
# 		filter(possessions >= 2) %>%
# 		mutate(stint = row_number()) %>%
# 		select(possessions, stint, h1:a3, net_rating) %>%
# 		pivot_longer(cols = c(h1:a3), values_to = 'player_id')
# %>%
# 		mutate(
# 			# offense = ifelse(startsWith(name, 'h'), home_PPP, away_PPP),
# 			# defense = ifelse(startsWith(name, 'h'), away_PPP, home_PPP),
# 			value = ifelse(startsWith(name, 'h'), 1, -1),
# 		) %>%
# 		# head(20) %>%
# 		# pivot_longer(cols=c(offense, defense), names_to = 'side', values_to = 'result') %>%
# 		# inner_join(select(rosters, player_id, full_name)) %>%
# 		# select(-name, -player_id, -contains('PPP')) %>%
# 		select(-name, -stint) %>%
# 		# filter(side == 'offense', is.finite(result)) %>%
# 		filter(is.finite(net_rating)) %>%
# 		distinct() %>%
# 		pivot_wider(names_from = player_id, values_fill = 0)

results %>%
	filter(MIN >= 100) %>%
	select(RAPM, APM, PM, IMPACT, NETRTG_tm_on, RECORD) %>%
	rename(NETRTG_ON = NETRTG_tm_on) %>%
	ggpairs() +
	theme_minimal(base_size=9, base_family="Consolas") +
	theme(
		plot.background = element_rect(fill = 'ghostwhite', color = "ghostwhite"),
		plot.title.position = 'plot',
		plot.title = element_text(face = 'bold', size = 13, hjust = 0),
		plot.subtitle = element_text(margin = margin(5, 0, 5, 0), lineheight = 1.2, hjust = 0, size = 8),
		plot.caption = element_text(lineheight = 1.2, margin = margin(10, 0, 0, 0), hjust = 1),
		plot.margin = margin(10, 15, 10, 10),
		axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)),
		axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 5, l = 0)),
		legend.position = "top",
	) +
	labs(title = "Correlation between various impact metrics",
		 subtitle = "Across players with at least 100 minutes between 2017 and 2022",
		 caption = "Author: @f_olivo99\nData: euroleaguebasketball.net")

ggsave("analyses/RAPM/plots/correlogram.png", w = 6, h = 6, dpi = "retina")

table1 <-
	results %>%
		filter(MIN >= 1000) %>%
		mutate(PM = PM / GAMES, MPG = MIN / GAMES) %>%
		inner_join(rosters) %>%
		group_by(player_id) %>%
		slice_max(order_by = season_id, n = 1) %>%
		ungroup() %>%
		arrange(desc(RAPM)) %>%
		mutate(rank = paste0(row_number(), '.'), player = paste0(substr(name, 1, 1), '. ', surname)) %>%
		select(rank, picture_url, player, GAMES, RECORD, MIN, MPG, PM, NETRTG_tm_on, IMPACT, APM, RAPM) %>%
		head(10) %>%
		gt() %>%
		cols_label(
			rank = '',
			picture_url = '',
			player = 'PLAYER',
			NETRTG_tm_on = 'NETRTG ON',
		) %>%
		fmt_number(columns = c(APM, RAPM), decimals = 2) %>%
		fmt_number(columns = c(MPG), decimals = 1) %>%
		fmt_number(columns = c(NETRTG_tm_on, IMPACT, PM), decimals = 1, force_sign = T) %>%
		fmt_number(columns = c(GAMES, MIN), decimals = 0) %>%
		fmt_percent(columns = c(RECORD), decimals = 0) %>%
		gt_img_rows(columns = picture_url)

table1
gtsave_extra(table1, 'analyses/RAPM/plots/results1.png', selector = '.gt_table', vwidth = 2500, vheight = 2000, zoom = 4)

## ORAPM


df2 <-
	actions %>%
		inner_join(games, by = c('season_id', 'edition_id', 'game_id'), suffix = c('', '_game')) %>%
		group_by(game_id) %>%
		mutate(
			team = case_when(
				team_id == home_team_id ~ 'home',
				team_id == away_team_id ~ 'away',
				TRUE ~ NA_character_),
		    FT_PE = grepl('.*2/2.*', details) | grepl('.*3/3.*', details),
		    type = ifelse(type %in% c('FTA', 'FTM') & FT_PE, paste0(type, '_PE'), type),
		    home_difference = home_score - lag(home_score, 1, 0),
		    away_difference = away_score - lag(away_score, 1, 0),
			score_difference = abs(lag(home_score - away_score)),
			game_type = ifelse(type_game == 'PO', 2, 1),
			game_phase = case_when(
				period == 4 & score_difference >= 20 ~ 0.5,
				(period == 4 & remaining_period_time <= 180 & score_difference <= 5) | period >= 5 ~ 1.5,
				TRUE ~ 1
			)
		) %>%
	select(game_type, game_phase, game_id, type, team, home_difference, away_difference, h1:a5) %>%
	group_by(game_type, game_phase, h1, h2, h3, h4, h5, a1, a2, a3, a4, a5) %>%
	summarise(
		home_possessions = sum(team == 'home' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'home' & type == 'OREB'),
		away_possessions = sum(team == 'away' & (type %in% c('2FGA', '2FGM', '3FGA', '3FGM', 'TOV') | (type %in% c('FTA_PE', 'FTM_PE')))) - sum(team == 'away' & type == 'OREB'),
		# possessions = (home_possessions + away_possessions) / 2,
		home_points = sum(home_difference),
		away_points = sum(away_difference),
		home_offrtg = 100 * home_points / home_possessions,
		away_offrtg = 100 * away_points / away_possessions,
	) %>%
	ungroup() %>%
	pivot_longer(cols=c('home_possessions', 'away_possessions', 'away_points', 'home_points', 'home_offrtg', 'away_offrtg'), names_sep = "\\_", names_to = c("team", "stat")) %>%
	pivot_wider(names_from = 'stat') %>%
	filter(possessions >= 1) %>%
	mutate(stint = row_number()) %>%
	# select(possessions, stint, h1:a5, net_rating) %>%
	pivot_longer(cols = c(h1:a5), values_to = 'player_id', names_to = 'position') %>%
	mutate(
		side = ifelse(substr(team, 1, 1) == substr(position, 1, 1), 'offense', 'defense'),
		player_id = paste(player_id, side, sep=','),
		value = ifelse(side == 'offense', 1, -1)
	) %>%
	select(-position, -points, -side, -team) %>%
	filter(is.finite(offrtg)) %>%
	distinct() %>%
	pivot_wider(names_from = player_id, values_fill = 0) %>%
	select(-stint)

possessions <- df2$possessions
game_type <- df2$game_type
game_phase <- df2$game_phase
net_rating <- df2$offrtg

players_matrix <-
	df2 %>%
		select(-offrtg, -possessions, -game_type, -game_phase) %>%
		data.matrix()

weights <-
	df2 %>%
		mutate(weight = possessions * game_type * game_phase) %>%
		select(weight) %>%
		pull()

lambda <- cv.glmnet(players_matrix, net_rating, alpha=0, weights=weights, nfolds=5, type.measure = 'deviance')
lambda.min <- lambda$lambda.min

ridge <- glmnet(players_matrix, net_rating,
				family='gaussian',
				weights=weights, alpha=0,lambda=lambda.min) #run the ridge regression. x is the matrix of independent variables, Marg is the dependent variable, Poss are the weights. alpha=0 indicates the ridge penalty.

results2 <-
	coef(ridge,s=lambda.min) %>%
		as.matrix() %>%
		as.data.frame() %>%
		rownames_to_column('player_id') %>%
		as_tibble() %>%
		slice(-1) %>%
		rename(RAPM = s1) %>%
		mutate(
			side = sub(".*,", "", player_id),
			player_id = sub(",.*", "", player_id),
			stat = ifelse(side == 'offense', 'ORAPM', 'DRAPM')
		) %>%
		select(-side) %>%
		pivot_wider(names_from = 'stat', values_from = 'RAPM') %>%
		inner_join(stats) %>%
		inner_join(apm) %>%
		mutate(RAPM = ORAPM + DRAPM, MPG = MIN / GAMES, RECORD = W / GAMES) %>%
		select(player_id, GAMES, RECORD, MIN, POSS, MPG, NETRTG_tm_on, IMPACT, PM, APM, ORAPM, DRAPM, RAPM) %>%
		filter(MIN >= 1000) %>%
		arrange(desc(RAPM))

table2 <-
	results2 %>%
		filter(MIN >= 1000) %>%
		mutate(PM = PM / GAMES, MPG = MIN / GAMES) %>%
		inner_join(rosters) %>%
		group_by(player_id) %>%
		slice_max(order_by = season_id, n = 1) %>%
		ungroup() %>%
		arrange(desc(RAPM)) %>%
		mutate(rank = paste0(row_number(), '.'), player = paste0(substr(name, 1, 1), '. ', surname)) %>%
		select(rank, picture_url, player, GAMES, RECORD, MIN, MPG, PM, NETRTG_tm_on, IMPACT, APM, ORAPM, DRAPM, RAPM) %>%
		head(10) %>%
		gt() %>%
		cols_label(
			rank = '',
			picture_url = '',
			player = 'PLAYER',
			RECORD = 'RECORD',
			NETRTG_tm_on = 'NETRTG ON',
			PM = '+/-'
		) %>%
		fmt_number(columns = c(APM, RAPM, ORAPM, DRAPM), decimals = 2, force_sign = T) %>%
		fmt_number(columns = c(MPG), decimals = 1) %>%
		fmt_number(columns = c(NETRTG_tm_on, IMPACT, PM), decimals = 1, force_sign = T) %>%
		fmt_number(columns = c(GAMES, MIN), decimals = 0) %>%
		fmt_percent(columns = c(RECORD), decimals = 0) %>%
		tab_spanner(label = 'USAGE', columns = c('GAMES', 'RECORD', 'MIN', 'MPG')) %>%
		tab_spanner(label = 'PLUS MINUS', columns = c('PM', 'NETRTG_tm_on', 'IMPACT')) %>%
		tab_spanner(label = 'ADVANCED', columns = c('APM', 'ORAPM', 'DRAPM', 'RAPM')) %>%
		gt_img_rows(columns = picture_url)

table2
gtsave_extra(table2, 'analyses/RAPM/plots/results2.png', selector = '.gt_table', vwidth = 2500, vheight = 2000, zoom = 4)
