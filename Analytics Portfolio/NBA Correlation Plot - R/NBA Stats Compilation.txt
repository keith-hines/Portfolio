/* Compiling home and away team stats into the scores table. Create new database and import flat files Team_Stats_Historical.csv and Historical_Scores.csv*/
WITH
	scores_stats AS (
	SELECT *
	FROM scores
	),	
	away_net_rtg AS (
	SELECT TEAM_ID, W_PERCENT, NETRTG, AST_TO, REB_PERCENT, TOV_PERCENT, TS_PERCENT
	FROM team_stats
	),
	home_ret_rtg AS (
	SELECT TEAM_ID, W_PERCENT, NETRTG, AST_TO, REB_PERCENT, TOV_PERCENT, TS_PERCENT
	FROM team_stats
	)
SELECT s.*, a.W_PERCENT AS away_w_perc, a.NETRTG AS away_netrtg, a.AST_TO as away_ast_to, a.REB_PERCENT as away_reb_perc, a.TOV_PERCENT AS away_tov_perc, a.TS_PERCENT as away_ts_perc, h.W_PERCENT AS home_w_perc, h.NETRTG AS home_netrtg, h.AST_TO as home_ast_to, h.REB_PERCENT as home_reb_perc, h.TOV_PERCENT AS home_tov_perc, h.TS_PERCENT as home_ts_perc
FROM scores_stats s
JOIN away_net_rtg a
ON s.Away_team_id = a.TEAM_ID
FULL JOIN home_ret_rtg h
ON s.Home_team_id = h.TEAM_ID;


