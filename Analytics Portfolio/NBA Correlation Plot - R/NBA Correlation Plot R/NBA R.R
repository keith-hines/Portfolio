team_stats <- read.csv(file = 'Team_Stats_Historical.csv')
head(team_stats)
team_stats_edit <- team_stats[, -c(1:3)]
head(team_stats_edit)
team_stats_edit <- team_stats_edit[, -c(17:19)]
head(team_stats_edit)
team_stats_edit <- team_stats_edit[,-3]
head(team_stats_edit)
class(team_stats_edit$W)
team_stats_edit[1:15] <- lapply(team_stats_edit[1:15], as.numeric)
team_stats_cor <- cor(team_stats_edit)

library(corrplot)
corrplot(team_stats_cor, method = "square", type = "upper", tl.col = "black", tl.cex = 0.7, 
  col = colorRampPalette(c("red", "white", "blue"))(200))

         