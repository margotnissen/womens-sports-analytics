# export_wnba_standings.R
#
# Pulls current WNBA standings via wehoop (ESPN-backed) and writes
# wnba-data.json in the exact shape index.html expects:
#
#   { "east": [["Team Name", wins, losses, pct], ...],
#     "west": [...],
#     "updated_at": "2026-08-06 10:14 EDT" }
#
# Run this whenever you want to refresh the dashboard, then re-deploy
# (or re-open) index.html alongside the resulting wnba-data.json.
#
# Requires: install.packages(c("wehoop", "jsonlite"))

library(wehoop)
library(jsonlite)

# --- 1. Pull stats -------------------------------------------------
# Change `year` as seasons roll over. most_recent_wnba_season() also works
standings <- wehoop::espn_wnba_standings(year = most_recent_wnba_season())
#wnba_pbp <- wehoop::load_wnba_pbp()
#wnba_team_box <- wehoop::load_wnba_team_box()
player_box <- wehoop::load_wnba_player_box()
today_wnba <- espn_wnba_scoreboard(season = most_recent_wnba_season())
daily_scoreboard <- espn_wnba_scoreboard(season = most_recent_wnba_season())

# wehoop returns team names like "Los Angeles Sparks" — trim just in case
standings$team <- trimws(standings$team)

# --- 2. Assign conference -----------------------------------------------
# espn_wnba_standings() doesn't reliably include a conference column, so
# this is a small hardcoded lookup. Update it if the league realigns or
# expands (e.g. new franchises).
east_teams <- c(
  "Indiana Fever", "Atlanta Dream", "New York Liberty", "Washington Mystics",
  "Chicago Sky", "Toronto Tempo", "Connecticut Sun"
)
west_teams <- c(
  "Minnesota Lynx", "Las Vegas Aces", "Golden State Valkyries", "Dallas Wings",
  "Los Angeles Sparks", "Phoenix Mercury", "Portland Fire", "Seattle Storm"
)

standings$conference <- ifelse(
  standings$team %in% east_teams, "east",
  ifelse(standings$team %in% west_teams, "west", NA_character_)
)

if (any(is.na(standings$conference))) {
  warning(
    "Unmapped team(s) found — update east_teams/west_teams in this script: ",
    paste(unique(standings$team[is.na(standings$conference)]), collapse = ", ")
  )
}

# --- 3. Shape into [team, wins, losses, pct] rows, sorted by win% -------
to_rows <- function(df) {
  df <- df[order(-df$winpercent), ]
  lapply(seq_len(nrow(df)), function(i) {
    list(
      df$team[i],
      as.integer(df$wins[i]),
      as.integer(df$losses[i]),
      round(df$winpercent[i], 3)
    )
  })
}

best <- standings[which.max(standings$winpercent), ]

# --- Compute Stat Leaders ----------------
# load_wnba_player_box() defaults to the current season, one row per
# athlete per game. We aggregate to per-game averages and take the top
# player in each category, with a minimum games-played floor so an early
# hot streak in 2 games doesn't outrank a real season leader.
 
MIN_GAMES <- 10
 
season_stats <- aggregate(
  cbind(points, rebounds, assists) ~ athlete_display_name + team_short_display_name,
  data = player_box,
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), gp = length(x))
)
# `aggregate` with a matrix FUN packs mean/gp into sub-columns — unpack them
season_stats$ppg <- season_stats$points[, "mean"]
season_stats$gp  <- season_stats$points[, "gp"]
season_stats$rpg <- season_stats$rebounds[, "mean"]
season_stats$apg <- season_stats$assists[, "mean"]
season_stats <- season_stats[season_stats$gp >= MIN_GAMES, ]
 
top_by <- function(df, col, label, suffix = "") {
  row <- df[which.max(df[[col]]), ]
  list(
    name = row$athlete_display_name,
    team = row$team_short_display_name,
    stat = paste0(sprintf("%.1f", row[[col]]), suffix),
    label = label
  )
}
 
leaders <- list(
  top_by(season_stats, "ppg", "PPG"),
  top_by(season_stats, "rpg", "RPG"),
  top_by(season_stats, "apg", "APG")
)

# --- 4. Write JSON --------------------------------------------------------
out <- list(
  facts = list(
    list(big = sprintf("%d–%d", best$wins, best$losses), lbl = paste("Best record (", best$team, ")", sep = "")),
    list(big = as.character(nrow(standings)), lbl = "Teams in the league")
  ),
  east = to_rows(standings[standings$conference == "east", ]),
  west = to_rows(standings[standings$conference == "west", ]),
  leaders = leaders,
  updated_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)

jsonlite::write_json(
  out,
  "data/wnba-data.json",
  auto_unbox = TRUE,
  pretty = TRUE
)

cat("Wrote wnba-data.json —", nrow(standings), "teams,", out$updated_at, "\n")