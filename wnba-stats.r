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

# --- 1. Pull standings -------------------------------------------------
# Change `year` as seasons roll over. most_recent_wnba_season() also works
# if you'd rather not hardcode it.
standings <- wehoop::espn_wnba_standings(year = 2026)

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

out <- list(
  east = to_rows(standings[standings$conference == "east", ]),
  west = to_rows(standings[standings$conference == "west", ]),
  updated_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)

# --- 4. Write JSON --------------------------------------------------------
write_json(out, "wnba-data.json", auto_unbox = TRUE, digits = 3, pretty = TRUE)

cat("Wrote wnba-data.json —", nrow(standings), "teams,", out$updated_at, "\n")