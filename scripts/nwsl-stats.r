# nwsl-stats.R
#
# Pulls NWSL teams + games via itscalledsoccer (American Soccer Analysis),
# attaches team names to raw game rows, and aggregates completed matches
# into a standings table shaped for index.html:
#
#   [[rank, team, gp, w, d, l, "+/-gd", pts], ...]
#
# Requires: install.packages(c("itscalledsoccer", "dplyr", "jsonlite"))

library(itscalledsoccer)
library(dplyr)
library(jsonlite)

asa_client <- AmericanSoccerAnalysis$new()

nwsl_teams <- asa_client$get_teams(leagues = "nwsl")
nwsl_games <- asa_client$get_games(leagues = "nwsl", seasons = "2026")

# --- Team ID -> name lookup, single table reused for both sides -------
team_lookup <- nwsl_teams |>
  select(team_id, team_name)

# --- Attach readable names directly onto the game rows ----------------
# (this replaces your two separate home_teams/away_teams lookups — one
# lookup table, joined twice, renaming the resulting column each time)
nwsl_games <- nwsl_games |>
  left_join(team_lookup, by = c("home_team_id" = "team_id")) |>
  rename(home_team_name = team_name) |>
  left_join(team_lookup, by = c("away_team_id" = "team_id")) |>
  rename(away_team_name = team_name)

# IMPORTANT: nothing below this line should reassign nwsl_games or
# standings back to a raw asa_client$get_games() call — that's what
# was silently discarding the joins in the original script.

# --- Build a standings table from completed games ----------------------
# get_games() is one row per match — a standings table (W/D/L/points) has
# to be aggregated from those results, not just read off the raw rows.
completed <- nwsl_games |> filter(status == "FullTime")

home_rows <- completed |>
  transmute(
    team = home_team_name,
    gf = home_score,
    ga = away_score,
    result = case_when(
      home_score > away_score ~ "W",
      home_score < away_score ~ "L",
      TRUE ~ "D"
    )
  )

away_rows <- completed |>
  transmute(
    team = away_team_name,
    gf = away_score,
    ga = home_score,
    result = case_when(
      away_score > home_score ~ "W",
      away_score < home_score ~ "L",
      TRUE ~ "D"
    )
  )

standings <- bind_rows(home_rows, away_rows) |>
  group_by(team) |>
  summarise(
    gp = n(),
    w = sum(result == "W"),
    d = sum(result == "D"),
    l = sum(result == "L"),
    gf = sum(gf),
    ga = sum(ga),
    gd = gf - ga,
    pts = w * 3 + d,
    .groups = "drop"
  ) |>
  arrange(desc(pts), desc(gd))

# --- Last-5 form per team -----------------------
# Same home/away result logic as the standings above, but kept per-game
# (with date) instead of aggregated, so we can take each team's most
# recent 5 completed matches and read off W/D/L in chronological order.
home_results <- completed |>
  transmute(
    team = home_team_name,
    date = date_time_utc,
    result = case_when(
      home_score > away_score ~ "W",
      home_score < away_score ~ "L",
      TRUE ~ "D"
    )
  )

away_results <- completed |>
  transmute(
    team = away_team_name,
    date = date_time_utc,
    result = case_when(
      away_score > home_score ~ "W",
      away_score < home_score ~ "L",
      TRUE ~ "D"
    )
  )

form_by_team <- bind_rows(home_results, away_results) |>
  arrange(team, date) |>
  group_by(team) |>
  slice_tail(n = 5) |>
  summarise(form = list(result), .groups = "drop")

form_lookup <- setNames(form_by_team$form, form_by_team$team)

# --- Shape into named row objects: rank, team, games_played, wins, ------
#        draws, losses, point_diff, points, last_5
standings_rows <- lapply(seq_len(nrow(standings)), function(i) {
  row <- standings[i, ]
  team_form <- form_lookup[[row$team]]
  list(
    rank = i,
    team = row$team,
    games_played = row$gp,
    wins = row$w,
    draws = row$d,
    losses = row$l,
    point_diff = paste0(if (row$gd > 0) "+" else "", row$gd),
    points = row$pts,
    last_5 = if (is.null(team_form)) list() else as.list(team_form)
  )
})

# --- Write JSON ---------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write_json(nwsl_teams, "data/nwsl-teams.json", auto_unbox = TRUE, pretty = TRUE)
write_json(standings_rows, "data/nwsl-standings.json", auto_unbox = TRUE, pretty = TRUE)

cat("Wrote data/nwsl-standings.json —", nrow(standings), "teams,",
    nrow(completed), "completed games\n")