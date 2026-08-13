# nwsl-goals-added.R
#
# Pulls NWSL player goals-added (g+) data via itscalledsoccer, joins on
# player and team names (the raw API only gives IDs), and writes
# data/nwsl-goals-added.json for the player search + radar chart page.
#
# The `data` field from get_player_goals_added() is already a nested
# list-column of per-action-type rows (Dribbling, Fouling, Interrupting,
# Passing, Receiving, Shooting) — this matches the sample shape exactly,
# so it's written straight through with no unnest() needed.
#
# Requires: install.packages(c("itscalledsoccer", "dplyr", "jsonlite"))

library(itscalledsoccer)
library(dplyr)
library(jsonlite)

asa_client <- AmericanSoccerAnalysis$new()

goals_added <- asa_client$get_player_goals_added(leagues = "nwsl")
players     <- asa_client$get_players(leagues = "nwsl")
teams       <- asa_client$get_teams(leagues = "nwsl")

# get_player_goals_added() returns team_id as a list-column — a player who
# was traded mid-season has more than one team_id in that cell. Flatten to
# their most recent team (last element) so it's a plain character column
# and can actually be joined against team_lookup$team_id.
goals_added <- goals_added |>
  mutate(team_id = vapply(
    team_id,
    function(x) if (length(x)) as.character(x[[length(x)]]) else NA_character_,
    character(1)
  ))

player_lookup <- players |> select(player_id, player_name)
team_lookup   <- teams   |> select(team_id, team_name)

goals_added <- goals_added |>
  left_join(player_lookup, by = "player_id") |>
  left_join(team_lookup, by = "team_id") |>
  filter(!is.na(player_name)) # drop any IDs that didn't resolve to a name

dir.create("data", showWarnings = FALSE)
write_json(goals_added, "data/nwsl-goals-added.json", auto_unbox = TRUE, pretty = TRUE)

cat("Wrote data/nwsl-goals-added.json —", nrow(goals_added), "players\n")