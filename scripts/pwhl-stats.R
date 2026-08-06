# data from https://github.com/IsabelleLefebvre97/PWHL-Data-Reference

library(httr2)
library(jsonlite)
library(dplyr)

standings <- paste0(
  "https://lscluster.hockeytech.com/feed/index.php?",
  "feed=modulekit",
  "&view=statviewtype",
  "&stat=conference",
  "&type=standings",
  "&season_id=5",
  "&key=446521baf8c38984",
  "&client_code=pwhl"
)

league_leaders <- paste0(
  "https://lscluster.hockeytech.com/feed/index.php?",
  "feed=modulekit",
  "&view=combinedplayers",
  "&type=skaters",
  "&season_id=5",
  "&key=446521baf8c38984",
  "&client_code=pwhl"
)

leading_goalies <- paste0("https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=combinedplayers&type=goalies&season_id=5&key=446521baf8c38984&client_code=pwhl")

standings_resp <- request(standings) |>
  req_perform()

leaders_resp <- request(league_leaders) |>
  req_perform()

goalies_resp <- request(leading_goalies) |>
  req_perform()

standings <- resp_body_json(standings_resp, simplifyVector = TRUE)
leaders <- resp_body_json(leaders_resp, simplifyVector = TRUE)$SiteKit$Combinedplayers
goalie_leaders <- resp_body_json(goalies_resp, simplifyVector = TRUE)$SiteKit$Combinedplayers

goal_leader <- leaders$goals[1, ]
assist_leader <- leaders$assists[1, ]
points_leader <- leaders$points[1, ]
save_percentage <- goalie_leaders$save_percentage[1, ]
wins <- goalie_leaders$wins[1, ]
shutouts <- goalie_leaders$shutouts[1, ]

leaders <- list(
  list(
    name = goal_leader$name,
    team = goal_leader$team_name,
    stat = goal_leader$goals,
    label = "Goals"
  ),
  list(
    name = assist_leader$name,
    team = assist_leader$team_name,
    stat = assist_leader$assists,
    label = "Assists"
  ),
  list(
    name = points_leader$name,
    team = points_leader$team_name,
    stat = points_leader$points,
    label = "Points"
  ),
  list(
    name = save_percentage$name,
    team = save_percentage$team_name,
    stat = save_percentage$save_percentage,
    label = "Save Percentage"
  ),
  list(
    name = wins$name,
    team = wins$team_name,
    stat = wins$wins,
    label = "Wins"
  ),
  list(
    name = shutouts$name,
    team = shutouts$team_name,
    stat = shutouts$shutouts,
    label = "Shutouts"
  )
)

to_rows <- function(df) {
  df <- df[order(-df$win_percentage), ]
  lapply(seq_len(nrow(df)), function(i) {
    list(
      df$team_name[i],
      as.integer(df$wins[i]),
      as.integer(df$losses[i]),
      round(df$win_percentage[i], 3)
    )
  })
}

teams <- standings$SiteKit$Statviewtype |>
  bind_rows() |>
  filter(is.na(repeatheader)) |>
  mutate(
    wins = as.integer(wins),
    losses = as.integer(losses),
    points = as.integer(points),
    games_played = as.integer(games_played),
    win_percentage = as.numeric(win_percentage),
    rank = as.integer(rank)
  )

best <- teams |>
  slice_max(win_percentage, n = 1)

out <- list(
  facts = list(
    list(
      big = sprintf("%d–%d", best$wins, best$losses),
      lbl = paste("Best record (", best$team_name, ")", sep = "")
    ),
    list(
      big = as.character(nrow(teams)),
      lbl = "Teams in the league"
    )
  ),
  standings = to_rows(teams),
  leaders = leaders,
  updated_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
)

write_json(
  out,
  "data/pwhl-standings.json",
  auto_unbox = TRUE,
  pretty = TRUE
)
