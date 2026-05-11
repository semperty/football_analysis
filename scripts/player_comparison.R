# player_comparison.R
# Compares Dusan Vlahovic and Gonzalo Higuaín during their time at Juventus
# Metrics:
#   1. Bunched goals (2+ goals in same game vs spread across games)
#   2. Goals vs end-of-season table position groups
#   3. Decisive goals (last goal that changed points outcome and wasn't cancelled)

library(pacman)
p_load(dplyr, purrr, arrow, here, stringr)

# ── Config ─────────────────────────────────────────────────────────────────────

PLAYERS <- c("Dusan Vlahovic", "Gonzalo Higuaín")
CLUB    <- "Juventus"
LEAGUE  <- "serie_a"

VLAHOVIC_SEASONS <- c(2021, 2022, 2023, 2024, 2025)
HIGUAIN_SEASONS  <- c(2016, 2017, 2019)

group_order <- c(
  "Champions League (1-4)",
  "Europa League (5-6)",
  "Top Half Midtable (7-10)",
  "Bottom Half Midtable (11-17)",
  "Relegation Zone (18-20)"
)

# ── Helpers ────────────────────────────────────────────────────────────────────

#' Parse Understat minute strings to numeric preserving stoppage time order
#' "45+2" -> 45.0002, "90+5" -> 90.0005, "46" -> 46
#' Uses /10000 so no stoppage time amount ever rounds up to the next minute
#' @param x Character vector of minute strings
parse_minute <- function(x) {
  x <- as.character(x)
  case_when(
    str_detect(x, "\\+") ~ {
      parts <- str_split_fixed(x, "\\+", 2)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 10000
    },
    TRUE ~ as.numeric(x)
  )
}

#' Compute points state for Juventus given current score
#' @param juve_goals Goals scored by Juventus so far
#' @param opp_goals  Goals scored by opponent so far
pts_state <- function(juve_goals, opp_goals) {
  case_when(
    juve_goals > opp_goals  ~ 3L,
    juve_goals == opp_goals ~ 1L,
    TRUE                    ~ 0L
  )
}

#' Table position group label
#' @param pos Final league position integer
group_label <- function(pos) {
  case_when(
    pos <= 4  ~ "Champions League (1-4)",
    pos <= 6  ~ "Europa League (5-6)",
    pos <= 10 ~ "Top Half Midtable (7-10)",
    pos <= 17 ~ "Bottom Half Midtable (11-17)",
    TRUE      ~ "Relegation Zone (18-20)"
  )
}

# ── Load shot data ─────────────────────────────────────────────────────────────

#' Load all Serie A shot parquets for specified seasons
#' @param seasons Integer vector of season start years
load_shots <- function(seasons) {
  map_dfr(seasons, function(s) {
    path <- here("Data_Shot", paste0("shots_", LEAGUE, "_", s, ".parquet"))
    if (!file.exists(path)) {
      print(paste("Missing:", basename(path)))
      return(NULL)
    }
    read_parquet(path) |>
      mutate(
        season = s,
        result = as.character(result),
        side   = as.character(side),
        minute = as.character(minute)
      )
  })
}

# ── Filter to player shots for Juventus ───────────────────────────────────────

#' Filter shots to only those taken by a player while playing FOR Juventus
#' Uses side + team columns to exclude goals scored against Juventus
#' @param shots  Full shot data frame
#' @param player Player name string
filter_player_juve <- function(shots, player) {
  shots |>
    filter(
      player == !!player,
      case_when(
        side == "h" ~ h_team == CLUB,
        side == "a" ~ a_team == CLUB,
        TRUE        ~ FALSE
      )
    ) |>
    mutate(
      xG       = as.numeric(xG),
      is_goal  = result == "Goal",
      opp_team = case_when(side == "h" ~ a_team, TRUE ~ h_team)
    )
}

# ── 1. Bunched goals ───────────────────────────────────────────────────────────

#' Compute bunched vs spread goals for a player
#' Bunched = 2+ goals scored in the same match
#' @param player_shots Filtered shot data for one player
compute_bunched_goals <- function(player_shots) {
  goals_only <- player_shots |> filter(is_goal)
  
  per_match <- goals_only |>
    group_by(match_id) |>
    summarise(goals_in_match = n(), .groups = "drop")
  
  total_games   <- n_distinct(player_shots$match_id)
  scoring_games <- nrow(per_match)
  bunched_games <- sum(per_match$goals_in_match >= 2)
  spread_games  <- sum(per_match$goals_in_match == 1)
  total_goals   <- sum(per_match$goals_in_match)
  bunched_goals <- sum(per_match$goals_in_match[per_match$goals_in_match >= 2])
  spread_goals  <- sum(per_match$goals_in_match[per_match$goals_in_match == 1])
  
  tibble(
    total_games       = total_games,
    scoring_games     = scoring_games,
    total_goals       = total_goals,
    bunched_games     = bunched_games,
    spread_games      = spread_games,
    bunched_goals     = bunched_goals,
    spread_goals      = spread_goals,
    pct_scoring_games = round(scoring_games / total_games * 100, 1),
    pct_bunched_games = round(bunched_games / scoring_games * 100, 1),
    goals_per_game    = round(total_goals / total_games, 3),
    bunched_goals_pct = round(bunched_goals / total_goals * 100, 1),
    spread_goals_pct  = round(spread_goals / total_goals * 100, 1)
  )
}

# ── 2. Goals vs table position ─────────────────────────────────────────────────

#' Load end-of-season standings for a given Serie A season
#' @param season Season start year
load_final_standings <- function(season) {
  path <- here("Data_Team", paste0("team_matchday_history_", season, ".parquet"))
  if (!file.exists(path)) {
    print(paste("No matchday history for season", season))
    return(NULL)
  }
  
  read_parquet(path) |>
    filter(league == "Serie_A") |>
    group_by(team) |>
    filter(match_date == max(match_date)) |>
    ungroup() |>
    arrange(desc(cum_pts), desc(cum_gd), desc(cum_scored)) |>
    mutate(
      final_position = row_number(),
      opp_group      = group_label(final_position)
    ) |>
    select(team, final_position, opp_group)
}

#' Compute goals by opponent table group for a player
#' Reports pct of goals vs pct of games per group and their ratio
#' @param player_shots Filtered shot data for one player
#' @param seasons      Seasons to load standings for
compute_goals_by_group <- function(player_shots, seasons) {
  standings <- map_dfr(seasons, function(s) {
    st <- load_final_standings(s)
    if (!is.null(st)) mutate(st, season = s)
  })
  
  if (nrow(standings) == 0) return(NULL)
  
  goals_only <- player_shots |>
    filter(is_goal) |>
    left_join(standings, by = c("opp_team" = "team", "season"))
  
  games_vs_group <- player_shots |>
    left_join(standings, by = c("opp_team" = "team", "season")) |>
    filter(!is.na(opp_group)) |>
    distinct(match_id, opp_group) |>
    count(opp_group, name = "games_vs_group")
  
  total_games <- n_distinct(player_shots$match_id)
  total_goals <- sum(player_shots$is_goal)
  
  goals_only |>
    filter(!is.na(opp_group)) |>
    group_by(opp_group) |>
    summarise(goals = n(), .groups = "drop") |>
    left_join(games_vs_group, by = "opp_group") |>
    mutate(
      opp_group         = factor(opp_group, levels = group_order),
      pct_of_goals      = round(goals / total_goals * 100, 1),
      pct_of_games      = round(games_vs_group / total_games * 100, 1),
      goals_per_game    = round(goals / games_vs_group, 3),
      goals_games_ratio = round(pct_of_goals / pct_of_games, 2)
    ) |>
    arrange(opp_group)
}

# ── 3. Decisive goals ──────────────────────────────────────────────────────────

#' Classify which goals by a player in one match are decisive
#' A goal is decisive if:
#'   1. It changed the points state upward at the moment it was scored
#'   2. The points state never dropped below that level again after
#' @param match_goals Data frame of all goals in one match
#' @param player      Player name to check
#' @param juve_side   "h" or "a" — which side Juventus are on
classify_decisive <- function(match_goals, player, juve_side) {
  if (nrow(match_goals) == 0) return(tibble())
  
  match_goals <- match_goals |>
    arrange(parse_minute(minute)) |>
    mutate(
      juve_scored = (side == juve_side),
      juve_cum    = cumsum(juve_scored),
      opp_cum     = cumsum(!juve_scored),
      pts_after   = pts_state(juve_cum, opp_cum),
      pts_before  = lag(pts_after, default = 1L)  # game starts at draw
    )
  
  n <- nrow(match_goals)
  
  map_dfr(seq_len(n), function(i) {
    row <- match_goals[i, ]
    
    # Only consider goals by our player for Juventus
    if (!row$juve_scored || row$player != player) return(NULL)
    
    # Did this goal change the points state upward?
    if (row$pts_after <= row$pts_before) return(NULL)
    
    pts_level <- row$pts_after
    
    # Check all subsequent events — did pts ever drop below pts_level?
    cancelled <- if (i < n) {
      any(match_goals$pts_after[(i + 1):n] < pts_level)
    } else {
      FALSE
    }
    
    tibble(
      match_id   = row$match_id,
      player     = row$player,
      minute     = row$minute,
      pts_before = row$pts_before,
      pts_after  = row$pts_after,
      decisive   = !cancelled
    )
  })
}

#' Compute decisive goals for a player across all their scoring matches
#' @param player_shots Filtered shot data for one player
#' @param all_shots    Full shot data (for full match goal context)
compute_decisive_goals <- function(player_shots, all_shots) {
  player    <- unique(player_shots$player)
  match_ids <- unique(player_shots$match_id[player_shots$is_goal])
  juve_side <- player_shots |> distinct(match_id, side)
  
  if (length(match_ids) == 0) return(tibble())
  
  map_dfr(match_ids, function(mid) {
    js <- juve_side$side[juve_side$match_id == mid]
    if (length(js) == 0) return(NULL)
    
    match_goals <- all_shots |>
      filter(match_id == mid, result == "Goal")
    
    if (nrow(match_goals) == 0) return(NULL)
    
    classify_decisive(match_goals, player, js)
  })
}

#' Summarise decisive goal stats for a player
#' @param player_shots Filtered shot data for one player
#' @param decisive_df  Output of compute_decisive_goals()
summarise_decisive <- function(player_shots, decisive_df) {
  total_games    <- n_distinct(player_shots$match_id)
  total_goals    <- sum(player_shots$is_goal)
  decisive_goals <- sum(decisive_df$decisive, na.rm = TRUE)
  decisive_games <- n_distinct(decisive_df$match_id[decisive_df$decisive])
  
  tibble(
    total_games        = total_games,
    total_goals        = total_goals,
    decisive_goals     = decisive_goals,
    decisive_games     = decisive_games,
    pct_goals_decisive = round(decisive_goals / total_goals * 100, 1),
    pct_games_decisive = round(decisive_games / total_games * 100, 1),
    decisive_per_game  = round(decisive_goals / total_games, 3)
  )
}

# ── Run analysis ───────────────────────────────────────────────────────────────

print("Loading shot data...")
all_shots_v <- load_shots(VLAHOVIC_SEASONS)
all_shots_h <- load_shots(HIGUAIN_SEASONS)

print("Filtering to Juventus shots...")
vlahovic <- filter_player_juve(all_shots_v, "Dusan Vlahovic")
higuain  <- filter_player_juve(all_shots_h, "Gonzalo Higuaín")

print(paste("Vlahovic — shots:", nrow(vlahovic), "| goals:", sum(vlahovic$is_goal)))
print(paste("Higuaín  — shots:", nrow(higuain),  "| goals:", sum(higuain$is_goal)))

# ── 1. Bunched goals ───────────────────────────────────────────────────────────

print("Computing bunched goals...")
bunched_comparison <- bind_rows(
  compute_bunched_goals(vlahovic) |> mutate(player = "Dusan Vlahovic"),
  compute_bunched_goals(higuain)  |> mutate(player = "Gonzalo Higuaín")
) |> select(player, everything())

print("── Bunched Goals ──")
print(bunched_comparison)

# ── 2. Goals by table group ────────────────────────────────────────────────────

print("Computing goals by table group...")
group_comparison <- bind_rows(
  compute_goals_by_group(vlahovic, VLAHOVIC_SEASONS) |> mutate(player = "Dusan Vlahovic"),
  compute_goals_by_group(higuain,  HIGUAIN_SEASONS)  |> mutate(player = "Gonzalo Higuaín")
) |> select(player, opp_group, goals, games_vs_group,
            pct_of_goals, pct_of_games, goals_per_game, goals_games_ratio)

print("── Goals by Table Group ──")
print(group_comparison)

# ── 3. Decisive goals ──────────────────────────────────────────────────────────

print("Computing decisive goals...")
decisive_v <- compute_decisive_goals(vlahovic, all_shots_v)
decisive_h <- compute_decisive_goals(higuain,  all_shots_h)

decisive_comparison <- bind_rows(
  summarise_decisive(vlahovic, decisive_v) |> mutate(player = "Dusan Vlahovic"),
  summarise_decisive(higuain,  decisive_h) |> mutate(player = "Gonzalo Higuaín")
) |> select(player, everything())

print("── Decisive Goals ──")
print(decisive_comparison)

# ── Save ───────────────────────────────────────────────────────────────────────

dir.create(here("Data_Player"), showWarnings = FALSE)

write_parquet(bunched_comparison,  here("Data_Player", "comparison_bunched.parquet"))
write_parquet(group_comparison,    here("Data_Player", "comparison_by_group.parquet"))
write_parquet(decisive_comparison, here("Data_Player", "comparison_decisive.parquet"))

write_parquet(
  bind_rows(
    mutate(decisive_v, player = "Dusan Vlahovic"),
    mutate(decisive_h, player = "Gonzalo Higuaín")
  ),
  here("Data_Player", "decisive_goals_detail.parquet")
)

print("Saved to Data_Player/")
