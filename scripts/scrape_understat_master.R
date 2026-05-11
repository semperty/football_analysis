# scrape_understat_master.R
# Scrapes shots, team stats, and player stats from Understat
# Tracks progress at match level — safe to re-run anytime to pick up new matches
# Appends new shots to existing parquets rather than overwriting
# Team/player stats only re-scraped when new matches are found

library(pacman)
p_load(httr, rvest, jsonlite, stringr, stringi, dplyr, purrr, arrow, here)

# ── Config ─────────────────────────────────────────────────────────────────────

LEAGUES        <- c("EPL", "La_liga", "Bundesliga", "Serie_A", "Ligue_1")
SEASONS        <- 2025        # change to 2025:2014 for full history
SLEEP_SEC      <- 2.5
MATCH_LOG_PATH <- here("scraped_matches.rds")
FAILED_PATH    <- here("failed_matches.rds")

dir.create(here("Data_Shot"),   showWarnings = FALSE)
dir.create(here("Data_Team"),   showWarnings = FALSE)
dir.create(here("Data_Player"), showWarnings = FALSE)

# ── Match log helpers ──────────────────────────────────────────────────────────

#' Load existing match log or initialise empty
load_match_log <- function() {
  if (file.exists(MATCH_LOG_PATH)) {
    readRDS(MATCH_LOG_PATH)
  } else {
    tibble(
      league     = character(),
      season     = integer(),
      match_id   = character(),
      scraped_at = as.POSIXct(character())
    )
  }
}

#' Get already-scraped match IDs for a given league/season
#' @param log    Match log tibble
#' @param league League string
#' @param season Season start year
get_scraped_ids <- function(log, league, season) {
  log |>
    filter(league == !!league, season == !!season) |>
    pull(match_id)
}

#' Append newly scraped match IDs to the log and save
#' @param log       Match log tibble
#' @param league    League string
#' @param season    Season start year
#' @param match_ids Vector of match ID strings
mark_matches_done <- function(log, league, season, match_ids) {
  updated <- bind_rows(
    log,
    tibble(
      league     = league,
      season     = season,
      match_id   = as.character(match_ids),
      scraped_at = Sys.time()
    )
  )
  saveRDS(updated, MATCH_LOG_PATH)
  updated
}

# ── Failed match log helpers ───────────────────────────────────────────────────

#' Load existing failed match log or initialise empty
load_failed <- function() {
  if (file.exists(FAILED_PATH)) {
    readRDS(FAILED_PATH)
  } else {
    tibble(
      league    = character(),
      season    = integer(),
      match_id  = character(),
      failed_at = as.POSIXct(character())
    )
  }
}

#' Append a failed match to the failed log and save
#' @param failed   Failed matches tibble
#' @param league   League string
#' @param season   Season start year
#' @param match_id Understat match ID string
log_failed <- function(failed, league, season, match_id) {
  updated <- bind_rows(
    failed,
    tibble(
      league    = league,
      season    = season,
      match_id  = as.character(match_id),
      failed_at = Sys.time()
    )
  )
  saveRDS(updated, FAILED_PATH)
  updated
}

# ── Session helpers ────────────────────────────────────────────────────────────

#' Open an httr session on a given URL
#' @param url Target URL string
open_session <- function(url) {
  session(url, add_headers(`User-Agent` = "Mozilla/5.0"))
}

#' Perform an authenticated Ajax GET via an existing session
#' @param sess    Active httr session
#' @param url     API endpoint URL
#' @param referer Referer header value
ajax_get <- function(sess, url, referer) {
  resp <- session_jump_to(
    sess, url,
    add_headers(
      `Referer`          = referer,
      `X-Requested-With` = "XMLHttpRequest",
      `Accept`           = "application/json, text/javascript, */*; q=0.01"
    )
  )
  content(resp$response, "text") |> fromJSON()
}

# ── League data ────────────────────────────────────────────────────────────────

#' Fetch league-level data from getLeagueData API with retry on block
#' @param league League string
#' @param season Season start year
get_league_data <- function(league, season) {
  base_url <- paste0("https://understat.com/league/", league, "/", season)
  api_url  <- paste0("https://understat.com/getLeagueData/", league, "/", season)
  
  for (attempt in 1:3) {
    tryCatch({
      sess <- open_session(base_url)
      data <- ajax_get(sess, api_url, base_url)
      return(data)
    }, error = function(e) {
      print(paste("  Attempt", attempt, "failed:", e$message))
      Sys.sleep(10 * attempt)
    })
  }
  NULL
}

#' Extract completed match IDs from league data
#' @param league_data List returned by get_league_data()
get_completed_ids <- function(league_data) {
  dates     <- league_data$dates
  completed <- dates[dates$isResult == TRUE, ]
  as.character(completed$id)
}

# ── Shot scraper ───────────────────────────────────────────────────────────────

#' Scrape shot-level data for a single match via getMatchData API
#' Includes NULL/type checks to handle malformed responses
#' @param sess     Active httr session
#' @param match_id Understat match ID
get_match_shots <- function(sess, match_id) {
  data <- ajax_get(
    sess,
    paste0("https://understat.com/getMatchData/", match_id),
    paste0("https://understat.com/match/", match_id)
  )
  
  if (is.null(data$shots)) return(NULL)
  
  home <- tryCatch(
    if (!is.null(data$shots$h) && is.data.frame(data$shots$h) && nrow(data$shots$h) > 0)
      mutate(as.data.frame(data$shots$h), side = "h") else NULL,
    error = function(e) NULL
  )
  away <- tryCatch(
    if (!is.null(data$shots$a) && is.data.frame(data$shots$a) && nrow(data$shots$a) > 0)
      mutate(as.data.frame(data$shots$a), side = "a") else NULL,
    error = function(e) NULL
  )
  
  bind_rows(home, away) |> mutate(match_id = match_id)
}

# ── Team and player stats ──────────────────────────────────────────────────────

#' Parse a statistics sub-list into a tidy data frame
#' @param stats_list Named list of category stats
#' @param team       Team name string
#' @param group_var  Column name for the grouping variable
parse_stats_group <- function(stats_list, team, group_var) {
  map_dfr(names(stats_list), function(category) {
    as.data.frame(stats_list[[category]]) |>
      mutate(team = team, !!group_var := category)
  })
}

#' Standardize column names and compute derived stats
#' @param df        Raw stats data frame
#' @param group_var Grouping column name string
clean_stats <- function(df, group_var) {
  df |>
    rename(
      sh  = shots,
      g   = goals,
      sha = against.shots,
      ga  = against.goals,
      xg  = xG,
      xga = against.xG
    ) |>
    mutate(
      sh     = as.numeric(sh),
      g      = as.numeric(g),
      sha    = as.numeric(sha),
      ga     = as.numeric(ga),
      xg     = round(as.numeric(xg), 2),
      xga    = round(as.numeric(xga), 2),
      xgd    = round(xg - xga, 2),
      xg_sh  = round(xg / sh,  2),
      xga_sh = round(xga / sha, 2)
    ) |>
    select(team, !!group_var, sh, g, sha, ga, xg, xga, xgd, xg_sh, xga_sh)
}

#' Fetch and parse all stat breakdowns and player records for one team
#' @param team   Understat team name
#' @param season Season start year
get_team_stats <- function(team, season) {
  team_url <- str_replace_all(team, " ", "_")
  base_url <- paste0("https://understat.com/team/", team_url, "/", season)
  api_url  <- paste0("https://understat.com/getTeamData/", team_url, "/", season)
  
  sess  <- open_session(base_url)
  data  <- ajax_get(sess, api_url, base_url)
  stats <- data$statistics
  
  players_df <- if (!is.null(data$players) && nrow(as.data.frame(data$players)) > 0)
    mutate(as.data.frame(data$players), team = team)
  else
    NULL
  
  list(
    situation    = parse_stats_group(stats$situation,   team, "situation"),
    shot_zone    = parse_stats_group(stats$shotZone,    team, "shot_zone"),
    attack_speed = parse_stats_group(stats$attackSpeed, team, "attack_speed"),
    result       = parse_stats_group(stats$result,      team, "result"),
    players      = players_df
  )
}

# ── Parquet append helper ──────────────────────────────────────────────────────

#' Append new rows to an existing parquet or create it if it doesn't exist
#' Forces arrow to release memory map before writing to avoid Windows error 1224
#' @param new_data Data frame of new rows to append
#' @param path     Path to the parquet file
append_parquet <- function(new_data, path) {
  if (file.exists(path)) {
    # Read and immediately force into plain R memory — releases arrow memory map
    existing <- read_parquet(path) |>
      as.data.frame() |>
      collect()
    
    # Force garbage collection to release any remaining file handles
    gc()
    
    combined <- bind_rows(existing, new_data) |> as.data.frame()
    
    # Write to tmp first, then rename — avoids overwriting a locked file
    tmp_path <- paste0(path, ".tmp")
    write_parquet(combined, tmp_path)
    
    # Brief pause to let Windows fully release the handle before rename
    Sys.sleep(0.5)
    file.rename(tmp_path, path)
  } else {
    write_parquet(as.data.frame(new_data), path)
  }
}

# ── Per league/season scrape ───────────────────────────────────────────────────

#' Scrape all new data for one league/season and append to existing files
#' @param league League string
#' @param season Season start year
#' @param log    Match log tibble
#' @param failed Failed matches tibble
scrape_one <- function(league, season, log, failed) {
  slug <- paste(league, season, sep = "_")
  print(paste("── Checking", slug))
  
  # ── League data
  league_data <- tryCatch(
    get_league_data(league, season),
    error = function(e) {
      print(paste("  ERROR fetching league data:", e$message))
      NULL
    }
  )
  
  if (is.null(league_data)) return(list(log = log, failed = failed))
  
  # ── Identify new matches
  all_completed <- get_completed_ids(league_data)
  already_done  <- get_scraped_ids(log, league, season)
  new_match_ids <- setdiff(all_completed, already_done)
  
  print(paste(" ", length(all_completed), "completed matches total,",
              length(new_match_ids), "new to scrape"))
  
  if (length(new_match_ids) == 0) {
    print("  No new matches — skipping")
    return(list(log = log, failed = failed))
  }
  
  # ── Scrape new shots
  shot_path   <- here("Data_Shot", paste0("shots_", tolower(league), "_", season, ".parquet"))
  sess        <- open_session(paste0("https://understat.com/league/", league, "/", season))
  new_shots   <- vector("list", length(new_match_ids))
  success_ids <- character(0)
  
  for (i in seq_along(new_match_ids)) {
    mid <- new_match_ids[i]
    
    # Refresh session every 50 matches to avoid rate limit cascades
    if (i %% 50 == 1 && i > 1) {
      print("  Refreshing session...")
      Sys.sleep(10)
      sess <- open_session(paste0("https://understat.com/league/", league, "/", season))
    }
    
    new_shots[[i]] <- tryCatch({
      shots <- get_match_shots(sess, mid)
      success_ids <- c(success_ids, mid)
      shots
    },
    error = function(e) {
      print(paste("  FAILED match", mid, ":", e$message))
      failed <<- log_failed(failed, league, season, mid)
      NULL
    })
    
    if (i %% 50 == 0) {
      print(paste(sprintf("  [%d/%d]", i, length(new_match_ids)), "new matches scraped"))
    }
    
    Sys.sleep(SLEEP_SEC)
  }
  
  shots_df <- bind_rows(new_shots)
  
  if (nrow(shots_df) > 0) {
    append_parquet(shots_df, shot_path)
    print(paste(" ", nrow(shots_df), "new shots appended to", basename(shot_path)))
  }
  
  # Update match log with successfully scraped IDs
  if (length(success_ids) > 0) {
    log <- mark_matches_done(log, league, season, success_ids)
  }
  
  # ── Team and player stats (only since new matches were found)
  team_names   <- map_chr(league_data$teams, ~ .x$title)
  
  all_situation    <- vector("list", length(team_names))
  all_shot_zone    <- vector("list", length(team_names))
  all_attack_speed <- vector("list", length(team_names))
  all_result       <- vector("list", length(team_names))
  all_players      <- vector("list", length(team_names))
  
  for (i in seq_along(team_names)) {
    team <- team_names[i]
    
    result <- tryCatch(
      get_team_stats(team, season),
      error = function(e) {
        print(paste("  FAILED team stats:", team, ":", e$message))
        NULL
      }
    )
    
    if (!is.null(result)) {
      all_situation[[i]]    <- result$situation
      all_shot_zone[[i]]    <- result$shot_zone
      all_attack_speed[[i]] <- result$attack_speed
      all_result[[i]]       <- result$result
      all_players[[i]]      <- result$players
    }
    
    Sys.sleep(SLEEP_SEC)
  }
  
  # Overwrite team stats (always reflects current season totals)
  write_parquet(
    bind_rows(all_situation)    |> clean_stats("situation"),
    here("Data_Team", paste0("team_by_situation_",    tolower(league), "_", season, ".parquet"))
  )
  write_parquet(
    bind_rows(all_shot_zone)    |> clean_stats("shot_zone"),
    here("Data_Team", paste0("team_by_shot_zone_",    tolower(league), "_", season, ".parquet"))
  )
  write_parquet(
    bind_rows(all_attack_speed) |> clean_stats("attack_speed"),
    here("Data_Team", paste0("team_by_attack_speed_", tolower(league), "_", season, ".parquet"))
  )
  write_parquet(
    bind_rows(all_result)       |> clean_stats("result"),
    here("Data_Team", paste0("team_by_result_",       tolower(league), "_", season, ".parquet"))
  )
  print("  Team stats updated")
  
  players_df <- bind_rows(all_players) |>
    mutate(league = league, season = season)
  
  if (nrow(players_df) > 0) {
    write_parquet(
      players_df,
      here("Data_Player", paste0("players_", tolower(league), "_", season, ".parquet"))
    )
    print(paste(" ", nrow(players_df), "player records updated"))
  } else {
    print("  WARNING: no player records saved — check getTeamData response")
  }
  
  list(log = log, failed = failed)
}

# ── Master loop ────────────────────────────────────────────────────────────────

#' Loop through all league/season combinations, only scraping new matches
run_master_scrape <- function() {
  log    <- load_match_log()
  failed <- load_failed()
  
  total <- length(SEASONS) * length(LEAGUES)
  done  <- 0
  
  for (season in SEASONS) {
    for (league in LEAGUES) {
      result <- scrape_one(league, season, log, failed)
      log    <- result$log
      failed <- result$failed
      done   <- done + 1
      
      print(paste(sprintf("[%d/%d]", done, total), league, season, "complete"))
      Sys.sleep(SLEEP_SEC * 2)
    }
  }
  
  print(paste("Run complete.", done, "league/season pairs checked."))
  
  if (nrow(failed) > 0) {
    print(paste(nrow(failed), "total failed matches in log —", FAILED_PATH))
  }
}

# ── Run ────────────────────────────────────────────────────────────────────────

run_master_scrape()
