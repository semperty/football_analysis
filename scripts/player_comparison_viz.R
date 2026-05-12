# viz_player_comparison.R
# Visualization script for Vlahovic vs Higuain comparison article
# Includes:
#   1. Shot maps (all shots + goals only) — StatsBomb-inspired style
#   2. Shot location heat maps — pure density, no other info
#   3. xG vs Goals charts — cumulative
#   4. Goals by opponent group — lollipop + slope + bump + diverging bar
#   5. Summary dumbbell chart
#   6. xG distribution beeswarm — Juventus only
#   7. Season-by-season career trajectory

library(pacman)
p_load(dplyr, purrr, arrow, here, stringr, ggplot2, patchwork, glue, scales,
       zoo, ggbeeswarm, tidyr)

dir.create(here("Viz"), showWarnings = FALSE)

# ── Config ─────────────────────────────────────────────────────────────────────

CLUB    <- "Juventus"
LEAGUE  <- "serie_a"

VLAHOVIC_SEASONS <- c(2021, 2022, 2023, 2024, 2025)
HIGUAIN_SEASONS  <- c(2016, 2017, 2019)

VLAHOVIC_JUVE_SEASONS <- c(2022, 2023, 2024, 2025)
HIGUAIN_JUVE_SEASONS  <- c(2016, 2017, 2019)

BG_COLOR    <- "#f0f0f0"
LINE_COLOR  <- "#aaaaaa"
TEXT_COLOR  <- "#333333"

COL_GOAL    <- "#e03131"
COL_SAVED   <- "#4dabf7"
COL_MISSED  <- "#868e96"
COL_BLOCKED <- "#adb5bd"
COL_POST    <- "#f59f00"

COL_VLAHOVIC <- "#c0392b"
COL_HIGUAIN  <- "#2980b9"

Y_MIN      <- 50
Y_MAX      <- 107
Y_MIN_GOAL <- 85

# ── Club colors ────────────────────────────────────────────────────────────────

club_colors <- list(
  "Fiorentina" = list(primary = "#482e92", secondary = "#ec1c23"),
  "Napoli"     = list(primary = "#003c82", secondary = "#12a0d7"),
  "Juventus"   = list(primary = "#000000", secondary = "#ffffff"),
  "AC Milan"   = list(primary = "#fb090b", secondary = "#000000"),
  "Chelsea"    = list(primary = "#034694", secondary = "#dba111")
)

# ── Career stints ──────────────────────────────────────────────────────────────

vlahovic_stints <- list(
  list(club = "Fiorentina", league = "serie_a", seasons = c(2018, 2019, 2020, 2021, 2022)),
  list(club = "Juventus",   league = "serie_a", seasons = c(2022, 2023, 2024, 2025))
)

higuain_stints <- list(
  list(club = "Napoli",   league = "serie_a", seasons = c(2014, 2015, 2016)),
  list(club = "Juventus", league = "serie_a", seasons = c(2016, 2017)),
  list(club = "AC Milan", league = "serie_a", seasons = c(2018)),
  list(club = "Chelsea",  league = "epl",     seasons = c(2018)),
  list(club = "Juventus", league = "serie_a", seasons = c(2019))
)

# ── Opponent tier order ────────────────────────────────────────────────────────

tier_order <- c(
  "Champions League (1-4)",
  "Europa League (5-6)",
  "Top Half Midtable (7-10)",
  "Bottom Half Midtable (11-17)",
  "Relegation Zone (18-20)"
)

# ── Load and filter shots ──────────────────────────────────────────────────────

#' Load shot parquets for specified seasons and league
#' @param seasons Integer vector of season start years
#' @param league  League string, defaults to serie_a
load_shots <- function(seasons, league = "serie_a") {
  map_dfr(seasons, function(s) {
    path <- here("Data_Shot", paste0("shots_", league, "_", s, ".parquet"))
    if (!file.exists(path)) return(NULL)
    read_parquet(path) |> mutate(season = s)
  })
}

#' Filter shots to a player's Juventus appearances and add derived columns
#' Excludes penalties
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
      ),
      situation != "Penalty"
    ) |>
    mutate(
      xG      = as.numeric(xG),
      X       = as.numeric(X),
      Y       = as.numeric(Y),
      is_goal = result == "Goal",
      is_head = shotType == "Head",
      pitch_x = Y * 68,
      pitch_y = X * 105,
      shape_code = case_when(
        is_goal & is_head  ~ 17L,
        is_goal & !is_head ~ 16L,
        !is_goal & is_head ~ 2L,
        TRUE               ~ 1L
      ),
      dot_color = case_when(
        result == "Goal"        ~ COL_GOAL,
        result == "SavedShot"   ~ COL_SAVED,
        result == "MissedShots" ~ COL_MISSED,
        result == "BlockedShot" ~ COL_BLOCKED,
        result == "ShotOnPost"  ~ COL_POST,
        TRUE                    ~ COL_MISSED
      )
    )
}

all_shots_v <- load_shots(VLAHOVIC_SEASONS)
all_shots_h <- load_shots(HIGUAIN_SEASONS)

vlahovic <- filter_player_juve(all_shots_v, "Dusan Vlahovic")
higuain  <- filter_player_juve(all_shots_h, "Gonzalo Higuaín")

# ── Pitch layer helpers ────────────────────────────────────────────────────────

#' Background rect layer for the pitch half
#' @param y_min Bottom y-coordinate of the visible pitch area
pitch_background <- function(y_min = Y_MIN) {
  list(
    annotate("rect", xmin = 0, xmax = 68, ymin = y_min, ymax = 105,
             fill = BG_COLOR, color = NA)
  )
}

#' Line annotations for pitch markings (no arc)
#' @param y_min Bottom y-coordinate of the visible pitch area
pitch_lines <- function(y_min = Y_MIN) {
  lw <- 0.5
  list(
    annotate("segment", x = 0,  xend = 0,  y = y_min, yend = 105,
             color = LINE_COLOR, linewidth = lw),
    annotate("segment", x = 68, xend = 68, y = y_min, yend = 105,
             color = LINE_COLOR, linewidth = lw),
    annotate("segment", x = 0,  xend = 68, y = y_min, yend = y_min,
             color = LINE_COLOR, linewidth = lw),
    annotate("segment", x = 0,  xend = 68, y = 105,   yend = 105,
             color = LINE_COLOR, linewidth = lw),
    annotate("rect", xmin = 13.84, xmax = 54.16, ymin = 88.5, ymax = 105,
             fill = NA, color = LINE_COLOR, linewidth = lw),
    annotate("rect", xmin = 24.84, xmax = 43.16, ymin = 99.5, ymax = 105,
             fill = NA, color = LINE_COLOR, linewidth = lw),
    annotate("point", x = 34, y = 94, color = LINE_COLOR, size = 0.8)
  )
}

#' Combined pitch background + lines
#' @param y_min Bottom y-coordinate of the visible pitch area
pitch_layers <- function(y_min = Y_MIN) {
  c(pitch_background(y_min), pitch_lines(y_min))
}

# ── Legend ─────────────────────────────────────────────────────────────────────

#' Add shot result + shape legend below the pitch
#' @param p          ggplot object to annotate
#' @param goals_only Logical — show only goal legend or full result legend
#' @param y_min      Bottom y-coordinate of the visible pitch area
add_legend <- function(p, goals_only = FALSE, y_min = Y_MIN) {
  leg_y1 <- y_min - 3.5
  leg_y2 <- y_min - 7.0
  leg_y3 <- y_min - 10.5

  if (goals_only) {
    results <- list(list(x = 34, color = COL_GOAL, label = "Goal"))
  } else {
    results <- list(
      list(x = 8,  color = COL_GOAL,    label = "Goal"),
      list(x = 20, color = COL_SAVED,   label = "Saved"),
      list(x = 32, color = COL_MISSED,  label = "Missed"),
      list(x = 44, color = COL_BLOCKED, label = "Blocked"),
      list(x = 56, color = COL_POST,    label = "Hit post")
    )
  }

  for (r in results) {
    p <- p +
      annotate("point", x = r$x - 1.5, y = leg_y1,
               color = r$color, fill = r$color, shape = 16, size = 4) +
      annotate("text",  x = r$x + 0.2, y = leg_y1,
               label = r$label, color = TEXT_COLOR,
               size = 3.5, hjust = 0, vjust = 0.5)
  }

  p +
    annotate("point", x = 7,    y = leg_y2, color = TEXT_COLOR,
             fill = NA, shape = 1, size = 4, stroke = 0.7) +
    annotate("text",  x = 8.5,  y = leg_y2, label = "Foot shot",
             color = TEXT_COLOR, size = 3.5, hjust = 0, vjust = 0.5) +
    annotate("point", x = 21,   y = leg_y2, color = TEXT_COLOR,
             fill = NA, shape = 2, size = 4, stroke = 0.7) +
    annotate("text",  x = 22.5, y = leg_y2, label = "Header",
             color = TEXT_COLOR, size = 3.5, hjust = 0, vjust = 0.5) +
    annotate("text",  x = 34,   y = leg_y2, label = "Size = xG",
             color = TEXT_COLOR, size = 3.5, hjust = 0.5, vjust = 0.5,
             fontface = "italic") +
    annotate("point", x = 46,   y = leg_y2, color = TEXT_COLOR,
             fill = NA, shape = 1, size = 2, stroke = 0.5) +
    annotate("text",  x = 47.5, y = leg_y2, label = "Low xG",
             color = TEXT_COLOR, size = 3.2, hjust = 0, vjust = 0.5) +
    annotate("point", x = 57,   y = leg_y2, color = TEXT_COLOR,
             fill = NA, shape = 1, size = 5, stroke = 0.5) +
    annotate("text",  x = 58.8, y = leg_y2, label = "High xG",
             color = TEXT_COLOR, size = 3.2, hjust = 0, vjust = 0.5) +
    annotate("text",  x = 68,   y = leg_y3,
             label = "viz: @semperty | data: understat.com",
             color = TEXT_COLOR, size = 2.8, hjust = 1, vjust = 0.5)
}

# ── Shot map builder ───────────────────────────────────────────────────────────

#' Build a StatsBomb-style shot map for one player
#' @param df         Filtered shot data frame
#' @param title_text Plot title string
#' @param goals_only Logical — show only goals or all shots
build_shot_map <- function(df, title_text = "", goals_only = FALSE) {
  y_min   <- ifelse(goals_only, Y_MIN_GOAL, Y_MIN)
  plot_df <- if (goals_only) filter(df, is_goal) else df
  n       <- nrow(plot_df)
  n_goals <- sum(df$is_goal)

  subtitle <- case_when(
    goals_only ~ glue("{n} goals (excl. penalties) | Juventus only"),
    TRUE       ~ glue("{n} shots | {n_goals} goals (excl. penalties) | Juventus only")
  )

  non_goals <- filter(plot_df, !is_goal)
  goals_df  <- filter(plot_df,  is_goal)

  p <- ggplot() + pitch_layers(y_min)

  if (nrow(non_goals) > 0 && !goals_only) {
    p <- p +
      geom_point(
        data   = non_goals,
        aes(x = pitch_x, y = pitch_y, color = dot_color,
            size = xG, shape = factor(shape_code)),
        fill   = NA, stroke = 0.6, alpha = 0.75
      )
  }

  if (nrow(goals_df) > 0) {
    p <- p +
      geom_point(
        data   = goals_df,
        aes(x = pitch_x, y = pitch_y, color = dot_color,
            fill = dot_color, size = xG, shape = factor(shape_code)),
        stroke = 0.4, alpha = 0.95
      )
  }

  p <- p +
    scale_shape_manual(values = c("1" = 1, "2" = 2, "16" = 16, "17" = 17),
                       guide = "none") +
    scale_color_identity(guide = "none") +
    scale_fill_identity(guide  = "none") +
    scale_size_continuous(range = c(1, 7), guide = "none") +
    scale_x_continuous(limits = c(0, 68),             expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min - 13, Y_MAX), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = title_text, subtitle = subtitle, x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = BG_COLOR, color = NA),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      plot.title       = element_text(color = TEXT_COLOR, size = 18,
                                      face = "bold", hjust = 0.5,
                                      margin = margin(b = 2)),
      plot.subtitle    = element_text(color = TEXT_COLOR, size = 9,
                                      hjust = 0.5, margin = margin(b = 8)),
      plot.margin      = margin(16, 16, 8, 16)
    )

  add_legend(p, goals_only = goals_only, y_min = y_min)
}

# ── Heat map builder ───────────────────────────────────────────────────────────

#' Build a KDE density heat map for shot locations
#' @param df         Filtered shot data frame
#' @param title_text Plot title string
#' @param goals_only Logical — show only goals or all shots
build_heat_map <- function(df, title_text = "", goals_only = FALSE) {
  y_min   <- ifelse(goals_only, Y_MIN_GOAL, Y_MIN)
  plot_df <- if (goals_only) filter(df, is_goal) else df
  n       <- nrow(plot_df)
  n_goals <- sum(df$is_goal)

  subtitle <- case_when(
    goals_only ~ glue("{n} goals (excl. penalties) | Juventus only"),
    TRUE       ~ glue("{n} shots | {n_goals} goals (excl. penalties) | Juventus only")
  )

  kde <- MASS::kde2d(
    x    = plot_df$pitch_x,
    y    = plot_df$pitch_y,
    n    = 150,
    lims = c(0, 68, y_min, 105)
  )

  density_df <- expand.grid(pitch_x = kde$x, pitch_y = kde$y) |>
    mutate(density = as.vector(kde$z)) |>
    filter(
      pitch_x >= 0,     pitch_x <= 68,
      pitch_y >= y_min, pitch_y <= 105
    )

  max_d <- max(density_df$density)

  ggplot() +
    pitch_background(y_min) +
    geom_raster(
      data        = density_df,
      aes(x = pitch_x, y = pitch_y, fill = density),
      alpha       = 0.9,
      interpolate = TRUE
    ) +
    scale_fill_gradientn(
      colors = c(BG_COLOR, "#ffd8a8", "#ff8787", "#c92a2a"),
      values = rescale(c(0, max_d * 0.05, max_d * 0.4, max_d)),
      limits = c(0, max_d),
      guide  = "none"
    ) +
    pitch_lines(y_min) +
    scale_x_continuous(limits = c(0, 68),            expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min - 8, Y_MAX), expand = c(0, 0)) +
    coord_fixed(clip = "on") +
    labs(title = title_text, subtitle = subtitle, x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = BG_COLOR, color = NA),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      plot.title       = element_text(color = TEXT_COLOR, size = 18,
                                      face = "bold", hjust = 0.5,
                                      margin = margin(b = 2)),
      plot.subtitle    = element_text(color = TEXT_COLOR, size = 9,
                                      hjust = 0.5, margin = margin(b = 8)),
      plot.margin      = margin(16, 16, 8, 16)
    ) +
    annotate("text", x = 68, y = y_min - 3,
             label = "viz: @semperty | data: understat.com",
             color = TEXT_COLOR, size = 2.8, hjust = 1, vjust = 0.5)
}

# ── Career xG vs Goals ─────────────────────────────────────────────────────────

#' Load shots for one career stint and tag with club
#' @param stint       List with club, league, seasons fields
#' @param player_name Player name string
load_stint_shots <- function(stint, player_name) {
  shots <- map_dfr(stint$seasons, function(s) {
    path <- here("Data_Shot",
                 paste0("shots_", stint$league, "_", s, ".parquet"))
    if (!file.exists(path)) return(NULL)
    read_parquet(path) |> mutate(season = s)
  })
  if (is.null(shots) || nrow(shots) == 0) return(NULL)

  shots |>
    filter(
      player == player_name,
      case_when(
        side == "h" ~ h_team == stint$club,
        side == "a" ~ a_team == stint$club,
        TRUE        ~ FALSE
      ),
      situation != "Penalty"
    ) |>
    mutate(
      xG      = as.numeric(xG),
      is_goal = result == "Goal",
      club    = stint$club,
      date    = as.Date(substr(date, 1, 10))
    )
}

#' Build per-match career data frame with cumulative + rolling stats
#' Stint number increments on each club change including return stints
#' @param stints      List of stint lists (club, league, seasons)
#' @param player_name Player name string
#' @param roll_n      Rolling window size, defaults to 15
build_career_df <- function(stints, player_name, roll_n = 15) {
  all_shots <- map_dfr(stints, load_stint_shots, player_name = player_name)
  if (is.null(all_shots) || nrow(all_shots) == 0) return(NULL)

  all_shots |>
    group_by(match_id, date, club) |>
    summarise(goals = sum(is_goal), xg = round(sum(xG), 3), .groups = "drop") |>
    arrange(date) |>
    mutate(
      game_num   = row_number(),
      stint      = cumsum(club != lag(club, default = first(club))) + 1,
      cum_goals  = cumsum(goals),
      cum_xg     = round(cumsum(xg), 2),
      roll_goals = rollmean(goals, k = roll_n, fill = NA, align = "right"),
      roll_xg    = round(rollmean(xg, k = roll_n, fill = NA, align = "right"), 3)
    )
}

#' Build cumulative or rolling xG vs Goals chart for one player's career
#' Club colors fill the ribbon between lines; stint transitions marked with vlines
#' @param career_df   Output of build_career_df()
#' @param player_name Player name string for title
#' @param mode        "cumulative" or "rolling"
#' @param roll_n      Rolling window size, used in subtitle when mode = "rolling"
build_xg_chart <- function(career_df, player_name, mode = "cumulative",
                           roll_n = 15) {
  select <- dplyr::select

  if (mode == "cumulative") {
    plot_df <- career_df |>
      select(date, club, stint, game_num,
             g_line = cum_goals, xg_line = cum_xg)
    y_label      <- "Cumulative goals / xG"
    title_suffix <- "Cumulative Goals vs xG"
  } else {
    plot_df <- career_df |>
      select(date, club, stint, game_num,
             g_line = roll_goals, xg_line = roll_xg)
    y_label      <- glue("Goals / xG ({roll_n}-game rolling avg)")
    title_suffix <- glue("{roll_n}-Game Rolling Goals vs xG")
  }

  transitions <- career_df |>
    group_by(stint, club) |>
    summarise(start_date = min(date), end_date = max(date), .groups = "drop") |>
    arrange(start_date)

  plot_df <- plot_df |>
    mutate(
      primary_col   = map_chr(club, ~ club_colors[[.x]]$primary),
      secondary_col = map_chr(club, ~ club_colors[[.x]]$secondary)
    ) |>
    filter(!is.na(g_line), !is.na(xg_line))

  p <- ggplot() + theme_void()

  runs <- rle(plot_df$stint)
  idx  <- cumsum(c(1, runs$lengths))

  for (k in seq_along(runs$lengths)) {
    start_i <- idx[k]
    end_i   <- min(idx[k + 1], nrow(plot_df))
    seg     <- plot_df[start_i:end_i, ]
    if (nrow(seg) < 2) next

    pri <- seg$primary_col[1]
    sec <- seg$secondary_col[1]

    p <- p +
      geom_ribbon(
        data  = seg,
        aes(x = date, ymin = xg_line, ymax = pmax(g_line, xg_line)),
        fill  = pri, alpha = 0.7
      ) +
      geom_ribbon(
        data  = seg,
        aes(x = date, ymin = pmin(g_line, xg_line), ymax = xg_line),
        fill  = sec, alpha = 0.6
      )
  }

  p <- p +
    geom_line(data = plot_df, aes(x = date, y = g_line),
              color = TEXT_COLOR, linewidth = 0.8) +
    geom_line(data = plot_df, aes(x = date, y = xg_line),
              color = TEXT_COLOR, linewidth = 0.8, linetype = "dashed")

  for (i in seq_len(nrow(transitions))) {
    trans_date <- transitions$start_date[i]
    club_name  <- transitions$club[i]

    if (i > 1) {
      p <- p +
        geom_vline(xintercept = trans_date, color = "#888888",
                   linewidth = 0.5, linetype = "solid")
    }

    p <- p +
      annotate("text", x = trans_date + 10, y = Inf,
               label = club_name, color = TEXT_COLOR,
               size = 3, hjust = 0, vjust = 1.5, fontface = "bold")
  }

  p +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(
      title    = glue("{player_name} — {title_suffix}"),
      subtitle = "Solid line = Goals | Dashed line = xG | Penalties excluded | Top 5 leagues only",
      caption  = "viz: @semperty | data: understat.com",
      x        = NULL,
      y        = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = BG_COLOR, color = NA),
      panel.background   = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd", linewidth = 0.3),
      plot.title         = element_text(color = TEXT_COLOR, size = 16,
                                        face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(color = TEXT_COLOR, size = 8,
                                        hjust = 1, margin = margin(t = 8)),
      axis.text.x        = element_text(color = TEXT_COLOR, size = 8,
                                        angle = 45, hjust = 1),
      axis.text.y        = element_text(color = TEXT_COLOR, size = 8),
      axis.title.y       = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(r = 6)),
      plot.margin        = margin(16, 16, 16, 16)
    )
}

# ── Goals by opponent group — shared helpers ───────────────────────────────────

#' Shared theme for opponent group charts
group_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = BG_COLOR, color = NA),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(color = TEXT_COLOR, size = 16,
                                      face = "bold", margin = margin(b = 4)),
      plot.subtitle    = element_text(color = TEXT_COLOR, size = 9,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(color = TEXT_COLOR, size = 8,
                                      hjust = 1, margin = margin(t = 8)),
      axis.text        = element_text(color = TEXT_COLOR, size = 9),
      plot.margin      = margin(16, 24, 16, 16)
    )
}

#' Shared labs for opponent group charts
#' @param title Plot title string
group_labs <- function(title = "Goals vs Games by Opponent Tier") {
  labs(
    title    = title,
    subtitle = "Ratio > 1 = more goals than share of games | Juventus only | Penalties excluded",
    caption  = "viz: @semperty | data: understat.com",
    x        = NULL,
    y        = "Goals % / Games % ratio"
  )
}

# ── Goals by opponent group — lollipop ─────────────────────────────────────────

#' Connected dot plot of goals/games ratio by opponent tier
#' @param by_group Output of comparison_by_group.parquet
build_group_lollipop <- function(by_group) {
  select <- dplyr::select

  plot_df <- by_group |>
    mutate(
      opp_group = factor(opp_group, levels = rev(tier_order)),
      col       = case_when(
        player == "Dusan Vlahovic" ~ COL_VLAHOVIC,
        TRUE                       ~ COL_HIGUAIN
      )
    )

  seg_df <- by_group |>
    select(opp_group, player, goals_games_ratio) |>
    pivot_wider(names_from = player, values_from = goals_games_ratio) |>
    mutate(opp_group = factor(opp_group, levels = rev(tier_order)))

  ggplot() +
    geom_hline(yintercept = 1, color = LINE_COLOR, linewidth = 0.4,
               linetype = "dashed") +
    geom_segment(
      data = seg_df,
      aes(x = opp_group, xend = opp_group,
          y = `Dusan Vlahovic`, yend = `Gonzalo Higuaín`),
      color = LINE_COLOR, linewidth = 0.6
    ) +
    geom_point(
      data = plot_df,
      aes(x = opp_group, y = goals_games_ratio, color = col),
      size = 4
    ) +
    geom_text(
      data = plot_df |> filter(player == "Dusan Vlahovic"),
      aes(x = opp_group, y = goals_games_ratio,
          label = round(goals_games_ratio, 2)),
      color = COL_VLAHOVIC, size = 3, hjust = 1.6
    ) +
    geom_text(
      data = plot_df |> filter(player == "Gonzalo Higuaín"),
      aes(x = opp_group, y = goals_games_ratio,
          label = round(goals_games_ratio, 2)),
      color = COL_HIGUAIN, size = 3, hjust = -0.6
    ) +
    annotate("text", x = length(tier_order) + 0.4, y = Inf,
             label = "Vlahovic", color = COL_VLAHOVIC,
             size = 3.5, hjust = 1, vjust = 1, fontface = "bold") +
    annotate("text", x = length(tier_order) + 0.4, y = -Inf,
             label = "Higuaín", color = COL_HIGUAIN,
             size = 3.5, hjust = 0, vjust = 1, fontface = "bold") +
    scale_color_identity() +
    scale_y_continuous(
      breaks = seq(0, 2, 0.5),
      labels = function(x) ifelse(x == 1, "1.0 (expected)", as.character(x))
    ) +
    coord_flip(clip = "off") +
    group_labs() +
    group_theme() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd", linewidth = 0.3)
    )
}

# ── Goals by opponent group — slope ────────────────────────────────────────────

#' Slope/line chart of goals/games ratio across opponent tiers
#' @param by_group Output of comparison_by_group.parquet
build_group_slope <- function(by_group) {
  plot_df <- by_group |>
    mutate(
      opp_group = factor(opp_group, levels = tier_order),
      col       = case_when(
        player == "Dusan Vlahovic" ~ COL_VLAHOVIC,
        TRUE                       ~ COL_HIGUAIN
      ),
      label = case_when(
        player == "Dusan Vlahovic" ~ "Vlahovic",
        TRUE                       ~ "Higuaín"
      )
    )

  ggplot(plot_df, aes(x = opp_group, y = goals_games_ratio,
                      group = player, color = col)) +
    geom_hline(yintercept = 1, color = LINE_COLOR, linewidth = 0.4,
               linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3.5) +
    geom_text(
      data = plot_df |> filter(opp_group == "Relegation Zone (18-20)"),
      aes(label = label),
      hjust = -0.2, size = 3.2, fontface = "bold"
    ) +
    annotate("text", x = 2.5, y = 1.02, label = "expected rate",
             color = LINE_COLOR, size = 2.8, hjust = 0.5, vjust = 0) +
    scale_color_identity() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
    scale_y_continuous(
      breaks = seq(0, 2, 0.5),
      labels = function(x) ifelse(x == 1, "1.0", as.character(x))
    ) +
    group_labs() +
    group_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd", linewidth = 0.3),
      plot.margin        = margin(16, 48, 16, 16)
    )
}

# ── Goals by opponent group — bump/rank ────────────────────────────────────────

#' Bump chart showing rank (1st/2nd) at each opponent tier
#' @param by_group Output of comparison_by_group.parquet
build_group_bump <- function(by_group) {
  plot_df <- by_group |>
    mutate(opp_group = factor(opp_group, levels = tier_order)) |>
    group_by(opp_group) |>
    mutate(
      rank  = rank(-goals_games_ratio, ties.method = "first"),
      col   = case_when(
        player == "Dusan Vlahovic" ~ COL_VLAHOVIC,
        TRUE                       ~ COL_HIGUAIN
      ),
      label = case_when(
        player == "Dusan Vlahovic" ~ "Vlahovic",
        TRUE                       ~ "Higuaín"
      )
    ) |>
    ungroup()

  ggplot(plot_df, aes(x = opp_group, y = rank, group = player, color = col)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 5) +
    geom_text(
      aes(label = round(goals_games_ratio, 2)),
      color = "white", size = 2.5, fontface = "bold"
    ) +
    geom_text(
      data = plot_df |> filter(opp_group == "Relegation Zone (18-20)"),
      aes(label = label),
      hjust = -0.2, size = 3.2, fontface = "bold"
    ) +
    scale_color_identity() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
    scale_y_reverse(breaks = 1:2, labels = c("1st", "2nd")) +
    labs(
      title    = "Who Scored More vs Each Opponent Tier?",
      subtitle = "Rank by goals%/games% ratio at each tier | Juventus only | Penalties excluded",
      caption  = "viz: @semperty | data: understat.com",
      x        = NULL,
      y        = NULL
    ) +
    group_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd", linewidth = 0.3),
      plot.margin        = margin(16, 48, 16, 16)
    )
}

# ── Goals by opponent group — diverging bar ────────────────────────────────────

#' Diverging bar chart showing Vlahovic minus Higuain ratio per tier
#' Positive = Vlahovic ahead, negative = Higuain ahead
#' @param by_group Output of comparison_by_group.parquet
build_group_diverging <- function(by_group) {
  select <- dplyr::select

  plot_df <- by_group |>
    select(opp_group, player, goals_games_ratio) |>
    pivot_wider(names_from = player, values_from = goals_games_ratio) |>
    mutate(
      opp_group = factor(opp_group, levels = rev(tier_order)),
      diff      = round(`Dusan Vlahovic` - `Gonzalo Higuaín`, 10),
      col       = case_when(
        diff > 0 ~ COL_VLAHOVIC,
        TRUE     ~ COL_HIGUAIN
      ),
      label_v = round(`Dusan Vlahovic`, 2),
      label_h = round(`Gonzalo Higuaín`, 2)
    )

  ggplot(plot_df, aes(x = opp_group, y = diff, fill = col)) +
    geom_col(width = 0.55) +
    geom_hline(yintercept = 0, color = TEXT_COLOR, linewidth = 0.5) +
    geom_text(
      data = plot_df |> filter(diff > 0),
      aes(y = diff + 0.03, label = glue("V: {label_v}")),
      color = COL_VLAHOVIC, size = 2.8, hjust = 0, fontface = "bold"
    ) +
    geom_text(
      data = plot_df |> filter(diff > 0),
      aes(y = -0.03, label = glue("H: {label_h}")),
      color = COL_HIGUAIN, size = 2.8, hjust = 1
    ) +
    geom_text(
      data = plot_df |> filter(diff <= 0),
      aes(y = diff - 0.03, label = glue("H: {label_h}")),
      color = COL_HIGUAIN, size = 2.8, hjust = 1, fontface = "bold"
    ) +
    geom_text(
      data = plot_df |> filter(diff <= 0),
      aes(y = 0.03, label = glue("V: {label_v}")),
      color = COL_VLAHOVIC, size = 2.8, hjust = 0
    ) +
    annotate("text", x = length(tier_order) + 0.45, y = 0.3,
             label = "Vlahovic\nahead", color = COL_VLAHOVIC,
             size = 3, hjust = 0.5, vjust = 1, fontface = "bold",
             lineheight = 0.9) +
    annotate("text", x = length(tier_order) + 0.45, y = -0.3,
             label = "Higuaín\nahead", color = COL_HIGUAIN,
             size = 3, hjust = 0.5, vjust = 1, fontface = "bold",
             lineheight = 0.9) +
    scale_fill_identity() +
    scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2)) +
    coord_flip(clip = "off") +
    labs(
      title    = "Goal Ratio Gap by Opponent Tier",
      subtitle = "Vlahovic minus Higuaín goals%/games% ratio | Juventus only | Penalties excluded",
      caption  = "viz: @semperty | data: understat.com",
      x        = NULL,
      y        = "Difference in ratio (Vlahovic - Higuaín)"
    ) +
    group_theme() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd", linewidth = 0.3),
      plot.margin        = margin(16, 32, 16, 16)
    )
}

# ── Summary dumbbell ───────────────────────────────────────────────────────────

#' Dumbbell chart comparing scoring profile metrics at Juventus
#' Labels always placed left-of-lower and right-of-higher dot
#' @param bunched  comparison_bunched.parquet data frame
#' @param decisive comparison_decisive.parquet data frame
build_dumbbell <- function(bunched, decisive) {
  select <- dplyr::select

  metrics <- bind_rows(
    bunched |>
      select(player, goals_per_game, pct_scoring_games,
             pct_bunched_games, bunched_goals_pct) |>
      pivot_longer(-player, names_to = "metric", values_to = "value"),
    decisive |>
      select(player, pct_goals_decisive, pct_games_decisive) |>
      pivot_longer(-player, names_to = "metric", values_to = "value")
  ) |>
    mutate(
      metric_label = case_when(
        metric == "goals_per_game"      ~ "Goals per game",
        metric == "pct_scoring_games"   ~ "% games scored",
        metric == "pct_bunched_games"   ~ "% games with 2+ goals",
        metric == "bunched_goals_pct"   ~ "% goals in multi-goal games",
        metric == "pct_goals_decisive"  ~ "% goals that were decisive",
        metric == "pct_games_decisive"  ~ "% games with a decisive goal"
      ),
      metric_label = factor(metric_label, levels = c(
        "Goals per game",
        "% games scored",
        "% games with 2+ goals",
        "% goals in multi-goal games",
        "% goals that were decisive",
        "% games with a decisive goal"
      )),
      col = case_when(
        player == "Dusan Vlahovic" ~ COL_VLAHOVIC,
        TRUE                       ~ COL_HIGUAIN
      )
    )

  seg_df <- metrics |>
    select(metric_label, player, value) |>
    pivot_wider(names_from = player, values_from = value)

  ggplot() +
    geom_segment(
      data = seg_df,
      aes(x = metric_label, xend = metric_label,
          y = `Dusan Vlahovic`, yend = `Gonzalo Higuaín`),
      color = LINE_COLOR, linewidth = 0.8
    ) +
    geom_point(
      data = metrics,
      aes(x = metric_label, y = value, color = col),
      size = 4.5
    ) +
    geom_text(
      data = metrics |>
        group_by(metric_label) |>
        mutate(is_lower = value == min(value)) |>
        ungroup(),
      aes(x = metric_label, y = value,
          label = round(value, 1),
          color = col,
          hjust = ifelse(is_lower, 2.2, -1.2)),
      size = 2.8
    ) +
    annotate("text", x = 6.4, y = 20,
             label = "Vlahovic", color = COL_VLAHOVIC,
             size = 3.5, hjust = 0.5, fontface = "bold") +
    annotate("text", x = 6.4, y = 35,
             label = "Higuaín", color = COL_HIGUAIN,
             size = 3.5, hjust = 0.5, fontface = "bold") +
    scale_color_identity() +
    coord_flip(clip = "off") +
    labs(
      title    = "Vlahovic vs Higuaín — Juventus Scoring Profile",
      subtitle = "At Juventus only | Penalties excluded",
      caption  = "viz: @semperty | data: understat.com",
      x        = NULL,
      y        = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = BG_COLOR, color = NA),
      panel.background   = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd", linewidth = 0.3),
      plot.title         = element_text(color = TEXT_COLOR, size = 16,
                                        face = "bold", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(color = TEXT_COLOR, size = 9,
                                        hjust = 0.5, margin = margin(b = 8)),
      plot.caption       = element_text(color = TEXT_COLOR, size = 8,
                                        hjust = 1, margin = margin(t = 8)),
      axis.text.y        = element_text(color = TEXT_COLOR, size = 9),
      axis.text.x        = element_blank(),
      plot.margin        = margin(16, 32, 16, 16)
    )
}

# ── xG distribution beeswarm ───────────────────────────────────────────────────

#' Beeswarm of individual shot xG values — goals in red, non-goals in grey
#' @param vlahovic_shots Filtered Vlahovic shot data
#' @param higuain_shots  Filtered Higuain shot data
build_beeswarm <- function(vlahovic_shots, higuain_shots) {
  plot_df <- bind_rows(
    vlahovic_shots |> mutate(player = "Dusan Vlahovic"),
    higuain_shots  |> mutate(player = "Gonzalo Higuaín")
  ) |>
    mutate(
      player = factor(player, levels = c("Dusan Vlahovic", "Gonzalo Higuaín")),
      col    = case_when(
        result == "Goal" ~ COL_GOAL,
        TRUE             ~ "#aaaaaa"
      )
    )

  n_v <- nrow(vlahovic_shots)
  n_h <- nrow(higuain_shots)

  ggplot(plot_df, aes(x = xG, y = player, color = col)) +
    geom_beeswarm(
      groupOnX = FALSE,
      size     = 1.8,
      alpha    = 0.75,
      cex      = 1.2
    ) +
    geom_vline(xintercept = 0.1, color = LINE_COLOR, linewidth = 0.4,
               linetype = "dashed") +
    geom_vline(xintercept = 0.3, color = LINE_COLOR, linewidth = 0.4,
               linetype = "dashed") +
    annotate("text", x = 0.1, y = 2.55, label = "xG = 0.1",
             color = LINE_COLOR, size = 2.6, hjust = 0.5, vjust = 0) +
    annotate("text", x = 0.3, y = 2.55, label = "xG = 0.3",
             color = LINE_COLOR, size = 2.6, hjust = 0.5, vjust = 0) +
    annotate("point", x = max(plot_df$xG, na.rm = TRUE) * 0.72,
             y = 0.6, color = COL_GOAL, size = 2.8) +
    annotate("text",  x = max(plot_df$xG, na.rm = TRUE) * 0.74,
             y = 0.6, label = "Goal", color = TEXT_COLOR,
             size = 2.8, hjust = 0) +
    annotate("point", x = max(plot_df$xG, na.rm = TRUE) * 0.84,
             y = 0.6, color = "#aaaaaa", size = 2.8) +
    annotate("text",  x = max(plot_df$xG, na.rm = TRUE) * 0.86,
             y = 0.6, label = "Non-goal", color = TEXT_COLOR,
             size = 2.8, hjust = 0) +
    scale_color_identity() +
    scale_x_continuous(breaks = seq(0, 0.8, 0.1), limits = c(0, NA)) +
    scale_y_discrete(labels = c(
      "Dusan Vlahovic"  = glue("Vlahovic\n({n_v} shots)"),
      "Gonzalo Higuaín" = glue("Higuaín\n({n_h} shots)")
    )) +
    labs(
      title    = "Shot Quality Distribution — Juventus",
      subtitle = "Each dot = one shot | Penalties excluded",
      caption  = "viz: @semperty | data: understat.com",
      x        = "xG per shot",
      y        = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = BG_COLOR, color = NA),
      panel.background   = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd", linewidth = 0.3),
      plot.title         = element_text(color = TEXT_COLOR, size = 16,
                                        face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(color = TEXT_COLOR, size = 8,
                                        hjust = 1, margin = margin(t = 8)),
      axis.text          = element_text(color = TEXT_COLOR, size = 9),
      axis.title.x       = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(t = 6)),
      plot.margin        = margin(16, 16, 16, 16)
    )
}

# ── Career trajectory ──────────────────────────────────────────────────────────

#' Season-by-season goals/game and xG/game line chart
#' Juventus seasons marked with open circles
#' Uses VLAHOVIC_JUVE_SEASONS and HIGUAIN_JUVE_SEASONS constants for circle tagging
build_career_trajectory <- function() {
  players <- list(
    list(name = "Dusan Vlahovic",  stints = vlahovic_stints,
         col = COL_VLAHOVIC, juve_seasons = VLAHOVIC_JUVE_SEASONS),
    list(name = "Gonzalo Higuaín", stints = higuain_stints,
         col = COL_HIGUAIN,  juve_seasons = HIGUAIN_JUVE_SEASONS)
  )

  season_df <- map_dfr(players, function(pl) {
    map_dfr(pl$stints, function(stint) {
      map_dfr(stint$seasons, function(s) {
        path <- here("Data_Shot",
                     paste0("shots_", stint$league, "_", s, ".parquet"))
        if (!file.exists(path)) return(NULL)

        shots <- read_parquet(path) |>
          filter(
            player == pl$name,
            case_when(
              side == "h" ~ h_team == stint$club,
              side == "a" ~ a_team == stint$club,
              TRUE        ~ FALSE
            ),
            situation != "Penalty"
          ) |>
          mutate(
            xG      = as.numeric(xG),
            is_goal = result == "Goal"
          )

        if (nrow(shots) == 0) return(NULL)

        n_games <- shots |> distinct(match_id) |> nrow()

        tibble(
          player         = pl$name,
          col            = pl$col,
          club           = stint$club,
          season         = s,
          goals          = sum(shots$is_goal),
          xg             = sum(shots$xG),
          games          = n_games,
          goals_per_game = goals / games,
          xg_per_game    = xg / games,
          is_juve        = s %in% pl$juve_seasons & stint$club == "Juventus"
        )
      })
    })
  }) |>
    mutate(
      season_label = glue("{season}/{str_sub(as.character(season + 1), 3, 4)}"),
      season_label = factor(season_label, levels = unique(season_label[order(season)]))
    )

  ggplot(season_df,
         aes(x = season_label, y = goals_per_game,
             group = player, color = col)) +
    geom_line(aes(y = xg_per_game), linewidth = 0.6, linetype = "dashed",
              alpha = 0.6) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3) +
    geom_point(
      data = season_df |> filter(is_juve),
      aes(y = goals_per_game),
      shape = 21, size = 5, stroke = 1.2, fill = NA
    ) +
    geom_text(
      data = season_df |>
        group_by(player) |>
        filter(season == max(season)) |>
        ungroup(),
      aes(label = case_when(
        player == "Dusan Vlahovic" ~ "Vlahovic",
        TRUE                       ~ "Higuaín"
      )),
      hjust = -0.2, size = 3.2, fontface = "bold"
    ) +
    scale_color_identity() +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.15))
    ) +
    annotate("text", x = Inf, y = Inf,
             label = "Circled = Juventus season",
             color = LINE_COLOR, size = 2.8,
             hjust = 1.1, vjust = 1.5) +
    labs(
      title    = "Season-by-Season Scoring Rate",
      subtitle = "Solid = Goals/game | Dashed = xG/game | Penalties excluded | Top 5 leagues only",
      caption  = "viz: @semperty | data: understat.com",
      x        = NULL,
      y        = "Per game"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = BG_COLOR, color = NA),
      panel.background   = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd", linewidth = 0.3),
      plot.title         = element_text(color = TEXT_COLOR, size = 16,
                                        face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(color = TEXT_COLOR, size = 8,
                                        hjust = 1, margin = margin(t = 8)),
      axis.text.x        = element_text(color = TEXT_COLOR, size = 8,
                                        angle = 45, hjust = 1),
      axis.text.y        = element_text(color = TEXT_COLOR, size = 8),
      axis.title.y       = element_text(color = TEXT_COLOR, size = 9,
                                        margin = margin(r = 6)),
      plot.margin        = margin(16, 40, 16, 16)
    )
}

# ── Save functions ─────────────────────────────────────────────────────────────

save_all_maps <- function() {
  maps <- list(
    list(df = vlahovic, name = "Dusan Vlahovic",  slug = "vlahovic",
         goals_only = FALSE),
    list(df = vlahovic, name = "Dusan Vlahovic",  slug = "vlahovic",
         goals_only = TRUE),
    list(df = higuain,  name = "Gonzalo Higuaín", slug = "higuain",
         goals_only = FALSE),
    list(df = higuain,  name = "Gonzalo Higuaín", slug = "higuain",
         goals_only = TRUE)
  )

  for (m in maps) {
    type       <- ifelse(m$goals_only, "goals", "all_shots")
    type_label <- ifelse(m$goals_only, "Goals", "All Shots")
    h          <- ifelse(m$goals_only, 9, 11)

    p1 <- build_shot_map(m$df,
                         title_text = glue("{m$name} — {type_label}"),
                         goals_only = m$goals_only)
    ggsave(here("Viz", glue("shot_map_{m$slug}_{type}.png")),
           p1, width = 10, height = h, dpi = 150, bg = BG_COLOR)
    print(paste("Saved: shot_map", m$slug, type))

    p2 <- build_heat_map(m$df,
                         title_text = glue("{m$name} — {type_label} (Heat Map)"),
                         goals_only = m$goals_only)
    ggsave(here("Viz", glue("heat_map_{m$slug}_{type}.png")),
           p2, width = 10, height = h, dpi = 150, bg = BG_COLOR)
    print(paste("Saved: heat_map", m$slug, type))
  }
}

save_xg_charts <- function() {
  players <- list(
    list(name = "Dusan Vlahovic",  stints = vlahovic_stints, slug = "vlahovic"),
    list(name = "Gonzalo Higuaín", stints = higuain_stints,  slug = "higuain")
  )

  for (pl in players) {
    career_df <- build_career_df(pl$stints, pl$name)
    if (is.null(career_df)) {
      print(paste("No data for", pl$name))
      next
    }

    p        <- build_xg_chart(career_df, pl$name, mode = "cumulative")
    out_path <- here("Viz", glue("xg_cumulative_{pl$slug}.png"))
    ggsave(out_path, p, width = 14, height = 7, dpi = 150, bg = BG_COLOR)
    print(paste("Saved:", basename(out_path)))
  }
}

save_group_charts <- function(by_group) {
  p1 <- build_group_lollipop(by_group)
  ggsave(here("Viz", "group_lollipop.png"),
         p1, width = 10, height = 6, dpi = 150, bg = BG_COLOR)
  print("Saved: group_lollipop.png")

  p2 <- build_group_slope(by_group)
  ggsave(here("Viz", "group_slope.png"),
         p2, width = 10, height = 6, dpi = 150, bg = BG_COLOR)
  print("Saved: group_slope.png")

  p3 <- build_group_bump(by_group)
  ggsave(here("Viz", "group_bump.png"),
         p3, width = 10, height = 6, dpi = 150, bg = BG_COLOR)
  print("Saved: group_bump.png")

  p4 <- build_group_diverging(by_group)
  ggsave(here("Viz", "group_diverging.png"),
         p4, width = 10, height = 6, dpi = 150, bg = BG_COLOR)
  print("Saved: group_diverging.png")
}

save_dumbbell <- function(bunched, decisive) {
  p <- build_dumbbell(bunched, decisive)
  ggsave(here("Viz", "dumbbell_summary.png"),
         p, width = 10, height = 7, dpi = 150, bg = BG_COLOR)
  print("Saved: dumbbell_summary.png")
}

save_beeswarm <- function() {
  p <- build_beeswarm(vlahovic, higuain)
  ggsave(here("Viz", "beeswarm_xg.png"),
         p, width = 12, height = 5, dpi = 150, bg = BG_COLOR)
  print("Saved: beeswarm_xg.png")
}

save_career_trajectory <- function() {
  p <- build_career_trajectory()
  ggsave(here("Viz", "career_trajectory.png"),
         p, width = 14, height = 7, dpi = 150, bg = BG_COLOR)
  print("Saved: career_trajectory.png")
}

# ── Run ────────────────────────────────────────────────────────────────────────

by_group <- read_parquet(here("Data_Player", "comparison_by_group.parquet"))
bunched  <- read_parquet(here("Data_Player", "comparison_bunched.parquet"))
decisive <- read_parquet(here("Data_Player", "comparison_decisive.parquet"))

#save_all_maps()
#save_xg_charts()
#save_group_charts(by_group)
#save_dumbbell(bunched, decisive)
#save_beeswarm()
save_career_trajectory()
