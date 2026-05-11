# viz_player_comparison.R
# Visualization script for Vlahovic vs Higuain comparison article
# Includes:
#   1. Shot maps (all shots + goals only) — StatsBomb-inspired style
#   2. Shot location heat maps — pure density, no other info
#   3. xG vs Goals charts — cumulative and 15-game rolling

library(pacman)
p_load(dplyr, purrr, arrow, here, stringr, ggplot2, patchwork, glue, scales, zoo)

dir.create(here("Viz"), showWarnings = FALSE)

# ── Config ─────────────────────────────────────────────────────────────────────

CLUB    <- "Juventus"
LEAGUE  <- "serie_a"

VLAHOVIC_SEASONS <- c(2021, 2022, 2023, 2024, 2025)
HIGUAIN_SEASONS  <- c(2016, 2017, 2019)

BG_COLOR    <- "#f0f0f0"
LINE_COLOR  <- "#aaaaaa"
TEXT_COLOR  <- "#000000"

COL_GOAL    <- "#e03131"
COL_SAVED   <- "#4dabf7"
COL_MISSED  <- "#868e96"
COL_BLOCKED <- "#adb5bd"
COL_POST    <- "#f59f00"

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

# ── Load and filter shots ──────────────────────────────────────────────────────

load_shots <- function(seasons, league = "serie_a") {
  map_dfr(seasons, function(s) {
    path <- here("Data_Shot", paste0("shots_", league, "_", s, ".parquet"))
    if (!file.exists(path)) return(NULL)
    read_parquet(path) |> mutate(season = s)
  })
}

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

pitch_background <- function(y_min = Y_MIN) {
  list(
    annotate("rect", xmin = 0, xmax = 68, ymin = y_min, ymax = 105,
             fill = BG_COLOR, color = NA)
  )
}

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
    annotate("point", x = 34, y = 94, color = LINE_COLOR, size = 0.8),
    annotate("path",
             x = 34 + 9.15 * cos(seq(pi * 0.82, pi * 0.18, length.out = 50)),
             y = 94  + 9.15 * sin(seq(pi * 0.82, pi * 0.18, length.out = 50)),
             color = LINE_COLOR, linewidth = lw)
  )
}

pitch_layers <- function(y_min = Y_MIN) {
  c(pitch_background(y_min), pitch_lines(y_min))
}

# ── Legend ─────────────────────────────────────────────────────────────────────

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

build_shot_map <- function(df, title_text = "", goals_only = FALSE) {
  y_min   <- if (goals_only) Y_MIN_GOAL else Y_MIN
  plot_df <- if (goals_only) filter(df, is_goal) else df
  n       <- nrow(plot_df)
  n_goals <- sum(df$is_goal)
  
  subtitle <- if (goals_only) {
    glue("{n} goals (excl. penalties) | Top 5 leagues only")
  } else {
    glue("{n} shots | {n_goals} goals (excl. penalties) | Top 5 leagues only")
  }
  
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

build_heat_map <- function(df, title_text = "", goals_only = FALSE) {
  y_min   <- if (goals_only) Y_MIN_GOAL else Y_MIN
  plot_df <- if (goals_only) filter(df, is_goal) else df
  n       <- nrow(plot_df)
  n_goals <- sum(df$is_goal)
  
  subtitle <- if (goals_only) {
    glue("{n} goals (excl. penalties) | Top 5 leagues only")
  } else {
    glue("{n} shots | {n_goals} goals (excl. penalties) | Top 5 leagues only")
  }
  
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

# ── Save shot + heat maps ──────────────────────────────────────────────────────

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
    type       <- if (m$goals_only) "goals" else "all_shots"
    type_label <- if (m$goals_only) "Goals" else "All Shots"
    h          <- if (m$goals_only) 9 else 11
    
    # Shot map
    p1 <- build_shot_map(m$df,
                         title_text = glue("{m$name} — {type_label}"),
                         goals_only = m$goals_only)
    ggsave(here("Viz", glue("shot_map_{m$slug}_{type}.png")),
           p1, width = 10, height = h, dpi = 150, bg = BG_COLOR)
    print(paste("Saved: shot_map", m$slug, type))
    
    # Heat map
    p2 <- build_heat_map(m$df,
                         title_text = glue("{m$name} — {type_label} (Heat Map)"),
                         goals_only = m$goals_only)
    ggsave(here("Viz", glue("heat_map_{m$slug}_{type}.png")),
           p2, width = 10, height = h, dpi = 150, bg = BG_COLOR)
    print(paste("Saved: heat_map", m$slug, type))
  }
}

# ── Career xG vs Goals ─────────────────────────────────────────────────────────

#' Load shots for one stint and tag with club
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
#' Tags each match with a stint number so return stints are tracked separately
build_career_df <- function(stints, player_name, roll_n = 15) {
  all_shots <- map_dfr(stints, load_stint_shots, player_name = player_name)
  if (is.null(all_shots) || nrow(all_shots) == 0) return(NULL)
  
  per_match <- all_shots |>
    group_by(match_id, date, club) |>
    summarise(goals = sum(is_goal), xg = round(sum(xG), 3), .groups = "drop") |>
    arrange(date) |>
    mutate(
      game_num = row_number(),
      # Stint number — increments each time club changes
      stint    = cumsum(club != lag(club, default = first(club))) + 1,
      cum_goals  = cumsum(goals),
      cum_xg     = round(cumsum(xg), 2),
      # Compute rolling on full career sequence — no per-stint reset
      # NA rows at start of career where window isn't full yet are kept
      # and filtered per segment in build_xg_chart()
      roll_goals = rollmean(goals, k = roll_n, fill = NA, align = "right"),
      roll_xg    = round(rollmean(xg, k = roll_n, fill = NA, align = "right"), 3)
    )
  
  per_match
}

#' Build xG vs Goals chart
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
  
  # Join club colors onto every row
  plot_df <- plot_df |>
    mutate(
      primary_col   = map_chr(club, ~ club_colors[[.x]]$primary),
      secondary_col = map_chr(club, ~ club_colors[[.x]]$secondary)
    ) |>
    filter(!is.na(g_line), !is.na(xg_line))
  
  p <- ggplot() + theme_void()
  
  # Split into stint runs, overlapping by 1 row to eliminate gaps
  runs <- rle(plot_df$stint)
  idx  <- cumsum(c(1, runs$lengths))
  
  for (k in seq_along(runs$lengths)) {
    start_i <- idx[k]
    end_i   <- min(idx[k + 1], nrow(plot_df))  # overlap: include first row of next stint
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
  
  # Lines across full career
  p <- p +
    geom_line(data = plot_df, aes(x = date, y = g_line),
              color = TEXT_COLOR, linewidth = 0.8) +
    geom_line(data = plot_df, aes(x = date, y = xg_line),
              color = TEXT_COLOR, linewidth = 0.8, linetype = "dashed")
  
  # Club transition lines + labels
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

#' Save all xG charts
save_xg_charts <- function() {
  players <- list(
    list(name = "Dusan Vlahovic",  stints = vlahovic_stints,
         slug = "vlahovic"),
    list(name = "Gonzalo Higuaín", stints = higuain_stints,
         slug = "higuain")
  )
  
  for (pl in players) {
    career_df <- build_career_df(pl$stints, pl$name)
    if (is.null(career_df)) { print(paste("No data for", pl$name)); next }
    
    p        <- build_xg_chart(career_df, pl$name, mode = "cumulative")
    out_path <- here("Viz", glue("xg_cumulative_{pl$slug}.png"))
    ggsave(out_path, p, width = 14, height = 7, dpi = 150, bg = BG_COLOR)
    print(paste("Saved:", basename(out_path)))
  }
}

# ── Run ────────────────────────────────────────────────────────────────────────

save_all_maps()
save_xg_charts()
