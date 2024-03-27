# Group exercises by muscle group ----------------------------------------------
group_exercises <- function(data) {
  data |>
    mutate(
      group = case_match(exercise,
        c("squat", "lowbar squat", "slsquat") ~ "Legs",
        c("close grip bench press", "bench press", "db-bench", "flies") ~ "Chest",
        c("deadlift") ~ "Hamstrings",
        c("bbrow", "seal row") ~ "Back",
        c("db-side lateral raise", "side lateral raise", "bb-curl", "french-press", "tricep pushdown") ~ "Arms"
      )
    )
}

# Get new data ----------------------------------------------------------------
refresh_data <- function() {
  dir_ls("./data-raw", recurse = TRUE) |>
  (\(x) x[str_detect(x, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")]) () |>
  map(\(x) read_delim(x, delim = " ", show_col_types = FALSE)) |>
  enframe(name = "path") |>
  mutate(
    date = str_extract(path, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]") |>
      as.Date(),
    cycle = str_extract(path, "[0-9][0-9][0-9]_cycle") |>
	    str_extract("[0-9][0-9][0-9]") |>
	    as.numeric()
  ) |>
  unnest(value) |>
  mutate(
    exercise = exercise |>
    str_replace_all("seal", "seal row") |>
    str_replace_all("^bench$", "bench press") |>
    str_replace_all("^cg-bench$", "close grip bench press") |>
    str_replace_all("^lb-squat$", "lowbar squat") |>
    str_replace_all("^french press$", "french press") |>
    str_replace_all("^tricep$", "tricep pushdown") |>
    str_replace_all("slr", "side lateral raise")
  ) |>
  select(cycle, date, exercise, weight, reps, rpe) |>
  group_exercises()
}

# Summarize data per workout ---------------------------------------------------
get_overview <- function(data, by = c(date, exercise)) { 
  data |>
  mutate(e1rm = weight * (1 + (0.033 * reps))) |>
  summarize(
    .by = {{ by }},
    sets = n(),
    volume = sum(weight * reps)/1000,
    reps = sum(reps),
    max = max(weight),
    e1rm = max(e1rm),
    cycle = unique(cycle)
  ) |>
  select({{ by }}, cycle, sets, reps, volume, everything())
}

# Evolution of volume ----
plot_volume <- function(
    data,
    groups,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    cycles = get_cycles()
  ) {
  data |>
  filter(group %in% groups) |>
  get_overview(by = c(date, group)) |>
  fill_dates(group = "group") |>
  summarize(
    .by = c(date, group),
    volume = sum(replace_na(volume, 0)),
    cycle = unique(cycle)
  ) |>
  mutate(
    .by = group,
    volume  = rollsum(replace_na(volume, 0), k = 7, na.pad = TRUE, align = "right")
  ) |>
  filter(date >= start_date & date <= end_date) |>
  filter(cycle %in% cycles) |>
  filter(cycle %in% cycles) |>
  ggplot() +
  aes(date, volume, color = group) +
  geom_line() +
  theme_ctp_grid(ctp_mocha) +
  scale_color_ctp(ctp_mocha) +
  labs(x = "", y = "", title = "Evolution of volume (7 day rolling sum)", color = "") +
  theme(text = element_text(size = 18))
}

# Evolution of e1rm ----
plot_e1rm <- function(
    data,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    exercises = c("bench press", "squat"),
    cycles = get_cycles()
  ) {
  data |>
  filter(date >= start_date & date <= end_date) |>
  filter(exercise %in% {{ exercises }}) |>
  filter(cycle %in% cycles) |>
  get_overview() |>
  ggplot() +
  aes(date, e1rm, color = exercise) +
  geom_point() +
  geom_line() +
  theme_ctp_grid(ctp_mocha) +
  scale_color_ctp(ctp_mocha) +
  labs(
    x = "",
    y = "",
    title = "Evolution of maximum estimated one rep max",
    color = ""
  ) +
  theme(text = element_text(size = 18))
}

max_table <- function(
    data,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    exercises = c("bench press", "squat"),
    cycles = get_cycles()
  ) {
  data |>
  filter(date >= start_date & date <= end_date) |>
  filter(exercise %in% exercises) |>
  filter(cycle %in% cycles) |>
  summarize(
    .by = c(exercise, reps),
    max = max(weight),
    rpe = min(rpe[weight == max(weight)])
  ) |>
  mutate(
    max = str_glue("{max} @ rpe {rpe}")
  ) |>
  select(-rpe) |>
  pivot_wider(
    names_from  = exercise,
    values_from = max
  ) |>
  right_join(
    tibble(reps = 1:10)
  ) |>
  arrange(reps) |>
  gt() |>
  tab_header("Max weight per repetitions") |>
  gt_theme_ctp(ctp_mocha) |>
  tab_options(
    table.font.name = "IBM Plex Mono"
  )
}


# Evolution of total volume ----

plot_total_volume <- function(
    data,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    cycles = get_cycles()
  ) {
  data |>
  get_overview() |>
  fill_dates() |>
  summarize(
    .by = date,
    volume = sum(volume, na.rm = TRUE),
    cycle = unique(cycle)
  ) |>
  mutate(
    cumvol7  = rollsum(replace_na(volume, 0), k = 7, na.pad = TRUE, align = "right"),
    cumvol14 = rollsum(replace_na(volume, 0), k = 14, na.pad = TRUE, align = "right"),
  ) |>
  filter(date >= start_date & date <= end_date) |>
  filter(cycle %in% cycles) |>
  select(date, starts_with("cumvol")) |>
  pivot_longer(-date, names_prefix = "cumvol", names_to = "days", values_to = "rollsum") |>
  ggplot() +
  aes(date, rollsum, color = days) +
  geom_line() +
  theme_ctp_grid(ctp_mocha) +
  scale_color_ctp(ctp_mocha) +
  labs(x = "", y = "", title = "Evolution of total volume (rolling sum)") +
  theme(text = element_text(size = 18)) +
  scale_color_ctp(ctp_mocha) 
}

show_meso <- function(
    data,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    exercises = c("bench press", "squat"),
    cycles = get_cycles()
  ) {
  data |>
  filter(date >= start_date & date <= end_date) |>
  filter(exercise %in% exercises) |>
  filter(cycle %in%  cycles) |>
  get_overview() |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = \(x) round(x, 1)
    )
  ) |>
  gt() |>
  gt_theme_ctp(ctp_mocha) |>
  tab_options(
    table.font.name = "IBM Plex Mono"
  ) |>
  tab_header("Meso details")
}

get_exercises <- function() {
  read_feather("./data/lifting-data.arrow") |>
    distinct(exercise) |>
    pull()
}

get_groups <- function() {
  read_feather("./data/lifting-data.arrow") |>
    distinct(group) |>
    pull()
}

get_cycles <- function() {
  read_feather("./data/lifting-data.arrow") |>
    distinct(cycle) |>
    pull()
}

fill_dates <- function(data, group = "exercise") {
  min_date = min(data$date)
  tibble(
    date         = seq.Date(from = min_date, to = Sys.Date(), by = "1 day"),
    !!group     := list(unique(data[[ group ]]))
  ) |>
  unnest(!!group) |>
  left_join(data, join_by(date, !!group)) |>
  fill(cycle)
}
