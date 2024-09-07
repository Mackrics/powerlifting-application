# Group exercises by muscle group ----------------------------------------------
group_exercises <- function(data) {
  data[,let(
      group = case_match(exercise,
        c("squat", "3xx-squat", "31x squat", "lowbar squat", "slsquat", "belt squat") ~ "Legs",
        c("close grip bench press", "bench press", "db-bench", "flies", "fu bench press") ~ "Chest",
        c("deadlift", "pause-deadlift", "back raise") ~ "Hamstrings",
        c("bbrow", "seal row", "barbell row") ~ "Back",
        c("db-side lateral raise", "side lateral raise", "bb-curl", "french-press", "tricep pushdown", "dumbbell curl") ~ "Arms"
      )
    )
  ][]
}

dt_enframe <- function(vec, name = "name", value = "value") {
  framed <-
    data.table(
      name = names(vec),
      value = vec
    )
  setnames(framed, old = c("name", "value"), new = c(name, value))
  return(framed)
}

# Get new data ----------------------------------------------------------------
refresh_data <- function() {
  dir_ls(
    "./data-raw",
    recurse = TRUE,
    regexp = "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
  ) |>
  map(fread) |>
  dt_enframe(name = "path", value = "data") |>
  _[, let(
    date = as.Date(str_extract(path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
    cycle = str_extract(path, "[0-9]{3}_cycle") |>
	    str_extract("[0-9]{3}") |>
	    as.numeric(),
    data = map(data, \(x) x[, let(weight = as.double(weight), rpe = as.double(rpe))])
  )]  |>
  _[, rbindlist(data), by = .(date, cycle)] |>
  _[, let(
    exercise = exercise |>
      str_replace_all("seal", "seal row") |>
      str_replace_all("^bench$", "bench press") |>
      str_replace_all("^cg-bench$", "close grip bench press") |>
      str_replace_all("^lb-squat$", "lowbar squat") |>
      str_replace_all("^french press$", "french press") |>
      str_replace_all("^tricep$", "tricep pushdown") |>
      str_replace_all("slr", "side lateral raise") |>
      str_replace_all("belt-squat", "belt squat") |>
      str_replace_all("fu-bench", "fu bench press") |>
      str_replace_all("db-curl", "dumbbell curl") |>
      str_replace_all("back-raise", "back raise") |>
      str_replace_all("31x-squat", "31x squat") |>
      str_replace_all("bb-row", "barbell row"),
     e1rm = weight * (1 + (0.033 * reps))
  )] |>
  group_exercises()
}

# Summarize data per workout ---------------------------------------------------
get_overview <- function(data, by = c("date", "exercise")) { 
  data[, .(
    sets   = .N,
    volume = sum(weight * reps)/1000,
    reps   = sum(reps),
    max    = max(weight),
    e1rm   = max(e1rm),
    cycle  = unique(cycle)
    ),
    by = by
  ][]
}


# Evolution of volume ----
plot_volume <- function(
    data,
    groups,
    start_date = Sys.Date() - 90,
    end_date = Sys.Date(),
    cycles = get_cycles()
  ) {
  data[group %in% groups] |>
  get_overview(by = c("date", "group")) |>
  fill_dates(group = "group") |>
  _[, list(
    volume = sum(replace_na(volume, 0)),
    cycle = unique(cycle)
    ),
    by = list(date, group)
  ] |>
  _[, let(
      volume  = rollsum(replace_na(volume, 0), k = 10, na.pad = TRUE, align = "right")
    ),
    by = group,
  ] |>
  _[(date >= start_date & date <= end_date) & (cycle %in% cycles)] |>
  ggplot() +
  aes(date, volume, color = group) +
  geom_line(linewidth = 1) +
  theme_ctp_grid(ctp_mocha) +
  scale_color_ctp(ctp_mocha) +
  labs(x = "", y = "", title = "Evolution of volume (10 day rolling sum)", color = "") +
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
  geom_line(linewidth = 1) +
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
  _[, .(
      volume = sum(volume, na.rm = TRUE),
      cycle = unique(cycle)
    ),
    by = date
  ] |>
  _[,
    let(
      cumvol7  = rollsum(replace_na(volume, 0), k = 7, na.pad = TRUE, align = "right"),
      cumvol14 = rollsum(replace_na(volume, 0), k = 14, na.pad = TRUE, align = "right")
    )
  ] |>
  _[(date >= start_date & date <= end_date) & (cycle %in% cycles), 
    .SD,
    .SDcols = patterns("^date$|^cumvol*")
  ] |>
  pivot_longer(-date, names_prefix = "cumvol", names_to = "days", values_to = "rollsum") |>
  ggplot() +
  aes(date, rollsum, color = days) +
  geom_line(linewidth = 1) +
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
  data[
    (date >= start_date & date <= end_date) &
    (exercise %in% exercises) &
    (cycle %in%  cycles)
  ] |>
  get_overview() |>
  _[,
     map(.SD, \(x) round(x, 1)),
    .SDcols = is.numeric,
    by = .(date, exercise)
  ] |>
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
  data.table(
    date   = seq.Date(from = data[, min(date)], to = Sys.Date(), by = "1 day"),
    group = list(data[,unique(.SD), .SDcols = group])
  ) |>
  _[, .(group = unlist(group)), by = date] |>
  setnames(old = "group", new = group) |>
  _[data, on = c("date", group)] |>
  fill(cycle)
}
