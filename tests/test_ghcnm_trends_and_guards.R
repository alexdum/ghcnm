library(testthat)
suppressPackageStartupMessages({
  library(plotly)
  library(arrow)
  library(dplyr)
})

source("utils/render_time_series_plot.R", local = TRUE)

test_that("evaluate_ghcnm_trend enforces sample size, span, completeness, and gap guards", {
  # 1. Single-year series -> ineligible
  df_one <- data.frame(YEAR = 2000L, VALUE = 12.5)
  res_one <- evaluate_ghcnm_trend(df_one)
  expect_false(res_one$eligible)
  expect_true(is.na(res_one$slope_yr))

  # 2. Sparse multi-decade gaps (only 12 years over 40-year span -> 30% completeness) -> ineligible
  df_sparse <- data.frame(YEAR = seq(1980L, 2020L, by = 4L), VALUE = seq(10, 12, length.out = 11L))
  res_sparse <- evaluate_ghcnm_trend(df_sparse)
  expect_false(res_sparse$eligible)

  # 3. 35 years total across 1980-2025 but with a 12-year gap (1995-2006 missing) -> max_gap = 12 > 10 -> ineligible
  yrs_gap <- c(1980:1994, 2007:2026)
  df_gap <- data.frame(YEAR = yrs_gap, VALUE = seq(10, 11.5, length.out = length(yrs_gap)))
  res_gap <- evaluate_ghcnm_trend(df_gap)
  expect_gt(res_gap$max_gap, 10L)
  expect_false(res_gap$eligible)

  # 4. Short 25-year series (< 30-year span) -> ineligible
  df_short <- data.frame(YEAR = 2001:2025, VALUE = 10 + 0.02 * (0:24))
  res_short <- evaluate_ghcnm_trend(df_short)
  expect_equal(res_short$obs_span, 25L)
  expect_false(res_short$eligible)

  # 5. Valid 35-year series with 90%+ completeness and <= 3-year gap -> eligible & exact slope
  yrs_ok <- setdiff(1990:2024, c(2000L, 2001L))
  df_ok <- data.frame(YEAR = yrs_ok, VALUE = 10 + 0.03 * (yrs_ok - 1990))
  res_ok <- evaluate_ghcnm_trend(df_ok)
  expect_true(res_ok$eligible)
  expect_equal(round(res_ok$slope_yr, 4), 0.0300)
  expect_equal(round(res_ok$slope_decade, 3), 0.300)
})

test_that("render_time_series_plot uses Monthly Mean Temperature, clean units, and omits trend trace when ineligible", {
  tavg_meta <- data.frame(ID = c("TEST01", "TEST02"), NAME = c("Alpha", "Beta"), stringsAsFactors = FALSE)
  prec_meta <- data.frame(ID = "PREC01", NAME = "RainStation", stringsAsFactors = FALSE)
  assign("tavg_meta", tavg_meta, envir = .GlobalEnv)
  assign("prec_meta", prec_meta, envir = .GlobalEnv)
  on.exit({
    if (exists("tavg_meta", envir = .GlobalEnv)) rm("tavg_meta", envir = .GlobalEnv)
    if (exists("prec_meta", envir = .GlobalEnv)) rm("prec_meta", envir = .GlobalEnv)
  }, add = TRUE)

  # Eligible temperature series -> includes Linear Trend trace and °C/decade annotation
  df_ok <- data.frame(YEAR = 1990:2024, VALUE = 10 + 0.025 * (0:34))
  p_ok <- render_time_series_plot(df_ok, "TEST01", "January", "Temperature (°C)", "Monthly Mean Temperature")
  built_ok <- plotly::plotly_build(p_ok)
  trace_names_ok <- vapply(built_ok$x$data, function(tr) tr$name %||% "", character(1))
  expect_true("Linear Trend" %in% trace_names_ok)
  ann_ok <- built_ok$x$layout$annotations[[1]]$text
  expect_true(grepl("Monthly Mean Temperature", built_ok$x$layout$title$text, fixed = TRUE))
  expect_true(grepl("°C/decade", ann_ok, fixed = TRUE))
  expect_true(grepl("°C/yr", ann_ok, fixed = TRUE))
  expect_false(grepl("(°C)", ann_ok, fixed = TRUE))

  # Ineligible short series -> no Linear Trend trace and informative suppressed message
  df_short <- data.frame(YEAR = 2020:2024, VALUE = c(10, 11, 10.5, 11.2, 11.4))
  p_short <- render_time_series_plot(df_short, "PREC01", "July", "Precipitation (mm)", "Total Precipitation")
  built_short <- plotly::plotly_build(p_short)
  trace_names_short <- vapply(built_short$x$data, function(tr) tr$name %||% "", character(1))
  expect_false("Linear Trend" %in% trace_names_short)
  ann_short <- built_short$x$layout$annotations[[1]]$text
  expect_true(grepl("Trend suppressed", ann_short, fixed = TRUE))
  expect_true(grepl("mm", ann_short, fixed = TRUE))
})

test_that("ghcnm server.R removes duplicate plot render, uses Monthly Mean Temperature, and guards async callbacks", {
  server_lines <- readLines("server.R", warn = FALSE)
  server_txt <- paste(server_lines, collapse = "\n")

  expect_equal(length(gregexpr("output\\$time_series_plot\\s*<-\\s*renderPlotly", server_txt)[[1]]), 1L)
  expect_false(grepl("Daily Mean Temperature", server_txt, fixed = TRUE))
  expect_true(grepl("Monthly Mean Temperature", server_txt, fixed = TRUE))
  expect_true(grepl("n_years >= min_years", server_txt, fixed = TRUE))
  expect_true(grepl("ceiling(0.80 * span_years)", server_txt, fixed = TRUE))

  n_later <- length(gregexpr("later::later\\(", server_txt)[[1]])
  n_closed <- length(gregexpr("current_session\\$isClosed\\(\\)", server_txt)[[1]])
  expect_gte(n_closed, n_later + 1L)

  parsed <- tryCatch({ parse(text = server_lines); TRUE }, error = function(e) FALSE)
  expect_true(parsed)
})

test_that("ghcnm server resolves precipitation-only station deep links and schedules map centering", {
  suppressPackageStartupMessages({
    source("global.R")
    ghcnm_server <- source("server.R")$value
  })

  # Case A: Host wrapper sends default parameter=Temperature for precip-only Baghlan (AFXLP338776)
  mock_baghlan <- shiny::MockShinySession$new()
  mock_baghlan$clientData <- shiny::reactiveValues(
    url_search = "?country=Afghanistan&parameter=Temperature&month=January&start=1971&end=1981&station=AFXLP338776"
  )
  shiny::testServer(ghcnm_server, session = mock_baghlan, {
    session$flushReact()

    expect_equal(effective_param(), "Precipitation")
    expect_equal(selected_station_id(), "AFXLP338776")
    expect_equal(as.integer(effective_year_range()), c(1971L, 1981L))
    expect_false(is.null(pending_map_fly()))
    expect_equal(pending_map_fly()$lon, 68.65, tolerance = 0.01)
    expect_equal(pending_map_fly()$lat, 36.10, tolerance = 0.01)

    # Simulate client initial defaults arriving before updateSelectInput/updateSliderInput echo back
    session$setInputs(parameter = "Temperature", month = "January", year_range = c(1961, max(stations_data$last_year, na.rm = TRUE)))
    session$flushReact()

    expect_equal(effective_param(), "Precipitation")
    expect_equal(selected_station_id(), "AFXLP338776")
    ts_df <- time_series_data()
    expect_gt(nrow(ts_df), 0L)
    expect_true(plot_available())
  })

  # Case B: Bare ?station=AFXLP338776 without parameter/start/end adopts station's active period & dataset
  mock_bare <- shiny::MockShinySession$new()
  mock_bare$clientData <- shiny::reactiveValues(url_search = "?station=AFXLP338776")
  shiny::testServer(ghcnm_server, session = mock_bare, {
    session$flushReact()

    expect_equal(effective_param(), "Precipitation")
    expect_equal(selected_station_id(), "AFXLP338776")
    expect_equal(as.integer(effective_year_range()), c(1971L, 1981L))
    expect_true(plot_available())
  })
})

cat("ALL GHCNM TREND & SESSION GUARD TESTS PASSED!\n")
