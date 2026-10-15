test_that("calendar units retain their calendar through class reconstruction", {
  for (calendar in list(cal_gregorian, cal_isoweek)) {
    unit <- calendar$day(n = 1L)
    expect_identical(time_calendar(unit), calendar)
    expect_identical(time_calendar(S7::S7_class(unit)(n = 2L)), calendar)
  }

  expect_equal(
    chronon_format_linear(cal_gregorian$day(n = 1L)),
    "{lin(year)}-{cyc(month,year)}-{cyc(day,month)}"
  )
  expect_equal(
    format(time_parse("2024-02-15", chronon = cal_gregorian$day(n = 1L))),
    "2024-02-15"
  )
  expect_equal(
    format(time_parse("2024-W07-Thu", chronon = cal_isoweek$day(n = 1L))),
    "2024-W07-Thu"
  )
})

test_that("calendar inheritance gives each calendar its own unit classes", {
  first <- new_calendar(inherit = cal_gregorian, class = "cal_custom")
  second <- new_calendar(inherit = cal_gregorian, class = "cal_custom")
  explicit <- new_calendar(day = cal_gregorian$day, class = "cal_explicit")

  for (calendar in list(first, second, explicit)) {
    unit <- calendar$day(n = 1L)
    expect_identical(time_calendar(unit), calendar)
    expect_identical(time_calendar(S7::S7_class(unit)(n = 2L)), calendar)
  }
  expect_identical(time_calendar(cal_gregorian$day(n = 1L)), cal_gregorian)
  expect_equal(
    chronon_cardinality(first$second(n = 1L), first$day(n = 1L)),
    86400
  )
  expect_equal(
    chronon_divmod(first$second(n = 1L), first$day(n = 1L), 86401L),
    list(div = 1, mod = 1)
  )
  expect_equal(
    chronon_divmod(first$day(n = 1L), cal_isoweek$day(n = 1L), 123L),
    list(div = 123L, mod = 0L)
  )
})

test_that("calendar subclasses inherit multi-step arithmetic", {
  `:=` <- S7::`:=`
  tu_custom_day := S7::new_class(parent = cal_gregorian$day)
  calendar <- new_calendar(day = tu_custom_day, inherit = cal_gregorian)

  expect_identical(time_calendar(calendar$day(n = 1L)), calendar)
  expect_equal(
    chronon_cardinality_fixed(calendar$second(n = 2L), calendar$day(n = 1L)),
    43200
  )
  expect_equal(
    chronon_cardinality_fixed(calendar$day(n = 1L), calendar$second(n = 2L)),
    1 / 43200
  )
  expect_equal(
    chronon_divmod(calendar$day(n = 1L), calendar$second(n = 1L), 2L),
    list(div = 172800, mod = 0L)
  )
})

test_that("shared calendar units retain their common granularity", {
  x <- c(
    mt_linear(.data = 0L, chronon = cal_gregorian$day(n = 1L)),
    mt_linear(.data = 0L, chronon = cal_isoweek$day(n = 1L))
  )
  expect_equal(chronon_glb(x)@n, 1L)
  expect_equal(time_unit_abbr(chronon_glb(x)), "D")
  expect_equal(time_unit_abbr(chronon_lub(x)), "D")
  expect_equal(
    chronon_divmod(cal_gregorian$month(n = 1L), cal_isoweek$week(n = 1L), 0L),
    list(div = 0, mod = 3 / 31)
  )
})

test_that("shared calendar units retain cyclical formats and labels", {
  expect_equal(format(day_of_week(as.Date("2024-02-15"))), "Thu")
  expect_equal(
    cyclical_labels_format(
      cal_gregorian$day(n = 1L),
      cal_isoweek$week(n = 1L),
      3L,
      label = TRUE
    ),
    "Thu"
  )
})

test_that("multi-step arithmetic uses calendar-specific methods", {
  calendar <- new_calendar(inherit = cal_time_civil, class = "cal_short_hour")
  S7::method(chronon_cardinality_fixed, list(calendar$minute, calendar$hour)) <-
    function(x, y) 30L

  expect_equal(
    chronon_cardinality_fixed(calendar$second(n = 1L), calendar$hour(n = 1L)),
    1800
  )
  expect_equal(
    chronon_cardinality(calendar$hour(n = 1L), calendar$second(n = 1L)),
    1 / 1800
  )
  expect_equal(
    chronon_divmod(calendar$second(n = 1L), calendar$hour(n = 1L), 1801L),
    list(div = 1, mod = 1)
  )
  expect_equal(
    chronon_cardinality_fixed(
      cal_time_civil$second(n = 1L),
      cal_time_civil$hour(n = 1L)
    ),
    3600
  )
})
