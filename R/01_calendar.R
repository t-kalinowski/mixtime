#' Create a new calendar
#'
#' Define a new calendar as a collection of time units. Calendars are the
#' foundation for representing dates and times in terms of human-readable
#' components like years, months, days, hours, minutes, and seconds. Each
#' calendar is defined by specifying the time units it contains, which
#' determine how time values can be interpreted and manipulated.
#'
#' Time units are typically S7 class definitions that inherit from [`mt_unit`]
#' for standard units, [`mt_tz_unit`] for timezone-aware units (civil time), or 
#' [`mt_loc_unit`] for location-aware units (astronomical time). The calendar
#' object provides a namespace for accessing these unit constructors and
#' defines the relationships between them for calendar arithmetic.
#'
#' @param ... Named time unit class definitions. Each argument should be a
#'   time unit class (typically created with `S7::new_class()`) that inherits
#'   from `mt_unit` or `mt_tz_unit`. The names define the calendar's fields
#'   and are used to access unit constructors (e.g., `calendar$year()`).
#' @param inherit Optional calendar to inherit time units from. Units
#'   defined in `...` will override inherited units with the same name.
#' @param class Character vector of additional classes for the calendar object.
#'
#' @details
#' Each calendar contains its own subclasses of the supplied time units.
#' Methods for the supplied units are inherited, and each constructed granule
#' retains its calendar when reconstructed with `S7::S7_class()` or serialized.
#'
#' @return A calendar object of class `c(class, "mt_calendar")`, consisting of a
#'   named list containing the specified time unit classes.
#'
#' @seealso [linear_time()], [cyclical_time()]
#'
#' @examples
#' # Create a simple calendar with year and month units
#' # (inheriting from civil time units for day, hour, minute, second, ...)
#' cal_simple <- new_calendar(
#'   year = new_time_unit("tu_year", parent = mt_tz_unit),
#'   month = new_time_unit("tu_month", parent = mt_tz_unit),
#'   inherit = cal_time_civil,
#'   class = "cal_simple"
#' )
#'
#' # Create time granules from the calendar
#' cal_simple$year(1L)
#' cal_simple$month(1L)
#' 
#' @importFrom rlang list2
#' @export
new_calendar <- function(..., inherit = NULL, class = character()) {
  vctrs::vec_assert(class, character())
  time_units <- list2(...)
  if (!is.null(inherit)) {
    # Add units from inherit that aren't already in time_units
    new_names <- setdiff(names(inherit), names(time_units))
    time_units <- c(time_units, inherit[new_names])
  }
  
  cal <- structure(
    time_units,
    class = c(class, "mt_calendar")
  )
  
  # Each calendar owns subclasses, including for units it inherits or reuses.
  # Keep the calendar and shared unit definition in a declared getter so that
  # class reconstruction and serialization preserve their context.
  contexts <- vector("list", length(cal))
  calendar_id <- if (length(class) && isNamespace(parent.frame())) {
    class[[1L]]
  } else {
    prefix <- if (length(class)) class[[1L]] else "calendar"
    basename(tempfile(paste0(prefix, "_")))
  }
  for (i in seq_along(cal)) {
    context <- new.env(parent = emptyenv())
    context$unit <- unit_definition(cal[[i]])
    contexts[[i]] <- context
    getter <- local({
      context <- context
      function(self) context
    })
    cal[[i]] <- S7::new_class(
      name = paste(calendar_id, names(cal)[[i]], sep = "_"),
      parent = cal[[i]],
      package = topNamespaceName(parent.frame()),
      properties = list(
        .calendar = S7::new_property(
          class = S7::class_environment,
          getter = getter
        )
      )
    )
  }
  for (context in contexts) {
    context$calendar <- cal
  }
  
  cal
}

#' @exportS3Method base::print mt_calendar
print.mt_calendar <- function(x, ...) {
  # Get calendar class (excluding mt_calendar base class)
  cal_classes <- setdiff(class(x), "mt_calendar")
  
  # Header
  if (length(cal_classes) > 0) {
    cat("<", paste(cal_classes, collapse = ", "), ">\n", sep = "")
  } else {
    cat("<mt_calendar>\n")
  }
  
  # List time units
  if (length(x) > 0) {
    cat("Time units:\n")
    for (name in names(x)) {
      cat("  - ", name, "\n", sep = "")
    }
  } else {
    cat("No time units defined\n")
  }
  
  invisible(x)
}

#' Obtain the calendar of a time object
#' 
#' This S7 generic function extracts the calendar system from a time object.
#' The calendar defines the collection of time units (years, months, days, etc.)
#' used to interpret the time representation.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A calendar object (e.g., `cal_gregorian`, `cal_isoweek`)
#' 
#' @examples
#' 
#' # The calendar of a Date object is the Gregorian calendar
#' time_calendar(Sys.Date())
#' 
#' # The calendar of a POSIXct object is also Gregorian
#' time_calendar(Sys.time())
#' 
#' # The calendar of a yearweek object is the ISO week calendar
#' time_calendar(yearweek(Sys.Date()))
#' 
#' # A mixed time object returns a list of calendars
#' time_calendar(c(yearmonth(Sys.Date()), Sys.Date()))
#' 
#' @export
time_calendar <- S7::new_generic("calendar", c("x"))

method(time_calendar, S7::class_Date) <- function(x) cal_gregorian
method(time_calendar, S7::class_POSIXt) <- function(x) cal_gregorian
method(time_calendar, mt_unit) <- function(x) {
  if (S7::prop_exists(x, ".calendar")) x@.calendar$calendar
}
method(time_calendar, class_mixtime) <- function(x) {
  time_calendar(chronon_glb(x))
}
method(time_calendar, mt_time) <- function(x) time_calendar(time_chronon(x))
method(time_calendar, S7::class_any) <- function(x) cal_gregorian

# The calendar subclasses share the identity of the supplied time unit for
# graph topology. Their actual classes still determine arithmetic dispatch.
unit_definition <- function(x) {
  if (inherits(x, "S7_class")) {
    property <- x@properties[[".calendar"]]
    if (is.null(property)) x else property$getter(NULL)$unit
  } else {
    if (S7::prop_exists(x, ".calendar")) x@.calendar$unit else S7::S7_class(x)
  }
}
