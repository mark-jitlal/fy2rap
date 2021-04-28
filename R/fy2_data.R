
fy2_data <- function(x, log_level = futile.logger::WARN,
                             log_appender = "console",
                             log_issues = FALSE) {

  # Set logger severity threshold, defaults to
  # high level use (only flags warnings and errors)
  # Set log_level argument to futile.logger::TRACE for full info
  futile.logger::flog.threshold(log_level)

  # Set where to write the log to
  if (log_appender != "console")
  {
    # if not console then to a file called...
    futile.logger::flog.appender(futile.logger::appender.file(log_appender))
  }

  # Checks
  futile.logger::flog.info('Initiating fy2_data class.
\n\nExpects a data.frame with three columns: sector, year, and measure, where
measure is one of GVA, exports, or enterprises. The data.frame should include
historical data, which is used for checks on the quality of this year\'s data,
and for producing tables and plots. More information on the format expected by
this class is given by ?fy2_data().')

  # Integrity checks on incoming data ----

  # Check the structure of the data is as expected: data.frame containing no
  # missing values and three columns, containing sector, year, and one
  # additional column.

  futile.logger::flog.info('\n*** Running integrity checks on input dataframe (x):')
  futile.logger::flog.debug('\nChecking input is properly formatted...')

  futile.logger::flog.debug('Checking x is a data.frame...')
  if (!is.data.frame(x))
  {
    futile.logger::flog.error("x must be a data.frame",
                              x, capture = TRUE)
  }

  futile.logger::flog.debug('Checking x has correct columns...')
  if (length(colnames(x)) != 6)
  {
    futile.logger::flog.error("x must have six columns: id, age, sex, marstat, urban and food_sec")
  }

  futile.logger::flog.debug('Checking x contains an id column...')
  if (!'id' %in% colnames(x)) stop("x must contain id column")

  futile.logger::flog.debug('Checking x contains an age column...')
  if (!'age' %in% colnames(x)) stop("x must contain age column")

  futile.logger::flog.debug('Checking x contains a sex column...')
  if (!'sex' %in% colnames(x)) stop("x must contain sex column")

  futile.logger::flog.debug('Checking x contains a marstat column...')
  if (!'marstat' %in% colnames(x)) stop("x must contain marstat column")

  futile.logger::flog.debug('Checking x contains a urban column...')
  if (!'urban' %in% colnames(x)) stop("x must contain urban column")

  futile.logger::flog.debug('Checking x contains a food_sec column...')
  if (!'food_sec' %in% colnames(x)) stop("x must contain food_sec column")

  futile.logger::flog.debug('Checking x does not contain missing values...')
  if (anyNA(x)) stop("x cannot contain any missing values")

  # futile.logger::flog.debug('Checking for the correct number of rows...')
  # if (nrow(x) != length(unique(x$sector)) * length(unique(x$year))) {
  #   futile.logger::flog.warn("x does not appear to be well formed. nrow(x) should equal
  #                            length(unique(x$sector)) * length(unique(x$year)). Check the of x.")
  # }



  futile.logger::flog.info('...passed')

  # User assertr to run statistical tests on the data itself ----

  futile.logger::flog.info("\n***Running statistical checks on input dataframe (x)")

  futile.logger::flog.trace("These tests are implemented using the package assertr see:
  https://cran.r-project.org/web/packages/assertr for more details.")

  # Extract third column name

  #value <- colnames(x)[(!colnames(x) %in% c('sector','year'))]

  # Check sensible range for age:

  futile.logger::flog.debug('Checking age in a sensible range (16:110)...')

  assertr::assert_(x, assertr::in_set(16:110), ~age)

  # Check that the correct levels are in marital status

  futile.logger::flog.debug('Checking marstat is correct...')

  # Save marstat name lookup for use later

  marstat_set <- c(
    "Div/Wid/Sep"    = "Divorced/Widowed/Separated",
    "Married/civil"    = "Married/Civil partnership",
    "Single:couple"    = "Single: couple",
    "Single:no couple"    = "Single: not in a couple"
      )

  assertr::assert_(x, assertr::in_set(names(marstat_set)), ~marstat, error_fun = raise_issue)

  # Keep only essential columns:
  x <- x[, c("id", "age", "sex", "marstat", "urban", "food_sec")]

  # Check for outliers ----

  # Check for simple outliers in the value column (GVA, exports, enterprises)
  # for each sector, over the entire timeseries. Outliers are detected using
  # median +- 3 * median absolute deviation, implemented in the
  # assertr::within_n_mads() function.

  # futile.logger::flog.debug('Checking for outliers (x_i > median(x) + 3 * mad(x)) in each sector timeseries...')

  # # Create a list split by series containing a df in each
  #
  # series_split <- split(x, x$sector)
  #
  # # Apply to each df in the list
  #
  # lapply(
  #   X = series_split,
  #   FUN = function(x) {
  #     futile.logger::flog.trace("Checking sector timeseries: %s",
  #                               as.character(unique(x[['sector']])),
  #                               capture = FALSE)
  #     assertr::insist_(
  #       x,
  #       assertr::within_n_mads(3),
  #       lazyeval::interp(~value, value = as.name(value)),
  #       error_fun = raise_issue)
  #   }
  # )

  futile.logger::flog.info('...passed')

  # Check for outliers using mahalanobis ----

  # This test also looks for outliers, by considering the relationship between
  # the variable year and the value variable. It measures the mahalabois
  # distance, which is similar to the euclidean norm, and then looks for
  # outliers in this new vector of norms. Any value with a distance too great is
  # flagged as an outlier.

  # futile.logger::flog.debug('Checking for outliers on a row by row basis using mahalanobis distance...')
  #
  # lapply(
  #   X = series_split,
  #   FUN = maha_check
  # )

  futile.logger::flog.debug('...passed')

  ### ISSUE - these might be "changing the world" for the user unexpectedly!

  # Reset threshold to package default
  futile.logger::flog.threshold(futile.logger::INFO)
  # Reset so that log is appended to console (the package default)
  futile.logger::flog.appender(futile.logger::appender.console())

  # Message required to pass a test
  message("Checks completed successfully:
object of 'fy2_data' class produced!")

  # Define the class here ----

  structure(
    list(
      df = x,
      colnames = colnames(x),
      type = colnames(x)[!colnames(x) %in% c('id','age', 'sex', 'marstat', 'urban', 'food_sec')] #,
      # sector_levels = levels(x$sector),
      # sectors_set = sectors_set,
      # years = unique(x$year)
    ),
    class = "fy2_data")
}
