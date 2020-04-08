#' Check if Mdl is available on load
.onLoad <- function(libname, pkgname) {
  # Check if Mdl tools is available
  if (Sys.which("mdl") == "") {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }

  if (system("mdl", ignore.stdout = TRUE, ignore.stderr = TRUE) != 1) {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }
}

#' Turn identifiers into syntactically valid R names
.clean_names <- function(x) {
  x <- gsub("\\!", "exc", x)
  x <- gsub("\\%", "pct", x)
  x <- gsub("\\$", "usd", x)
  return(x)
}

#' Revert name cleanup
.revert_names <- function(x) {
  x <- gsub("exc", "\\!", x)
  x <- gsub("pct", "\\%", x)
  x <- gsub("usd", "\\$", x)
  return(x)
}

#' Convert Mdl date string into an R Date object
.oe_date <- function(x) {
  x[!is.na(x)] <- paste0(substr(x[!is.na(x)], 1, 4), "-",
                         as.numeric(substr(x[!is.na(x)], 6, 6)) * 3 - 2, "-01")
  x <- as.Date(x)
  return(x)
}

#' Return Oxford Economics colour palette
#'
#' Returns Oxford Economics colour palette. The palette includes 15 unique
#' colours.
#' @return Returns Oxford Economics colour palette.
#' @export
#' @examples
#' oe_palette()
oe_palette <- function() {
  oecols <- c(paste0("#", paste(as.hexmode(c(189, 27, 33)), collapse = "")),
              paste0("#", paste(as.hexmode(c(0, 52, 105)), collapse = "")),
              paste0("#", paste(as.hexmode(c(209, 162, 30)), collapse = "")),
              paste0("#", paste(as.hexmode(c(123, 124, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(150, 87, 147)), collapse = "")),
              paste0("#", paste(as.hexmode(c(0, 173, 220)), collapse = "")),
              paste0("#", paste(as.hexmode(c(0, 121, 63)), collapse = "")),
              paste0("#", paste(as.hexmode(c(222, 99, 40)), collapse = "")),
              paste0("#", paste(as.hexmode(c(75, 199, 231)), collapse = "")),
              paste0("#", paste(as.hexmode(c(173, 224, 242)), collapse = "")),
              paste0("#", paste(as.hexmode(c(169, 166, 162)), collapse = "")),
              paste0("#", paste(as.hexmode(c(208, 205, 201)), collapse = "")),
              paste0("#", paste(as.hexmode(c(197, 137, 48)), collapse = "")),
              paste0("#", paste(as.hexmode(c(218, 177, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(233, 212, 180)), collapse = "")))
  return(oecols)
}

#' Return Oxford Economics sector names and corresponding ISO codes
#'
#' Returns a data frame containing Oxford Economics sector names and
#' corresponding ISO 3-character code.
#' @return Data frame containing Oxford Economics sector names and corresponding
#' ISO 3-character code.
#' @export
#' @examples
#' oe_macromappings()
oe_macromappings <- function() {
  tab <- rbind(c("SAUDARNC", "P29"),
               c("SAUDEM", "P30"),
               c("SAULAB", "P31"),
               c("SAUSUBN", "P32"),
               c("SAUGROIL", "P40"),
               c("ADVANECO", "A05"),
               c("AFRICA", "AFR"),
               c("ASP", "ASIAPAC"),
               c("BRIC", "BRC"),
               c("EASTEUR", "EEU"),
               c("EMERGMAR", "A06"),
               c("EU", "EUR"),
               c("EURO_11", "EUZ"),
               c("GCC", "A18"),
               c("LATAMER", "LAT"),
               c("OPEC", "OPC"),
               c("RESTOECD", "ROD"),
               c("RESTWORL", "RWD"),
               c("WORLD", "WLD"),
               c("CANADA", "CAN"),
               c("MEXICO", "MEX"),
               c("US", "USA"),
               c("AUSTRIA", "AUT"),
               c("BELGIUM", "BEL"),
               c("DENMARK", "DNK"),
               c("FINLAND", "FIN"),
               c("FRANCE", "FRA"),
               c("GERMANY", "DEU"),
               c("GREECE", "GRC"),
               c("IRELAND", "IRL"),
               c("ITALY", "ITA"),
               c("NETH", "NLD"),
               c("NORWAY", "NOR"),
               c("PORTUGAL", "PRT"),
               c("SPAIN", "ESP"),
               c("SWEDEN", "SWE"),
               c("SWITZ", "CHE"),
               c("UK", "GBR"),
               c("RUSSIA", "RUS"),
               c("BULGARIA", "BGR"),
               c("CROATIA", "HRV"),
               c("CZECH", "CZE"),
               c("HUNGARY", "HUN"),
               c("POLAND", "POL"),
               c("ROMANIA", "ROU"),
               c("SLOVAKIA", "SVK"),
               c("TURKEY", "TUR"),
               c("AUSTRALI", "AUS"),
               c("CHINA", "CHN"),
               c("HK", "HKG"),
               c("INDIA", "IND"),
               c("INDONESI", "IDN"),
               c("JAPAN", "JPN"),
               c("MALAYSIA", "MYS"),
               c("PHILIPPI", "PHL"),
               c("SINGPORE", "SGP"),
               c("KOREA", "KOR"),
               c("TAIWAN", "TWN"),
               c("THAILAND", "THA"),
               c("ARGENTIN", "ARG"),
               c("BRAZIL", "BRA"),
               c("SAUDISR", "SAU"),
               c("UAEMOD", "ARE"),
               c("SAFRICA", "ZAF"),
               c("CYPRUS", "CYP"),
               c("ESTONIA", "EST"),
               c("LATVIA", "LVA"),
               c("LITH", "LTU"),
               c("SLOVENIA", "SVN"),
               c("VIETNAM", "VNM"),
               c("VENEZUEL", "VEN"),
               c("IRANSR", "IRN"),
               c("IRAQ_ANN", "IRQ"),
               c("KUWAIT", "KWT"),
               c("QATAR", "QAT"),
               c("ALGERIA", "DZA"),
               c("ANGOLA", "AGO"),
               c("EGYPTSR", "EGY"),
               c("MOROCCO", "MAR"),
               c("NIGERIA", "NGA"),
               c("CHILE", "CHL"),
               c("AZERBAI", "AZE"),
               c("KAZAK", "KAZ"),
               c("TURKMENI", "TKM"),
               c("UKRAINE", "UKR"),
               c("UZBEKIST", "UZB"),
               c("ALBANIA", "ALB"),
               c("BANGLAD", "BGD"),
               c("BRUNEI", "BRN"),
               c("CAMBODIA", "KHM"),
               c("MYANMAR", "MMR"),
               c("NZ_QTR", "NZL"),
               c("PAKISTAN", "PAK"),
               c("BOLIVIA", "BOL"),
               c("COLOMBIA", "COL"),
               c("ECUADOR", "ECU"),
               c("PERU", "PER"),
               c("TRIN_TOB", "TTO"),
               c("URUGUAY", "URY"),
               c("BAHRAIN", "BHR"),
               c("ISRAELSR", "ISR"),
               c("JORDANSR", "JOR"),
               c("OMAN", "OMN"),
               c("YEMEN", "YEM"),
               c("EQGUINEA", "GNQ"),
               c("LIBYA", "LBY"),
               c("TUNISIA", "TUN"),
               c("CAMEROON", "CMR"),
               c("CONGO", "COG"),
               c("COTE_DIV", "CIV"),
               c("ZAIRE", "COD"),
               c("ETHIOPIA", "ETH"),
               c("GABON", "GAB"),
               c("GHANA", "GHA"),
               c("KENYA", "KEN"),
               c("MOZAMBIQ", "MOZ"),
               c("NAMIBIA", "NAM"),
               c("SENEGAL", "SEN"),
               c("SUDAN", "SDN"),
               c("TANZANIA", "TZA"),
               c("AR_WE", "WE"),
               c("AR_CIS", "CIS"),
               c("AR_CEB", "CEB"),
               c("AR_ASIA", "ASIA"),
               c("AR_LATCA", "LATCA"),
               c("AR_ME", "ME"),
               c("AR_AFR", "RAF"))
  tab <- setNames(data.frame(tab, stringsAsFactors = FALSE),
                  nm = c("oesector", "iso3c"))
  return(tab)
}

#' Read Oxford Economics Global Economic Model database files
#'
#' Imports data from an Oxford Economics Global Economic Model database file
#' (.db) into R.
#'
#' @param db Character. Filename of or path to the database file.
#' @param mnemonic Character or character vector. Mnemonics (variable names) to
#' import.
#' @param sector Character or character vector. Sectors (country names) to
#' import.
#' @param mnemonic_sector Data frame with two columns, "Mnemonic" and "Sector".
#' These specify custom mnemonic-sector combinations.
#' @param exp_type Character or character vector. Type of values to be imported:
#' \code{"V"} for variable data (default), \code{"R"} for residual data, or
#' \code{c("V", "R")} for both.
#' @param model_dir Character. Path to model directory (default: C:/OEF).
#' @param start_year Numeric. The first year for which to import data (default:
#' 1980).
#' @param end_year Numeric. The last year for which to import data (default:
#' 2050).
#' @param as_xts Logical. If \code{TRUE}, data is returned in xts format.
#' @param verbose Logical. If \code{TRUE}, status messages are printed.
#' @return A list containing the data \code{$dat}, header metadata
#' \code{$dat_head}, fix metadata \code{$fix}, variable metadata \code{$var}
#' (including a logical value \code{$var$Is.percent} indicating if a variable is
#' a percentage value, and a character value \code{$var$Residual.Indicator}
#' listing the residual name corresponding to a specific variable), a character
#' vector \code{$type} storing the type of data that has been imported (variable
#' or residual data), and numeric values \code{$last_hist} and
#' \code{$first_fcst} specifying the index values of the last historical data
#' point and the first forecast data point respectively. Note that mnemonics are
#' converted to syntactically valid R names and that an "R_" prefix is added to
#' names of columns containing residual data.
#' @seealso \code{\link{oe_macromappings}}
#' @export
read_oedb <- function(db, mnemonic = NULL, sector = NULL,
                      mnemonic_sector = NULL, exp_type = "V",
                      model_dir = "C:/OEF", start_year = 1980, end_year = 2050,
                      as_xts = FALSE, verbose = FALSE) {
  # Check if Mdl tool is available
  if (Sys.which("mdl") == "") {
    stop("This function requires the Oxford Economics Mdl tool to run.")
  }

  if (system("mdl", ignore.stdout = TRUE, ignore.stderr = TRUE) != 1) {
    stop("This function requires the Oxford Economics Mdl tool to run.")
  }

  # Check if database file exists
  if (!file.exists(db)) {
    stop("Database file not found.")
  }

  # Make sure mnemonic_sector combinations are provided in correct format
  if (!is.null(mnemonic_sector)) {
    if (ncol(mnemonic_sector) == 2) {
      names(mnemonic_sector) <- c("Mnemonic", "Sector")
      mnemonic_sector$Mnemonic <- .clean_names(mnemonic_sector$Mnemonic)
    } else {
      warning("mnemonic_sector must be a data frame with two columns: ",
      "'Mnemonic', and 'Sector'.")
      mnemonic_sector <- NULL
    }
  }

  # Check if level/residual switch is being used correctly
  exp_type <- unique(exp_type)
  if (!all(exp_type %in% c("V", "R"))) {
    warning("Parameter exp_type must be 'V' for variable data, 'R' for ",
            "residual data, or both. Defaulting to 'V'.")
    exp_type <- "V"
  }

  # Print summary of function call
  if (verbose) {
    message("Call: db: ", db, "; mnemonic: ", mnemonic, "; sector: ", sector,
            "; mnemonic_sector: ",
            ifelse(!is.null(mnemonic_sector), "Provided", ""),
            "; exp_type: ", paste0(exp_type, collapse = ", "),
            "; model_dir: ", model_dir,
            "; start_year: ", start_year, "; end_year: ", end_year,
            "; verbose: ", verbose, "; as_xts: ", as_xts)
  }

  # Export fix metadata and read in fix metadata
  fix_dat <- NULL

  fix_file <- paste0(tempfile(tmpdir = tempdir()), "fix.csv")
  mdl_fix_call <- paste0("mdl export-entities -d ", db, " -m ", model_dir,
                         " -o ", fix_file, " -e fixes")

  if (verbose) {
    message(paste0("Running Mdl to export fix metadata: ", mdl_fix_call))
  }

  if (system(mdl_fix_call) != 0) {
    stop("Mdl tool returned an error.")
  }

  fix_dat             <- read.csv(fix_file, header = TRUE,
                                  stringsAsFactors = FALSE, na.strings = "")
  fix_dat             <- fix_dat[(fix_dat$Sector != "#ERROR"), ]
  fix_dat             <- fix_dat[(!is.na(fix_dat$Mnemonic)), ]
  fix_dat$Mnemonic    <- .clean_names(fix_dat$Mnemonic)
  fix_dat$Indicator   <- paste0(fix_dat$Mnemonic, "_", fix_dat$Sector)
  fix_dat$StartPeriod <- .oe_date(fix_dat$StartPeriod)
  fix_dat$EndPeriod   <- .oe_date(fix_dat$EndPeriod)

  file.remove(fix_file)

  # Export variable metadata and read in variable metadata
  var_dat <- NULL

  var_file <- paste0(tempfile(tmpdir = tempdir()), "var.csv")
  mdl_var_call <- paste0("mdl export-entities -d ", db, " -m ", model_dir,
                         " -o ", var_file, " -e variables")

  if (verbose) {
    message(paste0("Running Mdl to export variable metadata: ", mdl_var_call))
  }

  if (system(mdl_var_call) != 0) {
    stop("Mdl tool returned an error.")
  }

  var_dat                <- read.csv(var_file, header = TRUE,
                                     stringsAsFactors = FALSE, na.strings = "")
  var_dat$Mnemonic       <- .clean_names(var_dat$Mnemonic)
  var_dat$Indicator      <- paste0(var_dat$Mnemonic, "_", var_dat$Sector)
  var_dat$End.of.History <- .oe_date(var_dat$End.of.History)
  var_dat$Is.percent     <- grepl(".*\\[%.*", var_dat$Description)
  if ("R" %in% exp_type) {
    var_dat$Residual.Indicator <- paste0("R_", var_dat$Indicator)
  }

  file.remove(var_file)

  # Export actual data and read in data
  dat_file <- paste0(tempfile(tmpdir = tempdir()), "dat.csv")
  mdl_dat_call <- paste0("mdl export -d ", db, " -m ", model_dir, " -y ",
                         start_year, " -e ", end_year, " -o ", dat_file,
                         " -f Classic_v")

  # If mnemonics and/or sectors and/or mnemonics_sectors have been supplied, use
  # that information to build sel file
  sel_content <- NULL

  if (!is.null(mnemonic) || !is.null(sector) || !is.null(mnemonic_sector)) {
    # Create subsets
    var_dat_subset <- var_dat
    fix_dat_subset <- fix_dat

    # If mnemonics have been supplied, subset fix and variable metadata by these
    if (!is.null(mnemonic)) {
      mnemonic <- unique(mnemonic)
      var_dat_subset <- subset(var_dat_subset, Mnemonic %in% mnemonic)
      var_dat_subset <- unique(var_dat_subset)
      rownames(var_dat_subset) <- NULL

      fix_dat_subset <- subset(fix_dat_subset, Mnemonic %in% mnemonic)
      fix_dat_subset <- unique(fix_dat_subset)
      rownames(fix_dat_subset) <- NULL
    }

    # If sectors have been supplied, subset fix and variable metadata by these
    if (!is.null(sector)) {
      sector  <- unique(sector)
      var_dat_subset <- subset(var_dat_subset, Sector %in% sector)
      var_dat_subset <- unique(var_dat_subset)
      rownames(var_dat_subset) <- NULL

      fix_dat_subset <- subset(fix_dat_subset, Sector %in% sector)
      fix_dat_subset <- unique(fix_dat_subset)
      rownames(fix_dat_subset) <- NULL
    }

    # If custom mnemonic-sector combinations have been supplied, subset fix and
    # variable information by these
    if (!is.null(mnemonic_sector)) {
      mnemonic_sector <- unique(mnemonic_sector)
      mnemonic_sector$Indicator <- paste0(mnemonic_sector$Mnemonic, "_",
                                          mnemonic_sector$Sector)

      var_dat_sel <- var_dat
      var_dat_sel <- subset(var_dat_sel,
                            Indicator %in% mnemonic_sector$Indicator)

      fix_dat_sel <- fix_dat
      fix_dat_sel <- subset(fix_dat_sel,
                            Indicator %in% mnemonic_sector$Indicator)

      # If we already have an existing subset, add the new subset
      if (!is.null(mnemonic) || !is.null(sector)) {
        var_dat_subset <- rbind(var_dat_subset, var_dat_sel)
        fix_dat_subset <- rbind(fix_dat_subset, fix_dat_sel)
      } else {
        # Otherwise replace subset
        var_dat_subset <- var_dat_sel
        fix_dat_subset <- fix_dat_sel
      }

      var_dat_subset <- unique(var_dat_subset)
      rownames(var_dat_subset) <- NULL
      var_dat_subset <- unique(var_dat_subset)
      rownames(var_dat_subset) <- NULL
    }

    # Write content into sel file
    sel_content <- ""

    for (d in exp_type) {
      for (i in seq_len(nrow(var_dat_subset))) {
        sel_line <-
          paste0(unique(.revert_names(var_dat_subset$Mnemonic[i])), ",",
                 unique(var_dat_subset$Sector[i]), ",",
                 d, ",L,_\n")
        sel_content <- paste0(sel_content, sel_line)
      }
    }

    # Set name of temporary SEL file
    sel_file <- paste0(tempfile(tmpdir = tempdir()), "sel.sel")

    # Write content to SEL file
    write(sel_content, file = sel_file)

    # Append command to use SEL file to system call
    mdl_dat_call <- paste0(mdl_dat_call, " -s ", sel_file)

    # Overwrite main variable and fix metadata
    var_dat <- var_dat_subset
    fix_dat <- fix_dat_subset
  }

  # Execute system call to Mdl
  if (verbose) {
    message(paste0("Running Mdl to export data: ", mdl_dat_call))
  }

  if (system(mdl_dat_call) != 0) {
    stop("Mdl tool returned an error.")
  }

  # Load data into R
  dat <- read.csv(dat_file, header = FALSE, stringsAsFactors = FALSE,
                  na.strings = "")

  # Create column names (format: Indicator_Location)
  type_row     <- which(trimws(dat[, 1]) == "L") - 1
  mnemonic_row <- which(trimws(dat[, 1]) == "L") - 2
  sector_row   <- which(trimws(dat[, 1]) == "L") - 3
  date_row     <- which(trimws(dat[, 1]) == "L") + 2
  meta_end     <- which(trimws(dat[, 1]) == "L") + 4

  dat_col <- paste0((unname(trimws(dat[mnemonic_row, ]))), "_",
                    (unname(trimws(dat[sector_row, ]))))
  dat_col <- .clean_names(dat_col)
  dat_col[(trimws(dat[type_row, ]) == "R")] <-
    paste0("R_", dat_col[(trimws(dat[type_row, ]) == "R")])
  colnames(dat) <- make.names(dat_col, unique = TRUE)
  colnames(dat)[ncol(dat)] <- "date"

  # Save metadata separately
  dat_head      <- dat[1:meta_end, -ncol(dat)]
  dat           <- dat[-(1:meta_end), ]
  rownames(dat) <- NULL

  # Convert remaining data to numeric format while preserving data frame
  dat[, seq_len(ncol(dat))] <- sapply(dat[, seq_len(ncol(dat))],
                                      function(x) as.numeric(as.character(x)))

  # Carry forward last date value and append quarter
  year_ticks       <- !is.na(dat[, ncol(dat)])
  dat[, ncol(dat)] <- c(NA, dat[year_ticks, ncol(dat)])[cumsum(year_ticks) + 1]
  dat[, ncol(dat)] <- paste0(dat[, "date"], "-",
                             rep(c(1, 4, 7, 10), (nrow(dat) / 4)), "-", 1)
  dat[, ncol(dat)] <- as.Date(dat[, ncol(dat)])

  # Put date column first
  dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]

  # Save type of variable
  dat_type <- trimws(dat_head[type_row, ])
  dat_type <- setNames(dat_type, nm = colnames(dat[, -1]))

  # Save index of last historical data point
  last_hist <- trimws(dat_head[date_row, ])
  last_hist <- replace(last_hist, last_hist == "0", NA)
  last_hist <- .oe_date(last_hist)
  last_hist <- match(last_hist, dat$date, nomatch = NA)
  last_hist <- setNames(last_hist, nm = colnames(dat[, -1]))

  # Save index of first forecast data point
  first_fcst <- last_hist + 1
  first_fcst <- replace(first_fcst, first_fcst > nrow(dat), nrow(dat))

  # Return xts object
  if (as_xts) {
    dat <- xts::xts(x = dat[, -1], order.by = dat[, 1])
  }

  file.remove(dat_file)

  if (!is.null(sel_content)) {
    file.remove(sel_file)
  }

  # Return data and metadata
  return(list(dat = dat, dat_head = dat_head, fix = fix_dat, var = var_dat,
              type = dat_type, last_hist = last_hist, first_fcst = first_fcst))
}
