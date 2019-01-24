#' Check for availability of Mdl tool on load
.onLoad <- function(libname, pkgname) {
  # Check if Mdl tools is available
  if (Sys.which("mdl") == "") {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }
  
  if (system("mdl", ignore.stdout = TRUE, ignore.stderr = TRUE) != 1) {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }
}

#' Function to return Oxford Economics colour palette (discrete)
#'
#' Returns Oxford Economics colour palette. This palette includes 15 unique
#' colours.
#' @return Returns Oxford Economics colour palette.
#' @export
#' @examples
#' oe_pal()
oe_pal <- function(){
  oecols <- c(paste0("#", paste(as.hexmode(c( 0, 52, 105)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 0, 173, 220)), collapse = "")),
              paste0("#", paste(as.hexmode(c(123, 124, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(189, 27, 33)), collapse = "")),
              paste0("#", paste(as.hexmode(c(209, 162, 30)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 0, 121, 63)), collapse = "")),
              paste0("#", paste(as.hexmode(c(222, 99, 40)), collapse = "")),
              paste0("#", paste(as.hexmode(c(150, 87, 147)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 75, 199, 231)), collapse = "")),
              paste0("#", paste(as.hexmode(c(173, 224, 242)), collapse = "")),
              paste0("#", paste(as.hexmode(c(169, 166, 162)), collapse = "")),
              paste0("#", paste(as.hexmode(c(208, 205, 201)), collapse = "")),
              paste0("#", paste(as.hexmode(c(197, 137, 48)), collapse = "")),
              paste0("#", paste(as.hexmode(c(218, 177, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(233, 212, 180)), collapse = "")))
  return(oecols)
}

#' Return Oxford Economics location identifiers and corresponding ISO codes
#'
#' Returns a data frame containing Oxford Economics location identifiers and
#' corresponding ISO 3-character code.
#' @return Returns a data frame containing Oxford Economics location identifiers
#'         and corresponding ISO 3-character code.
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

#' Period-on-period differences
#'
#' A convenience function to calculate period-on-period differences for a
#' given data series. If the input vector is of annual frequency, the function
#' will return year-on-year differences. If the input vector is of quarterly
#' frequency, the function will return quarter-on-quarter differences. Note
#' that the frequency of the input vector is not checked for explicitly.
#' @param x A numeric vector containg the original values.
#' @return A numeric vector containing the transformed values.
#' @export
#' @examples
#' diffp(1:10)
diffp <- function(x) {
  w <- c(NA, x[1:(length(x) - 1)])
  x <- x - w
  return(x)
}

#' Period-on-period growth rates
#'
#' A convenience function to calculate period-on-period growth rates for a
#' given data series. If the input vector is of annual frequency, the function
#' will return year-on-year growth rates. If the input vector is of quarterly
#' frequency, the function will return quarter-on-quarter growth rates. Note
#' that the frequency of the input vector is not checked for explicitly.
#' @param x A numeric vector containg the original values.
#' @return A numeric vector containing the transformed values.
#' @export
#' @examples
#' pch(1:10)
pch <- function(x) {
  w <- c(NA, x[1:(length(x) - 1)])
  x <- (x - w) * 100 / w
  return(x)
}

#' Annualised quarterly growth rates
#'
#' A convenience function to calculate quarter-on-quarter annualised growth
#' rates for a given data series. Note that frequency of the input vector is not
#' checked for explicitly.
#' @param x A numeric vector containg the original values.
#' @return A numeric vector containing the transformed values.
#' rates.
#' @export
#' @examples
#' pach(1:10)
pach <- function(x) {
  w <- c(NA, x[1:(length(x) - 1)])
  x <- ((x / w)^4 - 1) * 100
  return(x)
}

#' Year-on-year differences
#'
#' A convenience function to calculate year-on-year differences for a given data
#' series. Note that frequency of the input vector is not checked for explicitly.
#' @param x A numeric vector containg the original values.
#' @return A numeric vector containing the transformed values.
#' @export
#' @examples
#' diffy(1:20)
diffy <- function(x) {
  w <- c(NA, NA, NA, NA, x[1:(length(x) - 4)])
  x <- x - w
  return(x)
}

#' Year-on-year growth rates
#'
#' A convenience function to calculate year-on-year growth rates for a given
#' data series. Note that frequency of the input vector is not checked for
#' explicitly.
#' @param x A numeric vector containg the original values.
#' @return A numeric vector containing the transformed values.
#' @export
#' @examples
#' pchy(1:20)
pchy <- function(x) {
  w <- c(NA, NA, NA, NA, x[1:(length(x) - 4)])
  x <- (x - w) * 100 / w
  return(x)
}

#' Read Oxford Economics Global Economic Model database files
#'
#' Reads an Oxford Economics Global Economic Model database file (.db) into a
#' data frame.
#'
#' @param db A filename.
#' @param md Model directory (default: C:/OEF).
#' @param sy The first year for which data should be exported (default: 1980).
#' @param ey The last year for which data should be exported (default: 2050).
#' @param id A vector of variable identifiers to be exported (optional).
#' Variable identifiers are made up of variable names and location names,
#' separated by a comma. If omitted, all variables will be imported.
#' @param tp Type of values to be exported: (\code{V}) for variable data
#' (default), \code{R} for residual data.
#' @return A list containing the data frame (\code{$df}), basic meta data
#' (\code{$meta}), and a vector of dates giving the last data point with
#' historical data (\code{$lasthistory})
#' @seealso \code{\link{oe_macromappings}}
#' @export
#' @examples
#' setwd("C:/OEF/")
#' ids <- c("GDPHEADPPP!,US", "GDPHEADPPP!,GERMANY", "GDPHEADPPP!,UK",
#'           "GDPHEADPPP!,JAPAN", "GDPHEADPPP!,KOREA")
#' dat <- read_oedb("Jan19.db", "C:/OEF", 1990, 2020, ids, "V")$df
#' head(dat)

read_oedb <- function(db, md = "C:/OEF", sy = 1980, ey = 2050, id = NULL,
                      tp = "V") {
  # Check if Mdl tools is available
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
  
  # Set name of temporary CSV file
  cf <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), ".csv")
  
  # Assemble string for system call to Mdl
  st <- paste0("mdl export -d ", db, " -m ", md, " -y ", sy, " -e ", ey, " -o ",
               cf, " -f Classic_v")
  
  # If variable IDs are supplied, write them to temporary SEL file
  if (!is.null(id)) {
    
    # Check if level/residual switch is being used correctly
    if (!is.element(tp, c("V", "R"))) {
      stop("Parameter tp must be 'V' for variable data or 'R' for residual
           data.")
    }
    
    # Check if variable IDs are being supplied correctly
    if (any(!grepl(",", id))) {
      stop("Parameter id must be a vector containing variable and country
           names.")
    }
    
    # Check if any IDs are duplicated
    if (any(duplicated(id))) {
      stop("Parameter id must contain unique values. Dropping duplicates.")
      id <- unique(id)
    }
    
    # Set name of temporary SEL file
    sf <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), ".sel")
    
    # Prepare content for temporary SEL file
    sc <- ""
    
    for (i in 1:length(id)) {
      sc <- paste0(sc, id[i], ",", tp, ",L,_\n")
    }
    
    # Write content to SEL file
    write(sc, file = sf)
    
    # Append command to use SEL file to system call
    st <- paste0(st, " -s ", sf)
  }
  
  # Execute system call to Mdl
  if (system(st) != 0) {
    stop("Mdl tool returned an error.")
  }
  
  # Load data into R
  df <- read.csv(cf, header = FALSE, stringsAsFactors = FALSE, na.strings = "")
  
  # Create column names (format: Indicator_Location)
  cn <- paste0((unname(trimws(df[3, ]))), "_", (unname(trimws(df[2, ]))))
  colnames(df) <- cn
  colnames(df)[ncol(df)] <- "date"
  
  # Save metadata separately
  mt     <- df[1:9, -ncol(df)]
  df     <- df[-(1:9), ]
  rownames(df) <- NULL
  
  # Last data point with historical data
  lhist <- mt[7, ]
  lhist[which(lhist == "0")] <- NA
  if (any(!is.na(lhist))) {
    lhist[, !is.na(lhist)] <-
      paste0(substr(lhist[1, !is.na(lhist)], 1, 4), "-",
             (as.numeric(substr(lhist[1, !is.na(lhist)], 5, 6)) *
                3 - 2), "-01")
  }
  lhist <- as.Date(t(lhist))
  names(lhist) <- colnames(mt)
  
  # Convert remaining data to numeric format while preserving data frame
  df[, 1:ncol(df)] <- sapply(df[, 1:ncol(df)],
                             function(x) as.numeric(as.character(x)))
  
  # Create date column
  na.locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v) + 1]
  }
  
  # Carry forward last date value
  md <- !is.na(df[, ncol(df)])
  df[, ncol(df)] <- c(NA, df[md, ncol(df)])[cumsum(md) + 1]
  df[, ncol(df)] <- as.Date(paste0(df[, "date"], "-",
                                   rep(c(1, 4, 7, 10), (nrow(df) / 4)), "-", 1))
  
  # Put date column first
  df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
  
  # Remove temporary CSV file
  file.remove(cf)
  
  # Remove temporary SEL file
  if (!is.null(id)) {
    file.remove(sf)
  }
  
  # Return data frame and meta data
  return(list(df = df, meta = mt, lasthistory = lhist))
}
