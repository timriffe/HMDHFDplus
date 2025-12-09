
# FILE CONTENTS:

# 1) readHMD()
# 2) readHMDweb()
# 3) readJMDweb()

############################################################################
# readHMD()
############################################################################

#'
#' @title \code{readHMD()} reads a standard HMD .txt table as a \code{data.frame}
#' 
#' @description This calls \code{read.table()} with all the necessary defaults to avoid annoying surprises. The Age column is also stripped of \code{"+"} and converted to integer, and a logical indicator column called \code{OpenInterval} is added to show where these were located. If the file contains population counts, values are split into two columns for Jan 1 and Dec 31 of the year. Output is invisibly returned, so you must assign it to take a look. This is to avoid lengthy console printouts. 
#' 
#' @param filepath path or connection to the HMD text file, including .txt suffix.
#' @param ... other arguments passed to \code{read.table}, not likely needed.
#' @param fixup logical. Should columns be made more user-friendly, e.g., forcing Age to be integer?
#' 
#' @return data.frame of standard HMD output, except the Age column has been cleaned, and a new open age indicator column has been added. If the file is Population.txt or Population5.txt, there will be two columns each for males and females.
#' 
#' @details Population counts in the HMD typically refer to Jan 1st. One exception are years in which a territorial adjustment has been accounted for in estimates. For such years, `YYYY-` refers to Dec 31 of the year before the adjustment, and `YYYY+` refers to Jan 1 directly after the adjustment (adjustments are always made Jan 1st). In the data, it will just look like two different estimates for the same year, but in fact it is a definition change or similar. In order to remove headaches from potential territorial adjustments in the data, we simply create two columns, one for January 1st (e.g.,\code{"Female1"}) and another for Dec 31st (e.g.,\code{"Female2"}) . One can recover the adjustment coefficient for each year by taking the ratio $$Vx = P1(t+1) / P2(t)$$. In most years this will be 1, but in adjustment years there is a difference. This must always be accounted for when calculating rates and exposures. Argument \code{fixup} is outsourced to \code{HMDparse()}.
#' 
#' @importFrom utils read.table
#' 
#' @export
#' 
#' @note function written by Tim Riffe.
#' 
readHMD <- function(filepath, fixup = TRUE, ...){
  DF              <- read.table(file = filepath, header = TRUE, skip = 2, na.strings = ".", as.is = TRUE, ...)
  if (fixup){
    DF        <- HMDparse(DF, filepath)
  }
  invisible(DF)
}

############################################################################
# readHMDweb()
############################################################################

#' @title readHMDweb a basic HMD data grabber.
#'
#' @description
#' Basic HMD data grabber, based on Carl Boe's original \code{HMD2R()}.
#' It can grab one or more HMD statistical products, possibly for multiple
#' countries, using a single login session.
#'
#' Typical pitfalls are removed: the Age column is coerced to integer, while an
#' AgeInterval column is created. Population counts are placed into two columns
#' for Jan. 1 and Dec. 31 of the same year, so as to remove headaches from
#' population universe adjustments. Argument \code{fixup} is outsourced to
#' \code{HMDparse()}.
#'
#' @param CNTRY character vector. HMD country letter codes. If not spelled
#'   right, or not specified, an interactive menu is offered (if possible).
#' @param item character vector. HMD item code(s), e.g. \code{"fltper_1x1"}.
#'   If missing and interactive, you can select from items available in at
#'   least one selected country.
#' @param username character. HMD username (usually your email).
#' @param password character. HMD password.
#' @param fixup logical. If TRUE, pass the downloaded table through
#'   \code{HMDparse()} for convenience.
#' @param validate_items logical. If TRUE (default), item codes are validated
#'   against the union of items available across the selected countries.
#'
#' @return
#' If a single item ends up with available data: a data.frame (possibly stacked
#' over multiple countries, with \code{PopName} as the first column when more
#' than one country is requested).
#'
#' If multiple items end up with available data: a named list of such
#' data.frames, one per item.
#'
#' Output is returned invisibly.
#'
#' @details
#' This function points to the new HMD website (from June 2022). If login
#' fails, you may need to re-register at:
#' \url{https://www.mortality.org/Account/UserAgreement}.
#'
#' @importFrom rvest html_form_set session html_form session_submit session_jump_to
#' @importFrom httr content status_code
#' @importFrom dplyr pull
#' @export
readHMDweb <- function(CNTRY, item, username, password,
                       fixup = TRUE, validate_items = TRUE){
  
  ## -------- 1) LOGIN -------- ##
  if (missing(username)) {
    if (interactive()) {
      cat("\ntype in HMD username (usually your email, quotes not necessary):\n")
      username <- userInput(FALSE)
    } else {
      stop("username/password must be given in non-interactive mode.")
    }
  }
  if (missing(password)) {
    if (interactive()) {
      cat("\ntype in HMD password:\n")
      password <- userInput(FALSE)
    } else {
      stop("username/password must be given in non-interactive mode.")
    }
  }
  
  loginURL <- "https://www.mortality.org/Account/Login"
  html     <- rvest::session(loginURL)
  
  pgform        <- rvest::html_form(html)[[1]]
  pgform$action <- loginURL
  pgform$url    <- loginURL
  the_token     <- pgform$fields["__RequestVerificationToken"]
  
  filled_form <- suppressWarnings(
    rvest::html_form_set(
      pgform,
      Email = username,
      Password = password,
      `__RequestVerificationToken` =
        unlist(the_token)["__RequestVerificationToken.value"]
    )
  )
  
  html2    <- rvest::session_submit(html, filled_form)
  Continue <- httr::status_code(html2) == 200L
  if (!Continue) {
    stop(
      "Login failed.\n",
      "Check your username/password, or re-register here:\n",
      "  https://www.mortality.org/Account/UserAgreement\n"
    )
  }
  
  ## -------- 2) COUNTRY VALIDATION -------- ##
  ctrylist   <- getHMDcountries()
  ctrylookup <- ctrylist |>
    dplyr::select(-"link")
  valid_codes <- ctrylookup$CNTRY
  
  if (missing(CNTRY)) {
    cat("\nCNTRY missing\n")
    if (interactive()) {
      CNTRY <- select.list(
        choices  = valid_codes,
        multiple = TRUE,
        title    = "Select one or more Country Codes"
      )
      if (!length(CNTRY)) stop("No country selected.")
    } else {
      stop("CNTRY should be one of:\n", paste(valid_codes, collapse = ", "))
    }
  }
  
  CNTRY <- unique(CNTRY)
  bad   <- setdiff(CNTRY, valid_codes)
  
  if (length(bad)) {
    if (interactive()) {
      message("Invalid CNTRY code(s): ", paste(bad, collapse = ", "))
      CNTRY <- select.list(
        choices  = valid_codes,
        multiple = TRUE,
        title    = "Select one or more valid Country Codes"
      )
      if (!length(CNTRY)) stop("No valid country selected.")
    } else {
      stop(
        "Invalid CNTRY code(s): ", paste(bad, collapse = ", "),
        "\nValid codes are:\n", paste(valid_codes, collapse = ", ")
      )
    }
  }
  
  CNTRY_vec <- CNTRY
  
  ## -------- 3) ITEM HANDLING / VALIDATION -------- ##
  
  # lazy cache for per-country item tables
  items_by_country <- list()
  
  get_items_for <- function(cntry) {
    if (!is.null(items_by_country[[cntry]])) {
      return(items_by_country[[cntry]])
    }
    tab <- getHMDitemavail(cntry)
    items_by_country[[cntry]] <<- tab
    tab
  }
  
  union_items <- NULL
  
  if (missing(item) || validate_items) {
    # Only pay the cost of scanning all countries when needed
    if (interactive() && length(CNTRY_vec) >= 3L) {
      message("Validating items across all selected countries...")
    }
    
    # build items_by_country cache for all countries
    for (cn in CNTRY_vec) {
      if (is.null(items_by_country[[cn]])) {
        items_by_country[[cn]] <- getHMDitemavail(cn)
      }
    }
    
    items_allowed_by_country <- lapply(CNTRY_vec, function(cn) {
      clean_HMD_items(items_by_country[[cn]])
    })
    names(items_allowed_by_country) <- CNTRY_vec
    union_items <- sort(unique(unlist(items_allowed_by_country)))
  }
  
  if (missing(item)) {
    cat("\nitem missing\n")
    if (interactive()) {
      item <- select.list(
        choices  = union_items,
        multiple = TRUE,
        title    = "Select one or more item Codes"
      )
      if (!length(item)) stop("No item selected.")
    } else {
      stop(
        "item must be given in non-interactive mode.\n",
        "Valid items (available in at least one selected country) include:\n",
        paste(union_items, collapse = ", ")
      )
    }
  }
  
  item_vec <- unique(item)
  
  if (validate_items) {
    invalid_items <- setdiff(item_vec, union_items)
    if (length(invalid_items)) {
      if (interactive()) {
        message("Invalid item code(s): ", paste(invalid_items, collapse = ", "))
        item_vec <- select.list(
          choices  = union_items,
          multiple = TRUE,
          title    = "Select valid item Codes"
        )
        if (!length(item_vec)) stop("No valid item selected.")
        item_vec <- unique(item_vec)
      } else {
        stop(
          "Invalid item code(s): ", paste(invalid_items, collapse = ", "),
          "\nValid items for these countries include:\n",
          paste(union_items, collapse = ", ")
        )
      }
    }
  }
  
  ## -------- 4) DOWNLOAD HELPER -------- ##
  unavailable <- character(0)
  
  grab_one <- function(cntry, itm) {
    # lazy fetch + cache
    item_table <- get_items_for(cntry)
    item_clean <- clean_HMD_items(item_table)
    
    if (!itm %in% item_clean) {
      # valid globally (if validate_items) but not for this country
      unavailable <<- c(unavailable, paste(cntry, itm, sep = ":"))
      return(NULL)
    }
    
    stub_url <- item_table$link[item_table$item == itm][1L]
    grab_url <- paste0("https://www.mortality.org", stub_url)
    
    data_grab <- rvest::session_jump_to(html2, url = grab_url)
    the_table <- httr::content(
      data_grab$response,
      as       = "text",
      encoding = "UTF-8"
    )
    
    # HTML guard (failed login/expired session/etc)
    if (grepl("<!DOCTYPE html", the_table, ignore.case = TRUE) ||
        grepl("<html",         the_table, ignore.case = TRUE)  ||
        grepl("Account/Login", the_table, fixed       = TRUE)) {
      stop(
        "HMD returned HTML instead of data for (", cntry, ", ", itm, "). ",
        "Likely a login/session issue."
      )
    }
    
    con <- textConnection(the_table)
    on.exit(close(con), add = TRUE)
    
    DF <- read.table(
      con,
      header     = TRUE,
      skip       = 2,
      na.strings = ".",
      as.is      = TRUE
    )
    
    if (fixup) {
      DF <- HMDparse(DF, filepath = grab_url)
    }
    
    DF
  }
  
  ## -------- 5) LOOP OVER (COUNTRY, ITEM) -------- ##
  res_by_item <- lapply(item_vec, function(itm) {
    dfs <- lapply(CNTRY_vec, function(cntry) {
      DF <- grab_one(cntry, itm)
      if (is.null(DF)) return(NULL)
      if (length(CNTRY_vec) > 1L) {
        DF$PopName <- cntry
      }
      DF
    })
    
    dfs <- Filter(Negate(is.null), dfs)
    
    if (!length(dfs)) {
      return(NULL)
    }
    
    if (length(dfs) == 1L) {
      dfs[[1L]]
    } else {
      do.call(rbind, dfs)
    }
  })
  names(res_by_item) <- item_vec
  
  # Drop items that had no data in any country
  empty_items <- vapply(res_by_item, is.null, logical(1))
  if (all(empty_items)) {
    stop("No requested item available in any selected country.")
  }
  if (any(empty_items)) {
    unavailable <- c(
      unavailable,
      paste("ALL_COUNTRIES", names(res_by_item)[empty_items], sep = ":")
    )
    res_by_item <- res_by_item[!empty_items]
  }
  
  ## -------- 6) SHAPE RETURN VALUE -------- ##
  out <- if (length(res_by_item) == 1L) res_by_item[[1L]] else res_by_item
  
  # Move PopName to first column (low-memory column reordering)
  if (is.data.frame(out)) {
    if ("PopName" %in% names(out)) {
      out <- out[c("PopName", setdiff(names(out), "PopName"))]
    }
  } else if (is.list(out)) {
    out <- lapply(out, function(x) {
      if (is.data.frame(x) && "PopName" %in% names(x)) {
        x[c("PopName", setdiff(names(x), "PopName"))]
      } else {
        x
      }
    })
  }
  
  ## -------- 7) WARN ABOUT SKIPPED COMBOS -------- ##
  if (length(unavailable)) {
    warning(
      "Some requested country/item combinations were unavailable and skipped:\n",
      paste("  ", unavailable, collapse = "\n"),
      call. = FALSE
    )
  }
  
  invisible(out)
}


# end readHMDweb()
############################################################################
# readJMDweb()
############################################################################

#'
#' @title read data from the Japan Mortality Database into R
#' 
#' @description JMD data are formatted exactly as HMD data. This function simply parses the necessary url together given a prefecture code and data item (same nomenclature as HMD). Data is parsed using \code{HMDparse()}, which converts columns into useful and intuitive classes, for ready-use. See \code{?HMDparse} for more information on type conversions. No authentication is required for this database. Only a single item/prefecture is downloaded. Loop for more complex calls (See examples). The prefID is not appended as a column, so be mindful of this if appending several items together into a single \code{data.frame}. Note that at the time of this writing, the finest Lexis resolution for prefectural lifetables is 5x5 (5-year, 5-year age groups). Raw data are, however, provided in 1x1 format, and deaths are also available in triangles.
#' 
#' @param prefID a single prefID 2-digit character string, ranging from \code{"00"} to \code{"47"}.
#' @param item the statistical product you want, e.g., \code{"fltper_5x5"}. Only 1.
#' @param fixup logical. Should columns be made more user-friendly, e.g., forcing Age to be integer?
#' @param ... extra arguments ultimately passed to \code{read.table()}. Not likely needed.
#' 
#' @return \code{data.frame} of the data item is invisibly returned
#' 
#' @details No details of note. This database in independently maintained, so file types/locations are subject to change. If this happens, please notify the package maintainer.
#' 
#' @importFrom httr HEAD
#' 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' library(HMDHFDplus)
#' # grab prefecture codes (including All Japan)
#' prefectures <- getJMDprefectures()
#' # grab all mltper_5x5
#' # and stick into long data.frame: 
#' mltper <- do.call(rbind, lapply(prefectures, function(prefID){
#'                    Dat        <- readJMDweb(prefID = prefID, item = "mltper_5x5", fixup = TRUE)
#'                    Dat$PrefID <- prefID
#'                    Dat
#' }))
#' }
#' 
readJMDweb <- function(prefID = "01", item = "Deaths_5x5", fixup = TRUE, ...){
	JMDurl      <- paste("https://www.ipss.go.jp/p-toukei/JMD",
			         prefID, "STATS", paste0(item, ".txt"), sep = "/")

	if (httr::HEAD(JMDurl)$all_headers[[1]]$status == 200){
		con         <- url(JMDurl)
		Dat         <- readHMD(con, fixup = fixup, ...)
		# close(con)
		return(invisible(Dat))
	} else {
		cat("Either the prefecture code or data item are not available\nCheck names.\nNULL returned\n")
		NULL
	}
}
# item <- "mltper_5x5";Dat <- readHMD(con, fixup = TRUE)
############################################################################
# readCHMDweb()
############################################################################

#'
#' @title read data from the Canadian Human Mortality Database into R
#' 
#' @description CHMD data are formatted exactly as HMD data. This function simply parses the necessary url together given a province code and data item (same nomenclature as HMD). Data is parsed using \code{HMDparse()}, which converts columns into useful and intuitive classes, for ready-use. See \code{?HMDparse} for more information on type conversions. No authentication is required for this database. Only a single item/prefecture is downloaded. Loop for more complex calls (See examples). The provID is not appended as a column, so be mindful of this if appending several items together into a single \code{data.frame}. Note that at the time of this writing, the finest Lexis resolution for prefectural lifetables is 5x5 (5-year, 5-year age groups). Raw data are, however, provided in 1x1 format, and deaths are also available in triangles. Note that cohort data are not produced for Canada at this time (but you could produce such data by starting with the \code{Deaths\_Lexis} file...).
#' 
#' @param provID a single provID 3 character string, as returned by \code{getCHMDprovinces()}.
#' @param item the statistical product you want, e.g., \code{"fltper_5x5"}. Only 1.
#' @param fixup logical. Should columns be made more user-friendly, e.g., forcing Age to be integer?
#' @param ... extra arguments ultimately passed to \code{read.table()}. Not likely needed.
#' 
#' @return \code{data.frame} of the data item is invisibly returned
#' 
#' @details This database is curated independently from the HMD/HFD family, and so file types and locations may be subject to change. If this happens, please notify the package maintainer.
#' 
#' @export 
#' 
#' @importFrom httr HEAD
#' 
#' @examples 
#' \dontrun{
#' library(HMDHFDplus)
#' # grab province codes (including All Canada)
#' provs <- getCHMDprovinces()
#' # grab all mltper_5x5  
#' # and stick into long data.frame: 
#' mltper <- do.call(rbind, lapply(provs, function(provID){
#'                    Dat        <- readCHMDweb(provID = provID, item = "mltper_5x5", fixup = TRUE)
#'                    Dat$provID <- provID
#'                    Dat
#' }))
#' }
#' 

readCHMDweb <- function(provID = "can", item = "Deaths_1x1", fixup = TRUE, ...){
	CHMDurl         <- paste("https://www.prdh.umontreal.ca/BDLC/data/",
			             provID, paste0(item, ".txt"), sep = "/")

	if (httr::HEAD(CHMDurl)$all_headers[[1]]$status == 200){
		con         <- url(CHMDurl)
		Dat         <- readHMD(con, fixup = fixup, ...)
		# close(con)
		return(invisible(Dat))
	} else {
		cat("Either the prefecture code or data item are not available\nCheck names.\nNULL returned\n")
		NULL
	}
	
	
	invisible(Dat)
}

