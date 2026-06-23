# ==============================================================================
# Addresses to CDP ----
# ==============================================================================


# ==============================================================================
# Load Packages ----
# ==============================================================================
library(httr)
library(jsonlite)
library(dplyr)


# ==============================================================================
# Function ----
# ==============================================================================
# Function: Addresses to CDP ----
#' You can use this code to autonomously link addresses to its CDP using the 
#' Census geocoder
#' 
#' @param address The address you wish to link
#' 
#' @returns The corresponding CDP
#' @export
#' 
#' @example 
#' geocode_cdp("303 University Drive, UOG Station, Mangilao, Guam 96923")
#' 
geocode_cdp <- function(address) {
  
  # ALWAYS return character
  if (is.na(address) || !nzchar(address)) {
    return(NA_character_)
  }
  
  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
  
  address <- gsub("\\s+", " ", address)
  address <- trimws(address)
  
  res <- tryCatch(
    httr::GET(
      url = base_url,
      query = list(
        address = address,
        benchmark = "Public_AR_Current",
        vintage = "Current_Current",
        format = "json"
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    return(NA_character_)
  }
  
  df <- tryCatch(
    httr::content(res, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  
  if (is.null(df) || is.null(df$result$addressMatches) ||
      length(df$result$addressMatches) == 0) {
    return(NA_character_)
  }
  
  geogs <- df$result$addressMatches[[1]]$geographies
  
  if (!is.null(geogs$`Census Designated Places`) &&
      length(geogs$`Census Designated Places`) > 0) {
    return(as.character(geogs$`Census Designated Places`[[1]]$NAME))
  }
  
  return(NA_character_)
}


# ==============================================================================
# Set Directory ----
# ==============================================================================
setwd("yourDir")
dir <- "yourDir"


# ==============================================================================
# Read Files ----
# ==============================================================================
enrollment_file <- read.csv(paste0(dir, "yourEnrollmentFile.csv"))


# ==============================================================================
# Apply Function to CDP ----
# ==============================================================================
enrollment_file$CDP <- vapply(
  enrollment_file$address,
  geocode_cdp,
  character(1)
)


# ==============================================================================
# Force CDPs if Only One Exists in the Area ----
# ==============================================================================
enrollment_file <- enrollment_file %>%
  mutate(CDP = ifelse(is.na(CDP) & yourCityVariable %in% 
                        c("CDP1","CDP2"), 
                      paste0(str_to_title(yourCityVariable), " CDP"), CDP))


