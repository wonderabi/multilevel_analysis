library(httr)
library(jsonlite)
library(dplyr)

# Create Function ----
#' For this function, we are linking the students' home addresses to the Census
#' geocoder website.
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


# Test Set to Ensure Function is working ----
subset_enrollment <- subset_enrollment[1:5, ]
subset_enrollment$CDP <- vapply(
  subset_enrollment$full_address,
  geocode_cdp,
  character(1)
)


# Apply function to enrollment dataset ----
enrollment$CDP_Geo <- vapply(
  enrollment$full_address,
  geocode_cdp,
  character(1)
)


# Force CDPs if there's only one CDP per village
enrollment <- enrollment %>%
  mutate(CDP = ifelse(is.na(CDP_Geo) & Village %in% c("HAGATNA", "CHALAN PAGO", 
                                                  "ORDOT", "MONGMONG", "TOTO",
                                                  "MAITE", "AGAT", "MERIZO",
                                                  "UMATAC"), 
                      paste0(str_to_title(Village), " CDP"), CDP_Geo))


