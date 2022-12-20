library(data.table)
library(sf)
library(stringr)
library(tidycensus)

# The tmp variable is used to store temporary data and unused code
tmp <- list()
tmp$unused_code <- function() {
    tmp$pums_variables <- as.data.table(pums_variables)[year == 2020]
    tmp$v <- tmp$pums_variables[, .N, keyby = .(var_code, var_label)]
    View(tmp$v)
    tmp$v[var_code == "MTFC"]
    tmp$v[var_code %in% c("SERIALNO", "SPORDER", "WGTP", "PWGTP", "AGEP", "ST"), .(var_code, var_label)]
    tmp$st_layers <- st_layers(tmp$dsn)
}

# Containers for app variables
facd <- list()
facd$cat <- function(...) cat(..., file=stderr(), sep = "")
facd$stopifna <- function(...) {
    stopifnot(!is.null(...))
    stopifnot(!anyNA(..., recursive = TRUE))
}
tmp <- list()
# 2022-11-13 The sf library requires gdal, geos, proj, et al in the server.
# PUMAs are not available for year %in% 2012:2015 due to inconsistent PUMA boundary definitions
facd$min_year <- as.integer(2016)
facd$max_year <- as.integer(2020)
facd$years <- (facd$min_year):(facd$max_year)
facd$n_years <- length(facd$years)
tmp$shape_files <- "tl_2021_39_puma10"
tmp$sf <- st_read(tmp$shape_files)
facd$sf <- st_transform(tmp$sf, st_crs("WGS84"))
# puma_helper is very useful for handling PUMAs that have 0 survey respondents or NA values.
# Area might be needed as a layerId. Do not convert to integer.
# See https://github.com/rstudio/leaflet/issues/496#issuecomment-1208869160
facd$puma_helper <- data.table(Area = facd$sf$PUMACE10, Name = facd$sf$NAMELSAD10)
stopifnot(nchar(facd$puma_helper$Area) == 5)
tmp$puma_test <- facd$puma_helper[, .N, keyby = Area]
stopifnot(tmp$puma_test$N == 1)
facd$puma_helper[, AreaId := .I]
setkey(facd$puma_helper, AreaId)
facd$stopifna(facd$puma_helper)
facd$year_puma_helper <- CJ(Year = facd$years, AreaId = facd$puma_helper$AreaId)
facd$year_puma_helper[, Area := facd$puma_helper$Area, by = Year]

# It looks like tryCatch() does not work well within lapply.
# If there is a 404 error for year 2021, then all datasets need to be downloaded again.
# Let's avoid using tryCatch here altogether.
tmp$dataset_list <- lapply(facd$years, function(yr) {
    facd$cat("\n\nDownloading the dataset for year ", yr, ":\n")
    dt <- as.data.table(get_pums(variables = c("PUMA", "AGEP"), state = "OH", year = yr, rep_weights = NULL))
    dt[, year := yr]
    facd$stopifna(dt$PUMA)
    stopifnot(nchar(dt$PUMA) <= 5)
    # Area might be needed as a layerId. Do not convert to integer.
    # See https://github.com/rstudio/leaflet/issues/496#issuecomment-1208869160
    dt[, .(Area = str_pad(PUMA, 5, pad = "0"), Year = year, Age = as.integer(AGEP))]
})
facd$dataset <- rbindlist(tmp$dataset_list, use.names = TRUE)
facd$dataset[facd$puma_helper, AreaId := i.AreaId, on = .(Area)]
setkey(facd$dataset, Year, AreaId)
facd$stopifna(facd$dataset)
tmp$dataset_test0 <- facd$dataset[, .N, by = .(Area, Year)]
tmp$dataset_test1 <- tmp$dataset_test0[, .N, by = Area]
stopifnot(tmp$dataset_test1$N == facd$n_years)
stopifnot(tmp$dataset_test1$Area == facd$puma_helper$Area)
facd$age_range <- range(facd$dataset$Age)
facd$min_age <- facd$age_range[1]
facd$max_age <- facd$age_range[2]
facd$properties <- data.table(statistic = c("Counts", "Proportions", "Averages", "Medians"),
  description = c("count", "proportion", "average age", "median age"),
  title = c("Count of respondents", "Proportion of respondents", "Average age of respondents",
  "Median age of respondents"))
tmp$unused_code <- function() {
    # This code explores extreme data handling by geom_histogram. Here are the conclusions:
    # - All bins are (] except for the first bin which is []
    # - For data 0 2 and breaks 0 1 2 the resulting histogram is 1 1
    # - For data 0 2 and breaks -1 0 1 2 the resulting histogram is 1 0 1
    library(ggplot2)
    tmp <- list() # The tmp list is useful for offline testing
    tmp$data <- data.table(x = c(0, 2))
    tmp$breaks <- c(-1, 0, 1, 2)
    ggplot(tmp$data) + geom_histogram(aes(x = x), breaks = tmp$breaks)
}
save(facd, file = "facd.RData")
