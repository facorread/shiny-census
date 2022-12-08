library(data.table)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)
library(tidycensus)
# Load the facd container to build the server
load("facd.RData")
tmp <- list()
# It looks like Shiny does not load saved functions on load(); we re-declare them here
facd$cat <- function(...) cat(..., file=stderr(), sep = "")
facd$breaks_function <- function(data_range) {
    # data_range <- c(100, 225) # For offline testing
    # data_range <- c(3729, 10821) # For offline testing
    tmp <- list() # The tmp list is useful for offline testing
    tmp$min <- data_range[1]
    tmp$max <- data_range[2]
    tmp$range_width <- tmp$max - tmp$min
    tmp$step_size_lower_coefficient <- 10 ^ (floor(log10(tmp$range_width)))
    for (step_size_coefficient in c(tmp$step_size_lower_coefficient, 10 * tmp$step_size_lower_coefficient)) {
        # The array of neat_width values must be declared in strictly ascending order
        for (neat_width in 0.01 * c(10, 15, 20, 25, 30, 40, 50, 60, 70, 75, 80, 90)) {
            # neat_width <- 0.1 # For offline testing
            tmp$step_size <- step_size_coefficient * neat_width
            tmp$first_break <- tmp$min - (tmp$min %% tmp$step_size)
            if ((tmp$min >= 0) && (tmp$first_break < 0)) {
                tmp$first_break <- 0
            }
            tmp$breaks <- seq(from = tmp$first_break, to = tmp$max, by = tmp$step_size)
            tmp$last_break <- last(tmp$breaks)
            if (tmp$last_break < tmp$max) {
                # Virtually always
                tmp$last_break <- tmp$last_break + tmp$step_size
                tmp$breaks <- c(tmp$breaks, tmp$last_break)
            }
            # breaks_score scores each partition according to how close it comes to 7 bins and
            # how close the last break comes to tmp$max
            tmp$breaks_score <- abs(length(tmp$breaks) - 7) +
              (tmp$last_break - tmp$max + tmp$min - tmp$first_break) / tmp$range_width
            # facd$cat("step_size ", tmp$step_size, " length ", length(tmp$breaks),
            #   " breaks", paste0(" ", tmp$breaks), "\n")
            if (tmp$breaks_score == 0) {
                return(tmp$breaks)
            } else if (is.null(tmp$smallest_score) || (tmp$breaks_score < tmp$smallest_score)) {
                tmp$smallest_score <- tmp$breaks_score
                tmp$best_breaks <- tmp$breaks
            }
        }
    }
    tmp$best_breaks
}
stopifnot(facd$breaks_function(c(100, 225)) == c(100, 125, 150, 175, 200, 225))
stopifnot(facd$breaks_function(c(100, 125)) == c(100, 105, 110, 115, 120, 125))
stopifnot(facd$breaks_function(c(-125, -100)) == c(-125, -120, -115, -110, -105, -100))
stopifnot(facd$breaks_function(c(1000, 1025)) == c(1000, 1005, 1010, 1015, 1020, 1025))
stopifnot(facd$breaks_function(c(3729, 10821)) == c(3000, 4500, 6000, 7500, 9000, 10500, 12000))

# See related article and code by Stefan Haring (2020)
# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
setShapeStyle <- function(map, data = getMapData(map), layerId, stroke = NULL, color = NULL,
  weight = NULL, opacity = NULL, fill = NULL, fillColor = NULL, fillOpacity = NULL, dashArray = NULL,
  smoothFactor = NULL, noClip = NULL, label = NULL, options = NULL) {
  options <- c(list(layerId = layerId), options, filterNULL(list(stroke = stroke, color = color,
    weight = weight, opacity = opacity, fill = fill, fillColor = fillColor,
    fillOpacity = fillOpacity, dashArray = dashArray, smoothFactor = smoothFactor, noClip = noClip,
    label = label)))
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId <- options[[1]]
  style <- options[-1]
  if ("label" %in% colnames(style)) {
    # labelData <- style[,"label", FALSE]
    style <- style[, -which(colnames(style) == "label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style)
}

ui <- navbarPage("Census demo by Fabio",
  tabPanel("Interactive map",
    div(class = "outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("shiny-census.js")
      ),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 360, height = "auto",
        h2("Census demo by Fabio"),
        p("Last updated: 2022-12-08"),
        p(a(href = "github.com/facorread/shiny-census", "View source code on GitHub")),
        p("This demo presents data filtered by age from the US Census Bureau's American Community Survey (ACS)",
          a(href = "https://www.census.gov/programs-surveys/acs/microdata.html",
          "Public Use Microdata Sample (PUMS).")),
        textOutput("general_description"),
        # See related article and code by Stefan Haring (2020)
        # https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1

        uiOutput("facdUI"),
        checkboxInput("show_names", "Show area names"),

        plotOutput("puma_histogram", height = 200)
      ),
    )
  ),

  tabPanel("Data explorer",
    textOutput("puma_table_description"),
    DT::dataTableOutput("puma_table")
  ),
  id = "nav",
  # Hack to make sure icons are loaded. This hack is currently unused.
  footer = conditionalPanel("false", icon("crosshairs"))
)

server <- function(input, output, session) {
  output$facdUI <- renderUI(list(
    sliderInput("age_slider", "Age filter (years)",
      min = facd$min_age,
      max = facd$max_age,
      value = facd$age_range,
      step = as.integer(1)
    ),
    sliderInput("year_slider", "Publication year of the ACS-5 survey",
      min = facd$min_year,
      max = facd$max_year,
      value = facd$min_year,
      step = as.integer(1),
      ticks = FALSE,
      animate = animationOptions(interval = 1000, loop = TRUE),
      sep = ""
    ),
    selectInput("statistic", "Statistic:", facd$properties$statistic)
  ))
  # Create the map
  output$map <- renderLeaflet({
      leaflet(facd$sf) %>%
      addTiles()  %>%
      fitBounds(-84.8203, 38.40342, -80.51845, 42.32713) %>%
      addPolygons(
        layerId = facd$puma_helper$Area,
        fillColor = "lightgray",
        stroke = TRUE,
        fillOpacity = 1,
        color = "white",
        weight = 1
      )
  })
  age_sel_reactive <- reactive({
    # input <- list(age_slider = c(0, 95), statistic = "Proportions") # Offline testing
    # req <- function(x) x # Offline testing
    tmp <- list() # The tmp list is useful for offline testing
    age_sel <- list()
    age_sel$age_slider <- req(input$age_slider)
    if (length(age_sel$age_slider) == 1) {
      if (age_sel$age_slider > 0) {
        age_sel$age_slider <- c(0, age_sel$age_slider)
      } else {
        age_sel$age_slider <- c(age_sel$age_slider, 1)
      }
      updateSliderInput(session, "age_slider", value = age_sel$age_slider)
    } else if (age_sel$age_slider[1] == age_sel$age_slider[2]) {
      if (age_sel$age_slider[1] > 0) {
        age_sel$age_slider[1] <- age_sel$age_slider[1] - 1
      } else {
        age_sel$age_slider[2] <- 1
      }
      updateSliderInput(session, "age_slider", value = age_sel$age_slider)
    }
    if (req(input$statistic) == "Counts") {
      tmp$per_year_puma <- facd$dataset[Age %between% age_sel$age_slider, .(stat = .N), by = .(Year, AreaId)]
    } else if(input$statistic == "Proportions") {
      tmp$per_year_puma <- facd$dataset[, .(stat = mean(Age %between% age_sel$age_slider)), by = .(Year, AreaId)]
      tmp$per_year_puma[!is.finite(stat), stat := NA]
      tmp$per_year_puma <- na.omit(tmp$per_year_puma)
    } else if(input$statistic == "Averages") {
      tmp$per_year_puma <- facd$dataset[Age %between% age_sel$age_slider, .(stat = mean(Age)), by = .(Year, AreaId)]
      tmp$per_year_puma[!is.finite(stat), stat := NA]
      tmp$per_year_puma <- na.omit(tmp$per_year_puma)
    } else if(input$statistic == "Medians") {
      tmp$per_year_puma <- facd$dataset[Age %between% age_sel$age_slider, .(stat = median(Age)), by = .(Year, AreaId)]
      tmp$per_year_puma[!is.finite(stat), stat := NA]
      tmp$per_year_puma <- na.omit(tmp$per_year_puma)
    } else {
      stop("Invalid input for the statistic control")
    }
    age_sel$statistic <- input$statistic
    age_sel$description <- facd$properties[statistic == age_sel$statistic, description]
    age_sel$title <- facd$properties[statistic == age_sel$statistic, title]
    age_sel$range <- range(tmp$per_year_puma$stat)
    if (anyNA(age_sel$range)) {
      age_sel$range <- c(0, 1)
    }
    if (age_sel$range[1] == age_sel$range[2]) {
      if (age_sel$range[1] > 0) {
        age_sel$range[1] <- 0
      } else if (age_sel$range[1] < 0) {
        age_sel$range[2] <- 0
      } else {
        age_sel$range[2] <- 1
      }
    }
    age_sel$breaks <- facd$breaks_function(age_sel$range)
    age_sel$per_year_hist <- tmp$per_year_puma[, .(gplot = list(ggplot(.SD) + geom_histogram(
      aes(x = stat), breaks = age_sel$breaks) + scale_x_continuous(name = age_sel$title)
      + scale_y_continuous(name = "Frequency (PUMAs)"))), keyby = Year]
    age_sel$per_year_puma <- facd$year_puma_helper[tmp$per_year_puma, on = .(Year, AreaId)]
    age_sel$color_function <- colorBin(palette = "YlOrBr", domain = age_sel$range,
      na.color = "transparent", bins = age_sel$breaks)
    age_sel$color_range <- age_sel$color_function(age_sel$range)
    age_sel$per_year_puma[, color := age_sel$color_function(stat)]
    age_sel$per_puma <- age_sel$per_year_puma[, .(common_label = paste0(Year, ": ", ifelse(is.finite(stat), stat,
      "No respondents"), collapse = "<br>")), keyby = AreaId]
    stopifnot(age_sel$per_puma$AreaId == facd$puma_helper$AreaId)
    age_sel$per_puma[, short_label := paste0("<b>", input$statistic, ":</b><br>", common_label)]
    age_sel$per_puma[, long_label := paste0(facd$puma_helper$Name, "<br>", input$statistic, ":<br>", common_label)]
    age_sel$per_puma_display <- dcast(age_sel$per_year_puma, Area ~ Year, value.var = "stat")
    age_sel$per_puma_display[, Name := facd$puma_helper$Name]
    output$puma_table <- DT::renderDataTable(age_sel$per_puma_display)
    output$general_description <- renderText(paste0("Each PUMA (Public Use Microdata Area) in this map is colored according to the ",
      age_sel$description, " of respondents in the age range ", age_sel$age_slider[1],
      " - ", age_sel$age_slider[2], "."))
    output$puma_table_description <- renderText(paste0("This table shows the ",
      age_sel$description, " of respondents in each PUMA for the age range ", age_sel$age_slider[1],
      " - ", age_sel$age_slider[2], "."))
    age_sel
  })
  observe({
    # input <- list(age_slider = c(0, 95), statistic = "Counts", year_slider = 2016) # Offline testing
    req(input$year_slider)
    age_sel <- age_sel_reactive()
    puma_stats <- age_sel$per_year_puma[Year == input$year_slider]
    leafletProxy("map", data = facd$sf) %>%
    clearMarkers() %>%
    addLegend(position = "bottomleft", pal = age_sel$color_function,
      values = puma_stats$stat, opacity = 1,
      title = age_sel$title, layerId = "map_legend") %>%
      setShapeStyle(layerId = puma_stats$Area, fillColor = puma_stats$color, label = if (isTruthy(input$show_names))
      age_sel$per_puma$long_label else age_sel$per_puma$short_label
    )
    output$puma_histogram <- renderPlot(age_sel$per_year_hist[Year == input$year_slider, gplot[[1]]])
  })

}

shinyApp(ui = ui, server = server)
