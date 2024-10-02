#### Load required libraries ####
library(shiny)
library(DT)
library(openxlsx)
library(bslib)
library(bsicons)
library(highcharter)
library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(purrr)
library(shinyTime)

#### Create tab function for UI ####
pill <- function(...) {
  shiny::tabPanel(..., class = "p-3 border rounded thick-border tab-body")
}

#### Read JSON and structure the data for multi drill down chart####
# Read the JSON file line by line
jsondata <- readLines("mwdrilldown.json")

# Paste the lines together as a string
jsondata <- paste(jsondata, collapse = "")

# Parse the JSON data
json_data <- jsonlite::fromJSON(jsondata)

# Extract data for Hydro
dataParentLayerHydel <- json_data$DataForParentOne$DataParentLayerHydel

# Initialize an empty list for the desired output
desiredOutputHydel <- list()

# Loop through the data and format it into the desired structure
for (entry in dataParentLayerHydel) {
  desiredOutputHydel <- c(
    desiredOutputHydel,
    list(
      list(name = entry$name, y = entry$y, drilldown = entry$drilldown)
    )
  )
}

# Extract data for Renewable
dataParentLayerRenewable <- json_data$DataForParentOne$DataParentLayerRenewable

# Initialize an empty list for the desired output
desiredOutputRenewable <- list()

# Loop through the data and format it into the desired structure
for (entry in dataParentLayerRenewable) {
  desiredOutputRenewable <- c(
    desiredOutputRenewable,
    list(
      list(name = entry$name, y = entry$y, drilldown = entry$drilldown)
    )
  )
}

# Extract data for Nuclear
dataParentLayerNuclear <- json_data$DataForParentOne$DataParentLayerNuclear

# Initialize an empty list for the desired output
desiredOutputNuclear <- list()

# Loop through the data and format it into the desired structure
for (entry in dataParentLayerNuclear) {
  desiredOutputNuclear <- c(
    desiredOutputNuclear,
    list(
      list(name = entry$name, y = entry$y, drilldown = entry$drilldown)
    )
  )
}

# Extract data for Thermal
dataParentLayerThermal <- json_data$DataForParentOne$DataParentLayerThermal

# Initialize an empty list for the desired output
desiredOutputThermal <- list()

# Loop through the data and format it into the desired structure
for (entry in dataParentLayerThermal) {
  desiredOutputThermal <- c(
    desiredOutputThermal,
    list(
      list(name = entry$name, y = entry$y, drilldown = entry$drilldown)
    )
  )
}

#### JS Code for Multi drill down chart ####
# Define the JavaScript code
js_code <- "function(e) {
    var javascriptArray = %s;
    var dataParentLayerIPPS = javascriptArray.DataForParentOne.DataParentLayerIPPS;
    var dataParentLayerGENCOS = javascriptArray.DataForParentOne.DataParentLayerGENCOS;
    var desiredOutputIPPS = [];
    var desiredOutputGENCOS = [];
// Loop through the data and push each entry to the desired output array
    for (var date in dataParentLayerGENCOS) {
        if (dataParentLayerGENCOS.hasOwnProperty(date)) {
            desiredOutputGENCOS.push(dataParentLayerGENCOS[date]);
        }
    }
    // Loop through the data and push each entry to the desired output array
    for (var date in dataParentLayerIPPS) {
        if (dataParentLayerIPPS.hasOwnProperty(date)) {
            desiredOutputIPPS.push(dataParentLayerIPPS[date]);
        }
    }
    var specificDate = javascriptArray[e.point.name];
    var chart = this,
    Hydel = specificDate.Hydel,

    Renewables = specificDate.Renewable,
    
   

    Thermal = {

        'IPPS': {
            name: 'IPPS',
            color: 'Brown',
            data: desiredOutputIPPS,
            stack: 'move'

        },


        'GENCOS': {

            name: 'GENCOS',
            color: 'DarkGrey',
            data: desiredOutputGENCOS,
            stack: 'move'

        }

    },


    IPPS = specificDate.IPPS,


    Gencos = specificDate.GENCOS,



    Nuclear = specificDate.Nuclear
    
    
    
    
    if (e.point.color == 'blue') {
      chart.addSingleSeriesAsDrilldown(e.point, Hydel.Hydel);
    } else if (e.point.color == 'green') {
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Solar);
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Wind);
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Bagasse);
      
    } else if (e.point.color == 'red') {
      chart.addSingleSeriesAsDrilldown(e.point, Thermal.IPPS);
      chart.addSingleSeriesAsDrilldown(e.point, Thermal.GENCOS);
      
    } else if (e.point.color == 'orange') {
      chart.addSingleSeriesAsDrilldown(e.point, Nuclear.Nuclear);
      
    }
    
    
    if (e.point.color == 'Brown') {
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.Gas);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.Coal);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.RLNG);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.FO);
    } else if (e.point.color == 'DarkGrey') {
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.Gas);
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.RLNG);
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.Coal);
    }
    
    
    chart.applyDrilldown();
  }"

# Replace %s in the JavaScript code with the actual JSON data
js_code <- sprintf(js_code, jsondata)

#### Read files ####
data <- read.csv("ENRG1.csv") 
df_normal <- read.csv("normalreserve.csv")
df_spinning <- read.csv("spinningreserve.csv")
df_capacity <- read.csv("generationcapacity.csv")
df_gasposition <- read.csv("gasposition.csv")
df_frequency <- read.csv("frequency.csv")
df_capability <- read.csv("capability.csv")
df_constraint <- read.csv("constraint.csv")
df_matiari <- read.csv("matiari.csv")
df_kesc <- read.csv("kesc.csv")
df_card <- read.csv("MW.csv")
df_card <- na.omit(df_card)
df_grouped <- df_card %>%
  group_by(date, time, TYPE) %>%
  summarise(EnergyMW = sum(EnergyMW, na.rm = TRUE)) %>%
  ungroup()

#### UI #####
ui <- page_navbar(
  theme = bs_theme(bootswatch = "default"),
  title = "DLR",
  nav_spacer(),
  nav_panel("Energy Generation", icon = icon("bolt"),
            accordion(
              id = "infoAccordion",
              open = TRUE, # Keep this accordion open by default
              accordion_panel(
                "Generation",
                fluidRow(
                  column(width = 3, 
                         dateInput("start_date", 
                                   "Select Start Date:", 
                                   value = as.Date(min(df_grouped$date), "%d-%m-%Y"))),
                  column(width = 3, 
                         selectInput("start_hour", 
                                     "Select Start Hour:", 
                                     choices = sprintf("%02d", 0:23))),
                  column(width = 3, 
                         dateInput("end_date", 
                                   "Select End Date:", 
                                   value = as.Date(max(df_grouped$date), "%d-%m-%Y"))),
                  column(width = 3, 
                         selectInput("end_hour", 
                                     "Select End Hour:", 
                                     choices = sprintf("%02d", 0:23)))
                ),
                fluidRow(
                  column(width = 3, uiOutput("hydelCardhourly")),
                  column(width = 3, uiOutput("renewableCardhourly")),
                  column(width = 3, uiOutput("nuclearCardhourly")),
                  column(width = 3, uiOutput("thermalCardhourly"))
                )
              ),
              accordion_panel(
                "Overview",
                fluidRow(
                  uiOutput("drilldownChart")
                )
              ),
              accordion_panel(
                "Plant Generation",
                fluidRow(
                  column(
                    width = 4,
                    selectInput("select_plant", "Select Plant:", choices = unique(df_card$NAME), multiple = TRUE, selected = unique(df_card$NAME)[1])
                  ),
                  column(
                    width = 4,
                    selectInput("select_plant_date", "Select Date:", choices = unique(df_card$date))
                  ),
                  column(
                    width = 4,
                    uiOutput("plantGenerationCard")
                  )
                ),
                fluidRow(
                  uiOutput("energyLineGraph")
                )
              ),
              accordion_panel(
                "Yearly Generation",
                fluidRow(
                  uiOutput("yearlyGenerationChart")
                )
              ),
              accordion_panel(
                "Load Factor",
                fluidRow(
                  uiOutput("loadFactorGenerationChart")
                )
              )
            )
  ),
  nav_panel("Events", icon = icon("calendar"),
            navset_card_pill(
              full_screen =TRUE,
              nav_panel(
                "Normal Reserve",
                uiOutput("normalReserveChart")
              ),
              nav_panel(
                "Spinning Reserve",
                uiOutput("spinningReserveChart")
              ),
              nav_panel(
                "Unutilized Generation Units",
                uiOutput("generationCapacityChart")
              )
            )
  ),
  nav_panel("Operational Performace", icon = icon("chart-line"),
            navset_card_pill(
              full_screen =TRUE,
              nav_panel(
                "Gas Position",
                uiOutput("gasPositionChart")
              ),
              nav_panel(
                "Daily Frequency",
                uiOutput("dailyFrequencyChart")
              ),
              nav_panel(
                "Present Dependable Capacity",
                uiOutput("presentDependableCapabilityChart")
              ),
              nav_panel(
                "DISCO's Constraint",
                uiOutput("discoConstraintChart")
              )
            )
  ),
  nav_panel("Matiari Conversion Station", icon = icon("exchange-alt"),
            card(
              uiOutput("matiariChart")
            )
  ),
  nav_panel("KESC", icon = icon("industry"),
            card(
              uiOutput("kescChart")
            )
  )
)

#### Server ####
server <- function(input, output, session) {
  #### Update Time Input in Overview Tab ####
  df_grouped$time <- gsub("24:00", "00:00", df_grouped$time)
  df_grouped$date <- as.Date(df_grouped$date, "%d-%m-%Y")
  
  df_grouped$datetime <- as.POSIXct(paste(df_grouped$date, df_grouped$time), format = "%Y-%m-%d %H:%M")
  
  reactive_range <- reactive({
    # Check if inputs are valid before conversion
    if (is.null(input$start_date) || is.null(input$start_hour) || is.null(input$end_date) || is.null(input$end_hour)) {
      return(NULL)
    }
    
    # Correct format string for POSIXct conversion with a space between date and time
    start_datetime <- tryCatch(
      as.POSIXct(paste(input$start_date, paste(input$start_hour, "00", "00", sep = ":"), sep = " "), format = "%Y-%m-%d %H:%M:%S"),
      error = function(e) NA
    )
    
    end_datetime <- tryCatch(
      as.POSIXct(paste(input$end_date, paste(input$end_hour, "00", "00", sep = ":"), sep = " "), format = "%Y-%m-%d %H:%M:%S"),
      error = function(e) NA
    )
    
    # Print for debugging
    
    list(start = start_datetime, end = end_datetime)
  })
  
  #### Multi Drill Down Chart ####
  output$drilldownChart <- renderUI({
    hc <- highchart() %>%
      hc_chart(
        type = "column",
        events = list(
          click = JS('function (e) {
        var pointName = e.point.name;
        Shiny.onInputChange("pointName", pointName);
      }')
          ,
          drilldown = JS(js_code)
        )
      ) %>%
      hc_xAxis(type = "category") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_tooltip(
        valueSuffix = " MW"
      ) %>%
      hc_add_series(
        name = "Hydel",
        color = "blue",
        data = desiredOutputHydel
      ) %>%
      hc_add_series(
        name = "Renewable",
        color = "green",
        data = desiredOutputRenewable
      ) %>%
      hc_add_series(
        name = "Nuclear",
        color = "orange",
        data = desiredOutputNuclear
      ) %>%
      hc_add_series(
        name = "Thermal",
        color = "red",
        data = desiredOutputThermal
      )
    
    hc
  })
  
  #### Yearly Generation Chart ####
  output$yearlyGenerationChart <- renderUI({
    # Define color mapping for each type
    type_colors <- c(
      "BAGASSE" = "green",
      "FOSSIL FUEL" = "black",
      "GENCO" = "maroon",
      "HYDEL" = "blue",
      "NUCLEAR" = "red",
      "SOLAR" = "orange",
      "WIND" = "lightblue"
    )
    
    # Create a summary data frame grouped by TYPE
    summary_data <- data %>%
      group_by(TYPE) %>%
      summarise(presentYear_sum = sum(presentYear))
    
    # Prepare the main series data for highchart
    main_data <- list(
      list(
        name = "Present Dependable Capability",
        data = list_parse(
          summary_data %>%
            mutate(
              y = presentYear_sum,
              drilldown = paste0(TYPE, "-drilldown"),
              color = type_colors[TYPE]  # Assign colors based on TYPE
            ) %>%
            select(name = TYPE, y, drilldown, color)
        )
      )
    )
    
    # Function to create drilldown data for each TYPE
    create_drilldown_data <- function(type) {
      drilldown_data <- data %>%
        filter(TYPE == type) %>%
        select(NAME, presentYear)
      
      list(
        id = paste0(type, "-drilldown"),
        name = "Present Year",
        data = list_parse2(
          drilldown_data %>%
            mutate(
              y = presentYear,
              color = type_colors[type]  # Assign colors for drilldown data
            ) %>%
            select(name = NAME, y, color)
        )
      )
    }
    
    # Create drilldown series
    drilldown_series <- lapply(summary_data$TYPE, create_drilldown_data)
    
    # Create the highchart
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category", title = list(text = "Type")) %>%
      hc_yAxis(title = list(text = "Energy (KWH)")) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = FALSE),
          grouping = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Present Year",
        data = main_data[[1]]$data,
        colorByPoint = TRUE
      ) %>%
      hc_drilldown(
        series = drilldown_series
      ) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_tooltip(
        pointFormat = paste0('<td style="padding:0"><b>{point.y} KWH</b></td></tr>'),
        footerFormat = '</table>',
        shared = TRUE,
        useHTML = TRUE
      )
    
    # Print the chart
    hc
  })
  
  #### Load Factor Generation Chart ####
  output$loadFactorGenerationChart <- renderUI({
    data$loadFactor <- round(data$loadFactor, 2)
    
    # Define color mapping for each type
    type_colors <- c(
      "BAGASSE" = "green",
      "FOSSIL FUEL" = "black",
      "GENCO" = "maroon",
      "HYDEL" = "blue",
      "NUCLEAR" = "red",
      "SOLAR" = "orange",
      "WIND" = "lightblue"
    )
    
    # Calculate the total load factor
    total_loadFactor <- sum(data$loadFactor)
    
    # Create a summary data frame grouped by TYPE and calculate percentages
    summary_data <- data %>%
      group_by(TYPE) %>%
      summarise(loadFactor_sum = sum(loadFactor)) %>%
      mutate(loadFactor_sum = (loadFactor_sum / total_loadFactor) * 100)
    
    # Prepare the main series data for highchart
    main_data <- list(
      list(
        name = "Load Factor",
        data = list_parse(
          summary_data %>%
            mutate(
              y = round(loadFactor_sum, 2),
              drilldown = paste0(TYPE, "-drilldown"),
              color = type_colors[TYPE]  # Assign colors based on TYPE
            ) %>%
            select(name = TYPE, y, drilldown, color)
        )
      )
    )
    
    # Function to create drilldown data for each TYPE
    create_drilldown_data <- function(type) {
      drilldown_data <- data %>%
        filter(TYPE == type) %>%
        select(NAME, loadFactor)
      
      list(
        id = paste0(type, "-drilldown"),
        name = "Present Year",
        data = list_parse2(
          drilldown_data %>%
            mutate(
              y = loadFactor,
              color = type_colors[type]  # Assign colors for drilldown data
            ) %>%
            select(name = NAME, y, color)
        )
      )
    }
    
    # Create drilldown series
    drilldown_series <- lapply(summary_data$TYPE, create_drilldown_data)
    
    # Create the highchart
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category", title = list(text = "Type")) %>%
      hc_yAxis(title = list(text = "Percentage %")) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = FALSE),
          grouping = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Load Factor",
        data = main_data[[1]]$data,
        colorByPoint = TRUE
      ) %>%
      hc_drilldown(
        series = drilldown_series
      ) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_tooltip(
        pointFormat = paste0('<td style="padding:0"><b>{point.y} %</b></td></tr>'),
        footerFormat = '</table>',
        shared = TRUE,
        useHTML = TRUE
      )
    
    # Print the chart
    hc
  })
  
  #### Normal Reserve Chart ####
  output$normalReserveChart <- renderUI({
    # Create a summary data frame grouped by TYPE
    summary_data <- df_normal %>%
      group_by(FUEL) %>%
      summarise(MW_sum = sum(MW))
    
    # Prepare the main series data for highchart
    main_data <- list(
      list(
        name = "Normal Cold Reserve",
        data = list_parse(summary_data %>%
                            mutate(y = MW_sum,
                                   drilldown = paste0(FUEL, "-drilldown")) %>%
                            select(name = FUEL, y, drilldown))
      )
    )
    
    # Function to create drilldown data for each TYPE
    create_drilldown_data <- function(type) {
      drilldown_data <- df_normal %>%
        filter(FUEL == type) %>%
        select(STATION, MW)
      
      list(
        id = paste0(type, "-drilldown"),
        name = "Normal Cold Reserve",
        data = list_parse2(drilldown_data %>% 
                             mutate(y = MW) %>%
                             select(name = STATION, y))
      )
    }
    
    # Create drilldown series
    drilldown_series <- lapply(summary_data$FUEL, create_drilldown_data)
    
    # Create the highchart
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category", title = list(text = "Type")) %>%
      hc_yAxis(title = list(text = "Reserve (MW)")) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = FALSE),
          grouping = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Reserve (MW)",
        data = main_data[[1]]$data,
        colorByPoint = TRUE
      ) %>%
      hc_drilldown(
        series = drilldown_series
      ) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_tooltip(
        pointFormat = paste0('<td style="padding:0"><b>{point.y} MW</b></td></tr>')
      )
    
    # Print the chart
    hc
  })
  
  #### Spinning Reserve Chart ####
  output$spinningReserveChart <- renderUI({
    # Create a bar graph using highcharter
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = df_spinning$STATION, title = list(text = "Station")) %>%
      hc_yAxis(title = list(text = "MW")) %>%
      hc_add_series(name = "Reserve (MW)", data = df_spinning$MW) %>%
      hc_tooltip(
        pointFormat = paste0('<td style="padding:0"><b>{point.y} MW</b></td></tr>')
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
    
    # Print the highchart object
    hc
  })
  
  #### Unutilized Generation Units Chart####
  output$generationCapacityChart <- renderUI({
    # Aggregate data at different levels
    type_summary <- df_capacity %>%
      group_by(TYPE) %>%
      summarise(installedCapacity = sum(installedCapacity),
                deratedCapacity = sum(deratedCapacity))
    
    name_summary <- df_capacity %>%
      group_by(TYPE, NAME) %>%
      summarise(installedCapacity = sum(installedCapacity),
                deratedCapacity = sum(deratedCapacity))
    
    unit_summary <- df_capacity %>%
      group_by(NAME, UNIT) %>%
      summarise(installedCapacity = sum(installedCapacity),
                deratedCapacity = sum(deratedCapacity))
    
    # Create the highchart with drilldown
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Capacity")) %>%
      hc_plotOptions(column = list(stacking = NULL, borderWidth = 0)) %>%
      hc_series(list(
        name = "Installed Capacity",
        data = lapply(1:nrow(type_summary), function(i) {
          list(
            name = type_summary$TYPE[i],
            y = type_summary$installedCapacity[i],
            drilldown = paste0("type_", type_summary$TYPE[i], "_installed")
          )
        }),
        stack = "installed"
      ),
      list(
        name = "Derated Capacity",
        data = lapply(1:nrow(type_summary), function(i) {
          list(
            name = type_summary$TYPE[i],
            y = type_summary$deratedCapacity[i],
            drilldown = paste0("type_", type_summary$TYPE[i], "_derated")
          )
        }),
        stack = "derated"
      )) %>%
      hc_drilldown(
        series = c(
          lapply(unique(name_summary$TYPE), function(type) {
            list(
              id = paste0("type_", type, "_installed"),
              name = "Installed Capacity",
              data = lapply(1:nrow(name_summary[name_summary$TYPE == type,]), function(j) {
                row <- name_summary[name_summary$TYPE == type,][j, ]
                list(
                  name = row$NAME,
                  y = row$installedCapacity,
                  drilldown = paste0("name_", row$NAME, "_installed")
                )
              }),
              type = "column"
            )
          }),
          lapply(unique(name_summary$TYPE), function(type) {
            list(
              id = paste0("type_", type, "_derated"),
              name = "Derated Capacity",
              data = lapply(1:nrow(name_summary[name_summary$TYPE == type,]), function(j) {
                row <- name_summary[name_summary$TYPE == type,][j, ]
                list(
                  name = row$NAME,
                  y = row$deratedCapacity,
                  drilldown = paste0("name_", row$NAME, "_derated")
                )
              }),
              type = "column"
            )
          }),
          lapply(unique(unit_summary$NAME), function(name) {
            list(
              id = paste0("name_", name, "_installed"),
              name = "Installed Capacity",
              data = lapply(1:nrow(unit_summary[unit_summary$NAME == name,]), function(k) {
                row <- unit_summary[unit_summary$NAME == name,][k, ]
                list(
                  name = as.character(row$UNIT),
                  y = row$installedCapacity
                )
              }),
              type = "column"
            )
          }),
          lapply(unique(unit_summary$NAME), function(name) {
            list(
              id = paste0("name_", name, "_derated"),
              name = "Derated Capacity",
              data = lapply(1:nrow(unit_summary[unit_summary$NAME == name,]), function(k) {
                row <- unit_summary[unit_summary$NAME == name,][k, ]
                list(
                  name = as.character(row$UNIT),
                  y = row$deratedCapacity
                )
              }),
              type = "column"
            )
          })
        )
      ) %>%
      hc_tooltip(
        pointFormat = paste0('<td style="padding:0"><b>{point.y} MW</b></td></tr>')
      )
  })
  
  #### Gas Position Chart ####
  output$gasPositionChart <- renderUI({
    # Create the dual-axis chart
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = df_gasposition$Plant) %>%
      hc_add_series(name = "Gas Allocated", data = df_gasposition$gas_allocated, color = "#5CD678", yAxis = 0) %>%
      hc_add_series(name = "Gas Consumed", data = df_gasposition$gas_consumed, color = "#FA7777", yAxis = 0) %>%
      hc_add_series(name = "Energy Generated", data = df_gasposition$energy_generated, type = "line", color = "#6590C6", yAxis = 1) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE),
        grouping = TRUE,
        pointPadding = 0,
        groupPadding = 0.2
      )) %>%
      hc_yAxis_multiples(
        list(title = list(text = "Gas Volume"), opposite = FALSE),
        list(title = list(text = "Energy Generated"), opposite = TRUE)
      ) %>%
      hc_tooltip(
        shared = FALSE,
        formatter = JS("
      function() {
        var unit = '';
        if (this.series.name === 'Gas Allocated' || this.series.name === 'Gas Consumed') {
          unit = ' MMCF';
        } else if (this.series.name === 'Energy Generated') {
          unit = ' MW';
        }
        return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + this.y + unit;
      }
    ")
      )
  })
  
  
  #### Daily Frequency Chart ####
  output$dailyFrequencyChart <- renderUI({
    # Create a list of data points with color
    data_points <- lapply(1:nrow(df_frequency), function(i) {
      list(name = df_frequency$TIME[i], y = df_frequency$FREQUENCY[i], color = df_frequency$color[i])
    })
    
    # Create the highcharter plot with scatter and non-interactive line series
    highchart() %>%
      hc_add_series(name = "Frequency (Scatter)", data = data_points, type = "scatter") %>%
      hc_add_series(name = "Frequency (Line)", data = df_frequency$FREQUENCY, type = "line", enableMouseTracking = FALSE, showInLegend = FALSE) %>%
      hc_xAxis(categories = df_frequency$TIME, title = list(text = "Time"), type = 'category') %>%
      hc_plotOptions(scatter = list(marker = list(radius = 5))) %>%
      hc_tooltip(pointFormat = '<b>Time:</b> {point.name}<br><b>Frequency:</b> {point.y}')
  })
  
  #### Present Dependable Capability Chart ####
  output$presentDependableCapabilityChart <- renderUI({
    # Create a summary data frame grouped by TYPE
    summary_data <- df_capability %>%
      group_by(TYPE) %>%
      summarise(present_dependable_capability_sum = sum(present_dependable_capability))
    
    # Prepare the main series data for highchart
    main_data <- list(
      list(
        name = "Present Dependable Capability",
        data = list_parse(summary_data %>%
                            mutate(y = present_dependable_capability_sum,
                                   drilldown = paste0(TYPE, "-drilldown")) %>%
                            select(name = TYPE, y, drilldown))
      )
    )
    
    # Function to create drilldown data for each TYPE
    create_drilldown_data <- function(type) {
      drilldown_data <- df_capability %>%
        filter(TYPE == type) %>%
        select(NAME, present_dependable_capability)
      
      list(
        id = paste0(type, "-drilldown"),
        name = "Present Dependable Capability",
        data = list_parse2(drilldown_data %>% 
                             mutate(y = present_dependable_capability) %>%
                             select(name = NAME, y))
      )
    }
    
    # Create drilldown series
    drilldown_series <- lapply(summary_data$TYPE, create_drilldown_data)
    
    # Create the highchart
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category", title = list(text = "Type")) %>%
      hc_yAxis(title = list(text = "Capacity")) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = FALSE),
          grouping = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Present Dependable Capability",
        data = main_data[[1]]$data,
        colorByPoint = TRUE
      ) %>%
      hc_drilldown(
        series = drilldown_series
      ) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE)
        )
      )
    
    # Print the chart
    hc
  })
  
  #### DISCO's Constraint Chart ####
  output$discoConstraintChart <- renderUI({
    # Create highchart
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = df_constraint$NAME) %>%
      hc_add_series(name = "Maximum", data = df_constraint$MAXIMUM, color = "#5CD678") %>%
      hc_add_series(name = "Minimum", data = df_constraint$MINIMUM, color = "#FA7777") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE),
        grouping = TRUE,
        pointPadding = 0,
        groupPadding = 0.2
      )) %>%
      hc_yAxis(
        title = list(text = "Constraint"),
        opposite = FALSE
      )
  })
  
  #### Matiari Conversion Station Chart ####
  output$matiariChart <- renderUI({
    hc <- highchart(type = "stock") %>%
      hc_title(text = "Matiari Conversion Station Energy Consumption Over Time") %>%
      hc_add_series(data = df_matiari %>% select(timestamp, Energy) %>% list_parse2(), 
                    type = "line", 
                    name = "Energy") %>%
      hc_xAxis(type = "datetime", title = list(text = "DateTime")) %>%
      hc_yAxis(title = list(text = "Energy (MW)")) %>%
      hc_tooltip(valueSuffix = " MW")
    
    hc
  })
  
  #### KESC Chart ####
  output$kescChart <- renderUI({
    hc <- highchart(type = "stock") %>%
      hc_title(text = "KESC Hourly Load (MW)") %>%
      hc_add_series(data = df_kesc %>% select(timestamp, Energy) %>% list_parse2(), 
                    type = "line", 
                    name = "Load",
                    color = "#009198") %>%
      hc_xAxis(type = "datetime", title = list(text = "DateTime")) %>%
      hc_yAxis(title = list(text = "Load")) %>%
      hc_tooltip(valueSuffix = " MW")
    
    hc
  })
  
  #### value boxes hourly ####
  output$hydelCardhourly <- renderUI({
    req(reactive_range()$start, reactive_range()$end)
    filtered_data <- df_grouped %>%
      filter(datetime >= reactive_range()$start, datetime <= reactive_range()$end, TYPE == 'HYDEL')
    
    sum_hydel <- sum(filtered_data$EnergyMW, na.rm = TRUE)
    value_box(
      title = "Hydel Generation",
      value = paste0(sum_hydel, " MW"),
      showcase = bs_icon("water"),
      theme = "primary"
    )
  })
  
  output$renewableCardhourly <- renderUI({
    req(reactive_range()$start, reactive_range()$end)
    filtered_data <- df_grouped %>%
      filter(datetime >= reactive_range()$start, datetime <= reactive_range()$end, TYPE == 'RENEWABLE')
    
    sum_renewable <- sum(filtered_data$EnergyMW, na.rm = TRUE)
    value_box(
      title = "Renewable Generation",
      value = paste0(sum_renewable, " MW"),
      showcase = bs_icon("recycle"),
      theme = "teal"
    )
  })
  
  output$nuclearCardhourly <- renderUI({
    req(reactive_range()$start, reactive_range()$end)
    filtered_data <- df_grouped %>%
      filter(datetime >= reactive_range()$start, datetime <= reactive_range()$end, TYPE == 'NUCLEAR')
    
    sum_nuclear <- sum(filtered_data$EnergyMW, na.rm = TRUE)
    value_box(
      title = "Nuclear Generation",
      value = paste0(sum_nuclear, " MW"),
      showcase = bs_icon("radioactive"),
      theme = "orange"
    )
  })
  
  output$thermalCardhourly <- renderUI({
    req(reactive_range()$start, reactive_range()$end)
    filtered_data <- df_grouped %>%
      filter(datetime >= reactive_range()$start, datetime <= reactive_range()$end, TYPE == 'THERMAL')
    
    sum_thermal <- sum(filtered_data$EnergyMW, na.rm = TRUE)
    value_box(
      title = "Thermal Generation",
      value = paste0(round(sum_thermal, 2), " MW"),
      showcase = bs_icon("fire"),
      theme = "red"
    )
  })
  #### Plant generation value box ####
  output$plantGenerationCard <- renderUI({
    req(input$select_plant, input$select_plant_date)
    
    # Filter data for the selected plants and date
    filtered_data <- df_card %>%
      filter(NAME %in% input$select_plant, date == input$select_plant_date)
    
    # Calculate the sum of EnergyMW for the selected plants and date
    sum_energy <- sum(filtered_data$EnergyMW, na.rm = TRUE)
    
    # Handle single or multiple selected plants
    if (length(input$select_plant) == 1) {
      # Single plant selected
      plant_type <- unique(filtered_data$TYPE)
      plant_icon <- switch(plant_type,
                           HYDEL = "water",
                           RENEWABLE = "recycle",
                           NUCLEAR = "radioactive",
                           THERMAL = "fire",
                           "bolt" # Default icon
      )
      
      plant_color <- switch(plant_type,
                            HYDEL = "primary",    # Blue
                            RENEWABLE = "teal",   # Teal
                            NUCLEAR = "orange",   # Orange
                            THERMAL = "red",      # Red
                            "secondary"           # Default color
      )
      
      # Display the value box for a single plant
      value_box(
        title = paste(input$select_plant, "Generation"),
        value = paste0(sum_energy, " MW"),
        showcase = bs_icon(plant_icon),
        theme = plant_color
      )
      
    } else {
      # Multiple plants selected
      # Use a generic icon and color for multiple plants
      title_total <- "Total Generation for Selected Plants"
      total_icon <- "bolt"
      total_color <- "info"
      total_sum <- paste0(sum_energy, " MW")
      # Display the value box for multiple plants
      value_box(
        title = title_total,
        value = total_sum,
        showcase = icon(total_icon),
        theme = total_color
      )
    }
  })
  
  #### Plant Generation Line Graph ####
  output$energyLineGraph <- renderUI({
    req(input$select_plant, input$select_plant_date)
    
    # Filter data for the selected plants and date
    filtered_data <- df_card %>%
      filter(NAME %in% input$select_plant, date == input$select_plant_date)
    
    # Create a highchart object for the line graph
    highchart_obj <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = unique(filtered_data$time), title = list(text = "Time")) %>%
      hc_yAxis(title = list(text = "Energy (MW)"))
    
    # Add series for each selected plant
    for (plant in input$select_plant) {
      plant_data <- filtered_data %>%
        filter(NAME == plant)
      
      highchart_obj <- highchart_obj %>%
        hc_add_series(name = plant, data = plant_data$EnergyMW) %>%
        hc_tooltip(pointFormat = '<b>{series.name}<br><b>Energy:</b> {point.y} MW')
    }
    
    # Render the combined chart
    highchart_obj
  })
  
}

#### Run App ####
shinyApp(ui = ui, server = server)