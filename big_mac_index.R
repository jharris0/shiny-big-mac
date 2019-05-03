library(dplyr)
library(readr)
library(shiny)
library(shinythemes)
library(leaflet)
library(maps)
library(sp)
library(maptools)
library(DT)
library(qwraps2)

data <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/source-data/big-mac-source-data.csv")

data_2019 <- data %>% filter(date == "2019-01-01")

# euro_area <- c(
#     "Austria", "Belgium", "Cyprus", "Estonia",
#     "Finland", "France", "Germany", "Greece",
#     "Ireland", "Italy", "Latvia", "Lithuania",
#     "Luxembourg", "Malta", "Netherlands", "Portugal",
#     "Slovakia", "Slovenia", "Spain"
# )
# 
# data_temp <- data.frame(euro_area)
# colnames(data_temp) <- "name"
# data_temp <- data_temp %>% mutate(iso_a3 = "",
#                                   currency_code = "",
#                                   local_price = "",
#                                   dollar_ex = "",
#                                   GDP_dollar = "",
#                                   date = "")
# data_temp[,2:7] <- data_2019[data_2019$name == "Euro area", 2:7]
# data_2019 <- rbind(data_2019, data_temp)

data_2019 <- data_2019 %>% mutate(usd_price = round(local_price / dollar_ex, 2))

# make world map with country borders
world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

# fix country names in world map to match names in dataset
world_map[world_map$country == "USA",] <- "United States"
world_map[world_map$country == "UK",] <- "Britain"

# only draw countries that are in the dataset
target <- subset(world_map, country %in% data_2019$name)
target <- target[order(target$country),]

data_2019 <- data_2019[data_2019$name != "Euro area" & data_2019$name != "Hong Kong",]
data_2019 <- data_2019[order(data_2019$name),]
data_2019 <- data_2019 %>% mutate(label = paste0(name, "<br/>",
                                                "USD price: ",
                                                usd_price, "<br/>",
                                                "Local currency: ",
                                                local_price
                                                ))

labs <- lapply(seq(nrow(data_2019)), function(i) {
  paste0("<strong>", data_2019[i, "name"], "</strong><br/>", 
          "USD ", data_2019[i, "usd_price"], "<br/>", 
          data_2019[i, "currency_code"], " ", data_2019[i, "local_price"])
})


# what to do with: euro zone, hong kong, present in data set but not world_map?

bins <- c(1, 2, 3, 4, 5, 6, Inf)
pal <- colorBin("YlOrRd", domain = data_2019$usd_price)

ui <- fluidPage(theme = shinytheme("yeti"),
                # shinythemes::themeSelector(),
                navbarPage("The Big Mac Index",
                           fluidRow(
                             column(
                               3,
                               wellPanel(selectInput(
                                 "date", "Date:",
                                 choices = unique(data_2019$date)
                               )),
                               wellPanel(h4("Top 5 Cheapest"),
                                         tableOutput("least_expensive")),
                               wellPanel(h4("Top 5 Most Expensive"),
                                         tableOutput("most_expensive"))
                             ),
                             column(9,
                                    tabsetPanel(
                                      tabPanel("Map",
                                               leafletOutput("big_mac_map",
                                                             height = 720)),
                                      tabPanel("Charts"),
                                      tabPanel("Table",
                                               dataTableOutput("big_mac_data"))
                                    ))
                             
                           )))

server <- function(input, output, session) {
  output$least_expensive <- renderTable(
    data_2019 %>% arrange(usd_price) %>% top_n(-5, usd_price) %>% select("Country" = name, "Price (USD)" = usd_price)
  )
  
  output$most_expensive <- renderTable(
    data_2019 %>% arrange(-usd_price) %>% top_n(5, usd_price) %>% select("Country" = name, "Price (USD)" = usd_price)
  )
  
  output$big_mac_map <- renderLeaflet({
    leaflet(target) %>%
      addTiles() %>%
      addPolygons(weight = 1,
                  fillColor = ~pal(data_2019$usd_price),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = lapply(labs, HTML),
                  ) %>%
      addLegend(
        pal = pal,
        values = ~data_2019$usd_price,
        title = "Big Mac Price (USD)"
                ) %>%
      setView(0,30,2)
  })
  output$big_mac_data <- renderDataTable(
    data_2019 %>% select(-label, -iso_a3) %>% mutate(dollar_ex = round(dollar_ex, 4))
  )
}

shinyApp(ui, server)