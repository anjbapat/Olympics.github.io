# ## golbal.R ##
# install.packages("dplyr")
# install.packages("googleVis")
# install.packages("rworldmap")
# install.packages("plotly")
# install.packages("ggplot2")
# install.packages("Sp")
# install.packages("rgdal")
# install.packages("ggalt")
# install.packages("ggthemes")
# install.packages("tidyverse")
# install.packages("rsconnect")
# install.packages("shinydashboard")
# install.packages("knitr")
# install.packages("readxl")
# install.packages(shinyloadtest)

library(shinyloadtest)
library(dplyr)
library(googleVis)
library(rworldmap)
library(plotly)
library(ggplot2)
library(googleVis)
library(sp)
library(rgdal)
library(ggalt)
library(ggthemes)
library(tidyverse)
library(rsconnect)
library(shinydashboard)
suppressPackageStartupMessages(library(googleVis))
library(knitr)

rmdfiles <- c("Olympic_about.Rmd")
sapply(rmdfiles, knit, quiet = T)
# 
# ##### DATA FRAMES #####
library(readxl)
athlete_events <- read_excel("athlete_events.xls")

noc_regions = read.csv(file = "noc_regions.csv", stringsAsFactors = FALSE)
# 
# # merge olympic csv data frames #####
# 
df = data.frame(athlete_events)
df_regions = data.frame(noc_regions)

head(df)
head(df_regions)

df_merge = merge(df, df_regions)

df_merge

# # Create new dataframe with medals by country #####

medals_region = df_merge %>% na.omit() %>% group_by(., region) %>% count(.,Medal)

colnames(medals_region)[1] = 'Country'

medals_region$Country = as.character(medals_region$Country)

head(medals_region)

medals_region$Country

# # rename countries #####

medals_region[medals_region$Country == 'USA', ]$Country = 'United States'

medals_region$Country

# # create medal string column #####

medals_region$medal_string = paste0(medals_region$Medal, " : ", medals_region$n)

medals_region1 = medals_region %>%
  group_by(., Country) %>%
  mutate(., medal_string1 = paste0(medal_string, collapse = " ")) %>%
  select(., medal_string1, Country) %>%
  unique()

medals_region1

# # Sport v. Country with Medal Count DataFrame #####

Sport_Country_MedalCount = df_merge %>%
  na.omit %>%
  select(Sport, Medal, region) %>%
  group_by(Sport, region) %>%
  count(Medal)


# # Country Selector #####
Country = unique(df_merge['region'])
# Sport Selector #####
Sport = unique(df_merge['Sport'])
Sport_ = Sport$Sport[order(Sport)]



# ##### GRAPHS #####

# Host City Map #####

world1 = map_data("world")
world1

regioncity = unique(df_merge %>% select(Year, City))

data("world.cities")

regioncitylatlong = world.cities %>% select(name, country.etc, lat, long)

colnames(regioncitylatlong)[1] = 'City'
colnames(regioncitylatlong)[2] = 'Country'

HostCityDF = merge(regioncity, regioncitylatlong)

HostCityDF = HostCityDF[-c(5, 7, 9, 18, 19, 20, 21, 24, 25, 27, 29, 31, 37, 39, 52, 54), ]

head(medals_region)

hostcitymap = ggplotly(
  ggplot() +
    geom_polygon(
      data = world1,
      aes(x = long, y = lat, group = group),
      color = "#888888",
      fill = "#f2caae"
    ) +
    geom_point(data = HostCityDF, aes(x = long, y = lat), color = 'red') +
    theme(
      panel.background = element_rect(
        fill = "lightblue",
        colour = "lightblue",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.major = element_line(
        size = 0.5,
        linetype = 'solid',
        colour = "lightblue"
      ),
      panel.grid.minor = element_line(
        size = 0.25,
        linetype = 'solid',
        colour = "lihtblue"
      )
    )
)

# # World Map Medals by Country #####

Geo = gvisGeoChart(
  medals_region1,
  locationvar = "Country",
  colorvar = "",
  hovervar = "medal_string1"
)


# Top 10 Countries by Sport #####

Sport_Country_MedalCount

#reorder medals

Sport_Country_MedalCount$Medal = factor(Sport_Country_MedalCount$Medal,
                                        levels = c("Gold", "Silver", "Bronze"))

topcountry = ggplot(Sport_Country_MedalCount %>% filter(Sport == "Basketball")) +
  geom_bar(aes(x = region, y = n, fill = Medal), stat = "identity") +
  scale_fill_manual(values = c("gold", "gray69", "peru")) +
  coord_flip()
topcountry

# Countries ordered by medal and total medal count
Sport_Country_MedalCount %>% filter(Sport == "Basketball") %>% arrange(desc(n))
#Countries ordered by total Medal Count
Sport_Country_MedalCount %>%
  filter(Sport == "Basketball") %>%
  summarise(total_medals = sum(n)) %>%
  arrange(desc(total_medals))

# Top Atheletes by Country #####

AtheletesCountry = df_merge %>% group_by(Name, Medal, region) %>% na.omit() %>% select(Name, Medal, region) %>%
  count(Medal)
AtheletesCountry

AtheletesCountry$Medal = factor(AtheletesCountry$Medal,
                                levels = c("Gold", "Silver", "Bronze"))

topathletes = ggplot(AtheletesCountry %>% filter(region == 'USA', n > 5)) +
  geom_bar(aes(x = Name, y = n, fill = Medal), stat = "identity") +
  scale_fill_manual(values = c("gold", "gray69", "peru")) +
  coord_flip()


# # Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Olympic Games Data for 120 years"),
  dashboardSidebar(
    sidebarUserPanel(name="Olympics",image ="https://www.digitalshadows.com/uploads/2018/02/Threats-to-the-Winter-Olympics.png"),
    sidebarMenu(
      menuItem("About", tabName = "About", icon = icon("book")),
      menuItem("World Map", tabName = "worldmap", icon = icon("globe")),
      menuItem(
        "Host City Map",
        tabName = "hostcity",
        icon = icon("map-marker")
      ),
      menuItem("Top Countries", tabName = "topcountry", icon = icon("list")),
      menuItem("Top 10 Athletes", tabName = "topathlete", icon = icon("star"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "worldmap",
              fluidRow(box(
                htmlOutput("worldmap"), width = 12
              ))),
      tabItem(tabName = "hostcity",
              fluidRow(box(
                width = 12, (plotlyOutput(
                  "hostcitymap", height = 700, width = 1150
                ))
              ))),
      
      tabItem(tabName = "topcountry",
              fluidRow(box(
                width = 9, (plotlyOutput(
                  "topcountry", height = 700, width = 800
                ))
              ),
              (
                box(
                  title = "Select Sport",
                  width = 3,
                  selectizeInput("selectedsport", "Select Sport", Sport_, selected = "Basketball")
                )))),
      tabItem(tabName = "topathlete",
              fluidRow(box(
                width = 9, (plotlyOutput(
                  "topathlete", height = 700, width = 800
                ))
              ),
              (
                box(
                  title = "Select Country",
                  width = 3,
                  selectizeInput("selected",
                                                "Select Country",
                                                Country, selected = "USA")
                )))),
      
      tabItem(
        tabName = "About", includeMarkdown("Olympic_about.md")
      )))
  
)

      # Define server logic required to draw a histogram
      server <- function(input, output) {
        
        # World Map #####
        medals_region1$medal_string1 = paste(medals_region1$Country[1:202], medals_region1$medal_string1[1:202], sep = " - ")  
        
        output$worldmap = renderGvis({
          gvisGeoChart(
            medals_region1,
            "Country",
            hovervar = "medal_string1",
            options = list(region="world", displayMode="regions",
                           width = "1100",
                           height = "700"
            ) 
          ) 
        })
        # Host City Map #####
        output$hostcitymap = renderPlotly(
          ggplot(data = HostCityDF, aes(x = long, y = lat)) +
            geom_polygon(
              data = world1,
              aes(x = long, y = lat, group = group),
              color = "#888888",
              fill = "#f2caae"
            ) +
            geom_point(aes(text=paste(City,":", Year)), color = 'red') +
            theme(
              panel.background = element_rect(
                fill = "lightblue",
                colour = "lightblue",
                size = 0.5,
                linetype = "solid"
              ),
              panel.grid.major = element_line(
                size = 0.5,
                linetype = 'solid',
                colour = "lightblue"
              ),
              panel.grid.minor = element_line(
                size = 0.25,
                linetype = 'solid',
                colour = "lihtblue"
              )
            )
        )
        
        
        # Top Country Graph ####
        output$topcountry = renderPlotly(
          
          ggplot(
            Sport_Country_MedalCount %>% filter(Sport == input$selectedsport)
          ) + theme_minimal() +
            geom_bar(aes(
              x = reorder(region,n), y = n, fill = Medal
            ), stat = "identity") +
            scale_fill_manual(values = c("gold", "gray69", "peru")) +
            coord_flip() + xlab("Country") + ylab("Number of Medals")
        )
        
        # Top Athletes ####
        reactive_1 = reactive({
          AtheletesCountry %>%
            filter(region==input$selected) %>%
            spread(.,Medal, n) %>%
            mutate(total = sum(Gold,Silver,Bronze, na.rm=T)) %>%
            arrange(desc(total))
        })
        
        output$topathlete = renderPlotly({
          
          tmp = reactive_1()
          ggplot(
            tmp[1:10,] %>% gather(key="Medal", value="n", c(Gold,Silver,Bronze)) %>% select(-total)  %>%
              arrange(Name)
          ) + theme_minimal() +
            geom_bar(aes(
              x = reorder(Name, -n), y = n, fill = factor(Medal, levels =c("Gold", "Silver", "Bronze"))
            ), stat = "identity") +
            scale_fill_manual(values = c("gold", "gray69", "peru")) +
            coord_flip() + xlab("Athlete") + ylab("Total Medals") +
            guides(fill=guide_legend(title="Medal"))
        })
        
      }
      
      # Run the application
      shinyApp(ui = ui, server = server)
      
      