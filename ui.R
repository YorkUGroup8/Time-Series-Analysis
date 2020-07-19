#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(ggplot2)
library(ggfortify)
#install.packages("autoplotly")  # For autoplot
library(autoplotly)
#install.packages("forecast")  # For ggseasonplot
library(forecast)
library(lubridate) # parse_date_time
#install.packages("changepoint")
library(changepoint)  # For Cahnge point
library(stats)
library(dygraphs) # For dygraph
library(dplyr)
library(tseries)
library(vars)

CountryList <- c("Canada")
ProvinceList <- c("Quebec", "Ontario", "British Columbia") # total 3 province
CityList <- c("Quabec, Quebec", "Sherbrooke, Quebec", "Trois-Riviares, Quebec",
              "Montral, Quebec", "Ottawa-Gatineau, Quebec part, Ontario/Quebec", "Ottawa-Gatineau, Ontario part, Ontario/Quebec",
              "Oshawa, Ontario", "Toronto, Ontario", "Hamilton, Ontario", "St. Catharines-Niagara, Ontario",
              "Kitchener-Cambridge-Waterloo, Ontario", "Guelph, Ontario",
              "London, Ontario", "Windsor, Ontario", "Greater Sudbury, Ontario",
              "Kelowna, British Columbia", "Vancouver, British Columbia",
              "Victoria, British Columbia") # total 19 city


#rsconnect::showLogs()
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    dashboardPagePlus(
        skin = "blue",
        #- Dashboard Title
        dashboardHeader(title = span(tagList(icon("Time Series"), "Time Series Analysis")), titleWidth = 700),
        #- Left Menu
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Time-Series", tabName = "TimeSeries", icon = icon("chart-line")),
                    menuSubItem("Interative Time Series", tabName = "ITSeries"), 
                    menuSubItem("Forecast (ARIMA)", tabName = "PTSeries"),
                    menuSubItem("Fit & Residual", tabName = "FTRTSeries"),
                menuItem("Component Analysis", tabName = "sentimental", icon = icon("creative-commons-sampling")),
                    menuSubItem("Changepoint", tabName = "sentimental_1"), 
                    menuSubItem("Time series statistics", tabName = "sentimental_2"),
                    menuSubItem("Time series diagnostics", tabName = "sentimental_3"),
                    menuSubItem("Seasonal Plots", tabName = "sentimental_4"),
                    menuSubItem("Lag plots", tabName = "sentimental_5"),
                    menuSubItem("Autocorrelation", tabName = "sentimental_6"),
                    menuSubItem("Data Visualization Tools", tabName = "sentimental_7"),
                hr(),
                sliderInput(
                    inputId = "selectyear", label = "Year Released", min = as.Date("1980-01-01"),
                    value = c(as.Date("1980-01-01"),as.Date("2020-12-01")), max = as.Date("2020-12-01"),
                    timeFormat = "%b %Y")
                )
        ),
        
        dashboardBody(
            tabItems(
                #- First TAB
                tabItem(
                    tabName = "home",
                    fluidRow(
                        widgetUserBox(
                            title = "Welcome to Group 8 (Time Series Analysis)",
                            subtitle = "Joshua, Amin Samnani and Jad",
                            width = 7,
                            type = 2,
                            src = "image/TimeSeries.png",
                            color = "aqua-active",
                            align = "center",
                            socialButton(
                                url = "https://github.com/YorkUGroup8/Time-Series-Analysis/upload/master",
                                type = "github"
                            ),
                            closable = FALSE,
                            footer = "This online tool was built by Amin Samnani, Jad Zaki, and Joshua Seyoum of
                            York University's Advanced Data Science and Predictive Analytics Program",
                            helpText("Reference material acquired from Grant Morrison, Research Assistant(Time Series Visualization)., please visit",
                                         a("the link", href="https://www.kaggle.com/morrisonge/time-series-visualization/report#introduction-to-ts", target="_blank")
                                )  
                        )
                    )
                ),
                
                #- second TAB
                tabItem(
                    tabName = "ITSeries",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country31", "Select Country Name", CountryList ),
                            selectInput("input_province32", "Select Province Name", ProvinceList),
                            selectInput("input_city133", "Select City Name", CityList),
                            selectInput("input_city234", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            dygraphOutput("timePlot")
                        ))),
                tabItem(
                    tabName = "PTSeries",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country41", "Select Country Name", CountryList ),
                            selectInput("input_province42", "Select Province Name", ProvinceList),
                            selectInput("input_city143", "Select City Name", CityList),
                            selectInput("input_city244", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Forecast package for Each series ARIMA (Auto-Regressive Integrated Moving Average)",align="center"),
                            plotOutput("ForcasttimePlotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("ForcasttimePlotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("ForcasttimePlotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("ForcasttimePlotCityComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "FTRTSeries",
                    fluidRow(
                        box(width = 12, higth = 15,
                            h4("Fit & Residual for Each series)",align="center"),
                            plotOutput("FITResuidal1"),
                            h4("Housing price index time series plot - Forecasting 10 steps ahead", align="center"),
                            hr(),
                            plotOutput("FITResuidal2"),
                            h4("Housing price index time series plot - Forecasting 10 steps ahead", align="center"),
                            hr(),
                            plotOutput("FITResuidalCountry"),
                            h4("Housing price index time series plot - Country Residual", align="center"),
                            hr(),
                            plotOutput("FITResuidalProvince"),
                            h4("Housing price index time series plot - Province Residual", align="center"),
                            hr(),
                            plotOutput("FITResuidalCity"),
                            h4("Housing price index time series plot - City Residual", align="center"),
                            hr(),
                            plotOutput("FITResuidalCityComparison"),
                            h4("Housing price index time series plot - City Comparison Residual",align="center"),
                            br(),
                        )
                    )
                ),
                #- third TAB
                tabItem(
                    tabName = "sentimental_1",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country51", "Select Country Name", CountryList ),
                            selectInput("input_province52", "Select Province Name", ProvinceList),
                            selectInput("input_city153", "Select City Name", CityList),
                            selectInput("input_city254", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Changepoint analysis for time series",align="center"),
                            plotOutput("changeplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("changeplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("changeplotPlotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("changeplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_2",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country61", "Select Country Name", CountryList ),
                            selectInput("input_province62", "Select Province Name", ProvinceList),
                            selectInput("input_city163", "Select City Name", CityList),
                            selectInput("input_city264", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Statistics for time series",align="center"),
                            plotOutput("statisticsplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("statisticsplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("statisticsplotPlotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("statisticsplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_3",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country71", "Select Country Name", CountryList ),
                            selectInput("input_province72", "Select Province Name", ProvinceList),
                            selectInput("input_city173", "Select City Name", CityList),
                            selectInput("input_city274", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Diagnostics for time series",align="center"),
                            plotOutput("diagnosticsplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("diagnosticsplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("diagnosticsplotPlotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("diagnosticsplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_4",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country81", "Select Country Name", CountryList ),
                            selectInput("input_province82", "Select Province Name", ProvinceList),
                            selectInput("input_city183", "Select City Name", CityList),
                            selectInput("input_city284", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Seasonal for time series",align="center"),
                            plotOutput("SeasonalplotCountry"),
                            plotOutput("SeasonalpolarplotCountry"),
                            plotOutput("SeasonalSeriesplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("SeasonalplotProvince"),
                            plotOutput("SeasonalpolarplotProvince"),
                            plotOutput("SeasonalSeriesplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("SeasonalplotPlotCity"),
                            plotOutput("SeasonalpolarplotPlotCity"),
                            plotOutput("SeasonalSeriesplotPlotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("SeasonalplotComparison"),
                            plotOutput("SeasonalpolarplotComparison"),
                            plotOutput("SeasonalSeriesplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_5",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country91", "Select Country Name", CountryList ),
                            selectInput("input_province92", "Select Province Name", ProvinceList),
                            selectInput("input_city193", "Select City Name", CityList),
                            selectInput("input_city294", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Lag plots for time series to identify outliers",align="center"),
                            plotOutput("lagplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("lagplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("lagplotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("lagplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_6",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country101", "Select Country Name", CountryList ),
                            selectInput("input_province102", "Select Province Name", ProvinceList),
                            selectInput("input_city1103", "Select City Name", CityList),
                            selectInput("input_city2104", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Autocorrelation for time series - meassures the linear relationship of a variable and lagged value",align="center"),
                            plotOutput("autocorrelationL1plotCountry"),
                            plotOutput("autocorrelationL2plotCountry"),
                            plotOutput("autocorrelationL3plotCountry"),
                            plotOutput("autocorrelationplotCountry"),
                            h4("Housing price index time series plot - Country", align="center"),
                            hr(),
                            plotOutput("autocorrelationL1plotProvince"),
                            plotOutput("autocorrelationL2plotProvince"),
                            plotOutput("autocorrelationL3plotProvince"),
                            plotOutput("autocorrelationplotProvince"),
                            h4("Housing price index time series plot - Province", align="center"),
                            hr(),
                            plotOutput("autocorrelationL1plotCity"),
                            plotOutput("autocorrelationL2plotCity"),
                            plotOutput("autocorrelationL3plotCity"),
                            plotOutput("autocorrelationplotCity"),
                            h4("Housing price index time series plot - City", align="center"),
                            hr(),
                            plotOutput("autocorrelationL1plotComparison"),
                            plotOutput("autocorrelationL2plotComparison"),
                            plotOutput("autocorrelationL3plotComparison"),
                            plotOutput("autocorrelationplotComparison"),
                            h4("Housing price index time series plot - City Comparison",align="center"),
                            br(),
                        )
                    )
                ),
                tabItem(
                    tabName = "sentimental_7",
                    fluidRow(
                        box(width = 2,
                            selectInput("input_country105", "Select Country Name", CountryList ),
                            selectInput("input_province105", "Select Province Name", ProvinceList),
                            selectInput("input_city1105", "Select City Name", CityList),
                            selectInput("input_city2105", "Select another City Name for comparison", CityList, selected = CityList[2])
                        ),
                        box(width = 10,
                            h4("Data Visualization Tools for Statistical Analysis Results",align="center"),
                            plotOutput("DataVisualCityVsCityComplot"),
                            h4("Housing price index time series plot - GLMs model City ~ City Comparison", align="center"),
                            hr(),
                            plotOutput("DataVisualCountVsProvplot"),
                            h4("Housing price index time series plot - GLMs model Country ~ Province", align="center"),
                            br(),
                        )
                    )
                )
                
                
                
                
                
                
                
                
                
                
                
                
                
                
            )
        )
    )

    
))
