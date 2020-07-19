#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


housing_canada<- read.csv("CSV/housing_canada.csv")
housing_canada_city<- read.csv("CSV/housing_canada_city.csv")

#rsconnect::showLogs()



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$timePlot <- renderDygraph({
        
        country <- input$input_country31
        province <- input$input_province32
        city1 <- input$input_city133
        city2 <- input$input_city234
        
        start_date <- parse_date_time("1980-01-01",  "%Y-%m-%d")
        end_date <- parse_date_time("2020-12-01",  "%Y-%m-%d")
        plyr::vaggregate
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        
        start_year1 <- min(housing_sp1$yr)  # min start date of province level 1
        start_year2 <- min(housing_sp2$yr)  # min start date of province level 2
        start_year3 <- min(housing_sp3$yr)  # min start date of city level 1
        start_year4 <- min(housing_sp4$yr)  # min start date of city level 2
        
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1, 1), frequency = 12) # ts for province level 1
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2, 1), frequency = 12) # ts for province level 2
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3, 1), frequency = 12) # ts for city level 1
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4, 1), frequency = 12) # ts for city level 2
        
        All <- cbind (time_series_Country, time_series_Province, time_series_City, time_series_CityComparison) # combine province 1 & 2 and city 1 & 2
        
        dygraph (All , main = "monthly housing index times series" ,xlab="Year" ) %>%
            dySeries("time_series_Country", label= country, drawPoints = TRUE) %>%
            dySeries("time_series_Province", label= province, drawPoints = TRUE) %>%
            dySeries("time_series_City", label= city1, drawPoints = TRUE) %>%
            dySeries("time_series_CityComparison", label= city2, drawPoints = TRUE) %>%
            dyAxis("y",label = "housing price index") %>%
            dyAxis("y2", label = "Years") %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE )  %>%
            dyRangeSelector(dateWindow = c(start_date, end_date)) %>%
            dyRoller()

        })

    
    output$ForcasttimePlotCountry <- renderPlot({
        
        country <- input$input_country41
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
            
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        #start_year1 <- min(housing_sp1$yr)  # min start date of province level 1
        #time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1, 1), frequency = 12) # ts for province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year12 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year12) , frequency = 12) # ts for province level 1

        
        # Plotting with forecast package for Each series ARIMA (Auto-Regressive Integrated Moving Average)
        d.arima1 <- auto.arima(time_series_Country)
        d.forecast1 <- forecast(d.arima1, level = c(50), h = 50)
        autoplot(d.forecast1  , title = "sss" , xlab = "Years", ylab = "housing price index" ) # for province level 1
    })
    
    output$ForcasttimePlotProvince <- renderPlot({
        
        province <- input$input_province42
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        
        # Plotting with forecast package for Each series ARIMA (Auto-Regressive Integrated Moving Average)
        d.arima2 <- auto.arima(time_series_Province)
        d.forecast2 <- forecast(d.arima2, level = c(50), h = 50)
        autoplot(d.forecast2  , title = "sss" , xlab = "Years", ylab = "housing price index" ) # for province level 2
    })
    
    output$ForcasttimePlotCity <- renderPlot({
        
        city1 <- input$input_city143
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        
        # Plotting with forecast package for Each series ARIMA (Auto-Regressive Integrated Moving Average)
        d.arima3 <- auto.arima(time_series_City)
        d.forecast3 <- forecast(d.arima3, level = c(50), h = 50)
        autoplot(d.forecast3  , title = "sss" , xlab = "Years", ylab = "housing price index" ) # for city level 1
    })
        
    output$ForcasttimePlotCityComparison <- renderPlot({
        
        city2 <- input$input_city244
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        
        # Plotting with forecast package for Each series ARIMA (Auto-Regressive Integrated Moving Average)
        d.arima4 <- auto.arima(time_series_CityComparison)
        d.forecast4 <- forecast(d.arima4, level = c(50), h = 50)
        autoplot(d.forecast4  , title = "sss" , xlab = "Years", ylab = "housing price index" ) # for city level 2
    })

    
    output$FITResuidal1 <- renderPlot({
        
        country <- input$input_country41
        province <- input$input_province42
        city1 <- input$input_city143
        city2 <- input$input_city244
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        
        All <- cbind (time_series_Country, time_series_Province, time_series_City, time_series_CityComparison) # combine province 1 & 2 and city 1 & 2
        cases <- c(country, province, city1, city2)
        
        colnames(All) <- paste0(cases) # assign name of column
        All <-  na.remove(All)  # remove na
        
        var.2c <- VAR(All, p = 2, type = "const") # generate estimation result
        
        # forecasting 10 steps ahead
        #estimated with the function VAR() and as deterministic regresses a constant is included
        var.f10 <- predict(var.2c, n.ahead = 10, ci = 0.95, ts.colour = 'dodgerblue4',
                          predict.colour = 'blue', predict.linetype = 'dashed' )
        autoplot(var.f10)
        
    })
    
    
    output$FITResuidal2 <- renderPlot({
        
        country <- input$input_country41
        province <- input$input_province42
        city1 <- input$input_city143
        city2 <- input$input_city244
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        
        All <- cbind (time_series_Country, time_series_Province, time_series_City, time_series_CityComparison) # combine province 1 & 2 and city 1 & 2
        cases <- c(country, province, city1, city2)
        
        colnames(All) <- paste0(cases) # assign name of column
        All <-  na.remove(All)  # remove na
        
        var.2c <- VAR(All, p = 2, type = "const") # generate estimation results
        
        # forecasting 10 steps ahead
        #estimated with the function VAR() and as deterministic regresses a constant is included
        var.f10 <- predict(var.2c, n.ahead = 10, ci = 0.95, ts.colour = 'dodgerblue4',
                          predict.colour = 'blue', predict.linetype = 'dashed' )
        fanchart(var.f10)
        
    })
    
    
    output$FITResuidalCountry <- renderPlot({
        
        country <- input$input_country41
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1

        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
       
        fit<- auto.arima(time_series_Country, seasonal = FALSE)
        tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
        
    })
    
    output$FITResuidalProvince <- renderPlot({
        
        province <- input$input_province42
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
    
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        fit<- auto.arima(time_series_Province, seasonal = FALSE)
        tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
        
    })
    
    output$FITResuidalCity <- renderPlot({
        
        city1 <- input$input_city143
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        fit<- auto.arima(time_series_City, seasonal = FALSE)
        tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
        
    })
    
    output$FITResuidalCityComparison <- renderPlot({
        
        city2 <- input$input_city244
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        fit<- auto.arima(time_series_CityComparison, seasonal = FALSE)
        tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
        
    })
    
    output$changeplotCountry <- renderPlot({
        
        country <- input$input_country51
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with changepoint package #OK
        autoplot(cpt.meanvar(time_series_Country) , title = "sss \n dd" , xlab = "Years", ylab = "housing price index" ) # for province level 1
    })
    
    output$changeplotProvince <- renderPlot({
        
        province <- input$input_province52
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with changepoint package #OK
        autoplot(cpt.meanvar(time_series_Province) , title = "sss \n dd" , xlab = "Years", ylab = "housing price index" ) # for province level 2
    })
    
    output$changeplotPlotCity <- renderPlot({
        
        city1 <- input$input_city153
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with changepoint package #OK
        autoplot(cpt.meanvar(time_series_City) , title = "sss \n dd" , xlab = "Years", ylab = "housing price index" ) # for city level 1
    })
    
    output$changeplotComparison <- renderPlot({
        
        city2 <- input$input_city254
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        
        #Plotting with changepoint package #OK
        autoplot(cpt.meanvar(time_series_CityComparison) , title = "sss \n dd" , xlab = "Years", ylab = "housing price index" ) # for city level 2
        
    })
    
    output$statisticsplotCountry <- renderPlot({
        
        country <- input$input_country61
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        
        #Plotting with changepoint package #OK
        autoplot(stl(time_series_Country, s.window = 'periodic'), ts.colour = 'blue', ylab = "housing price index", main ="time series statistics") # for province level 1
    })
    
    output$statisticsplotProvince <- renderPlot({
        
        province <- input$input_province62
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with changepoint package #OK
        autoplot(stl(time_series_Province, s.window = 'periodic'), ts.colour = 'blue', ylab = "housing price index", main ="time series statistics") # for province level 2
    })
    
    output$statisticsplotPlotCity <- renderPlot({
        
        city1 <- input$input_city163
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        
        #Plotting with changepoint package #OK
        autoplot(stl(time_series_City, s.window = 'periodic'), ts.colour = 'blue', ylab = "housing price index", main ="time series statistics") # for city level 1
    })
    
    output$statisticsplotComparison <- renderPlot({
        
        city2 <- input$input_city264
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with changepoint package #OK
        autoplot(stl(time_series_CityComparison, s.window = 'periodic'), ts.colour = 'blue', ylab = "housing price index", main ="time series statistics") # for city level 2
    })
    
    output$diagnosticsplotCountry <- renderPlot({
        
        country <- input$input_country71
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with changepoint package #OK
        ggtsdiag(auto.arima(time_series_Country)  , title = "sss \n dd") # # for province level 1
    })
    
    output$diagnosticsplotProvince <- renderPlot({
        
        province <- input$input_province72
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with changepoint package #OK
        ggtsdiag(auto.arima(time_series_Province)  , title = "sss \n dd" ) # # for province level 2
    })
    
    output$diagnosticsplotPlotCity <- renderPlot({
        
        city1 <- input$input_city173
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with changepoint package #OK
        ggtsdiag(auto.arima(time_series_City)  , title = "sss \n dd" ) # # for city level 1
    })
    
    output$diagnosticsplotComparison <- renderPlot({
        
        city2 <- input$input_city274
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with changepoint package #OK
        ggtsdiag(auto.arima(time_series_CityComparison)  , title = "sss \n dd" ) # # for city level 2
    })
    
    output$SeasonalplotCountry <- renderPlot({
        
        country <- input$input_country81
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with Monthly  seasonality package #OK
        ggseasonplot(time_series_Country, year.labels=TRUE, year.labels.left=TRUE) +
            ylab("housing index price") +
            ggtitle("Seasonal plot: monthly housing index prices - Country")
    })
    
    output$SeasonalplotProvince <- renderPlot({
        
        province <- input$input_province82
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with Monthly  seasonality package #OK
        ggseasonplot(time_series_Province, year.labels=TRUE, year.labels.left=TRUE) +
            ylab("housing index price") +
            ggtitle("Seasonal plot: monthly housing index prices - Province")
    })
    
    output$SeasonalplotPlotCity <- renderPlot({
        
        city1 <- input$input_city183
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with Monthly  seasonality package #OK
        ggseasonplot(time_series_City, year.labels=TRUE, year.labels.left=TRUE) +
            ylab("housing index price") +
            ggtitle("Seasonal plot: monthly housing index prices - City")
    })
    
    output$SeasonalplotComparison <- renderPlot({
        
        city2 <- input$input_city284
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with Monthly  seasonality package #OK
        ggseasonplot(time_series_CityComparison, year.labels=TRUE, year.labels.left=TRUE) +
            ylab("housing index price") +
            ggtitle("Seasonal plot: monthly housing index prices - City Comparison")
    })
    
    output$SeasonalpolarplotCountry <- renderPlot({
        
        country <- input$input_country81
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with polar package #OK
        ggseasonplot(time_series_Country, polar=TRUE) +
            ylab("$ million") +
            ggtitle("Polar seasonal plot: housing index prices - Country")
    })
    
    output$SeasonalpolarplotProvince <- renderPlot({
        
        province <- input$input_province82
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with polar package #OK
        ggseasonplot(time_series_Province, polar=TRUE) +
            ylab("$ million") +
            ggtitle("Polar seasonal plot: housing index prices - Province")
    })
    
    output$SeasonalpolarplotPlotCity <- renderPlot({
        
        city1 <- input$input_city183
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with polar package #OK
        ggseasonplot(time_series_City, polar=TRUE) +
            ylab("$ million") +
            ggtitle("Polar seasonal plot: housing index prices - City")
    })
    
    output$SeasonalpolarplotComparison <- renderPlot({
        
        city2 <- input$input_city284
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with polar package #OK
        ggseasonplot(time_series_CityComparison, polar=TRUE) +
            ylab("$ million") +
            ggtitle("Polar seasonal plot: housing index prices - City Comparison")
    })
    
    output$SeasonalSeriesplotCountry <- renderPlot({
        
        country <- input$input_country81
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with subseries package #OK
        ggsubseriesplot(time_series_Country) +
            ylab("$ million") +
            ggtitle("Seasonal subseries plot: housing index prices - Country")
    })
    
    output$SeasonalSeriesplotProvince <- renderPlot({
        
        province <- input$input_province82
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with subseries package #OK
        ggsubseriesplot(time_series_Province) +
            ylab("$ million") +
            ggtitle("Seasonal subseries plot: housing index prices - province")
    })
    
    output$SeasonalSeriesplotPlotCity <- renderPlot({
        
        city1 <- input$input_city183
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with subseries package #OK
        ggsubseriesplot(time_series_City) +
            ylab("$ million") +
            ggtitle("Seasonal subseries plot: housing index prices - City")
    })
    
    output$SeasonalSeriesplotComparison <- renderPlot({
        
        city2 <- input$input_city284
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with subseries package #OK
        ggsubseriesplot(time_series_CityComparison) +
            ylab("$ million") +
            ggtitle("Seasonal subseries plot: housing index prices - City Comparison")
    })
    
    output$lagplotCountry <- renderPlot({
        
        country <- input$input_country91
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        #Plotting with lag package #OK
        gglagplot(time_series_Country, main= "Lag Plot : housing index prices - Country") # for province level 1
    })
    
    output$lagplotProvince <- renderPlot({
        
        province <- input$input_province92
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        #Plotting with lag package #OK
        gglagplot(time_series_Province, main= "Lag Plot : housing index prices - province") # for province level 1
    })
    
    output$lagplotCity <- renderPlot({
        
        city1 <- input$input_city193
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        #Plotting with lag package #OK
        gglagplot(time_series_City, main= "Lag Plot : housing index prices - city") # for province level 1
    })
    
    output$lagplotComparison <- renderPlot({
        
        city2 <- input$input_city294
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        #Plotting with lag package #OK
        gglagplot(time_series_CityComparison, main= "Lag Plot : housing price index - city comparison") # for city level 2
    })
    
    output$autocorrelationL1plotCountry <- renderPlot({
        
        country <- input$input_country101
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        lag1 <- tail(housing_sp1$VALUE,-1) # last part of dataset of province level 1
        qplot(x=housing_sp1$VALUE,y=c(lag1,0), main = "Lag 1 : housing index prices - Country" )
    })
    
    output$autocorrelationL2plotCountry <- renderPlot({
        
        country <- input$input_country101
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        lag2 <- tail(housing_sp1$VALUE,-2)
        qplot(x=housing_sp1$VALUE,y=c(lag2,0,0), main = "Lag 2 : housing index prices - Country" )
    })
    
    output$autocorrelationL3plotCountry <- renderPlot({
        
        country <- input$input_country101
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        lag3 <- tail(housing_sp1$VALUE,-3)
        qplot(x=housing_sp1$VALUE,y=c(lag3,0,0,0), main = "Lag 3 : housing index prices - country" )
    })
    
    output$autocorrelationplotCountry <- renderPlot({
        
        country <- input$input_country101
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        
        ggAcf(time_series_Country, main = "housing index prices - Country" )
    })
    
    
    output$autocorrelationL1plotProvince <- renderPlot({
        
        province <- input$input_province102
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        lag1 <- tail(housing_sp2$VALUE,-1) # last part of dataset of province level 2
        qplot(x=housing_sp2$VALUE,y=c(lag1,0), main = "Lag 1 : housing price index - province" )
    })
    
    output$autocorrelationL2plotProvince <- renderPlot({
        
        province <- input$input_province102
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        lag2 <- tail(housing_sp2$VALUE,-2)
        qplot(x=housing_sp2$VALUE,y=c(lag2,0,0), main = "Lag 2 : housing price index - province" )
    })
    
    output$autocorrelationL3plotProvince <- renderPlot({
        
        province <- input$input_province102
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        lag3 <- tail(housing_sp2$VALUE,-3)
        qplot(x=housing_sp2$VALUE,y=c(lag3,0,0,0), main = "Lag 3 : housing price index - province" )
    })
    
    output$autocorrelationplotProvince <- renderPlot({
        
        province <- input$input_province102
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        ggAcf(time_series_Province, main = "housing price index - province" )
    })
    
    output$autocorrelationL1plotCity <- renderPlot({
        
        city1 <- input$input_city1103
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        lag1 <- tail(housing_sp3$VALUE,-1) # last part of dataset of city level 1
        qplot(x=housing_sp3$VALUE,y=c(lag1,0), main = "Lag 1 : housing price index - city" )
    })
    
    output$autocorrelationL2plotCity <- renderPlot({
        
        city1 <- input$input_city1103
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        lag2 <- tail(housing_sp3$VALUE,-2)
        qplot(x=housing_sp3$VALUE,y=c(lag2,0,0), main = "Lag 2 : housing price index - city" )
    })
    
    output$autocorrelationL3plotCity <- renderPlot({
        
        city1 <- input$input_city1103
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        lag3 <- tail(housing_sp3$VALUE,-3)
        qplot(x=housing_sp3$VALUE,y=c(lag3,0,0,0), main = "Lag 3 : housing price index - city" )
    })
    
    output$autocorrelationplotCity <- renderPlot({
        
        city1 <- input$input_city1103
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        
        ggAcf(time_series_City, main = "housing price index - city" )
    })
    
    
    output$autocorrelationL1plotComparison <- renderPlot({
        
        city2 <- input$input_city2104
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        lag1 <- tail(housing_sp4$VALUE,-1) # last part of dataset of city level 2
        qplot(x=housing_sp4$VALUE,y=c(lag1,0), main = "Lag 1 : housing price index - city comparison" )
    })
    
    
    output$autocorrelationL2plotComparison <- renderPlot({
        
        city2 <- input$input_city2104
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        lag2 <- tail(housing_sp4$VALUE,-2)
        qplot(x=housing_sp4$VALUE,y=c(lag2,0,0), main = "Lag 2 : housing price index - city comparison" )
    })
    
    output$autocorrelationL3plotComparison <- renderPlot({
        
        city2 <- input$input_city2104
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        lag3 <- tail(housing_sp4$VALUE,-3)
        qplot(x=housing_sp4$VALUE,y=c(lag3,0,0,0), main = "Lag 3 : housing price index - city comparison" )
    })
    
    output$autocorrelationplotComparison <- renderPlot({
        
        city2 <- input$input_city2104
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        ggAcf(time_series_CityComparison, main = "housing price index - city comparison" )
    })
    
    
    output$DataVisualCityVsCityComplot <- renderPlot({
        
        city1 <- input$input_city1105
        city2 <- input$input_city2105
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp3 <- housing_canada_city %>% filter( GEO == city1) # City level 1
        housing_sp4 <- housing_canada_city %>% filter( GEO == city2) # city level 2
        
        start_year3 <- min(year(start_date))  # min start date of city level 1
        start_year31 <- min(year(end_date))  # min start date of city level 1
        start_year4 <- min(year(start_date))  # min start date of city level 2
        start_year41 <- min(year(end_date))  # min start date of city level 2
        
        time_series_City <- ts(housing_sp3$VALUE, start = c(start_year3), end = c(start_year31), frequency = 12) # ts for city level 1
        time_series_CityComparison <- ts(housing_sp4$VALUE, start = c(start_year4), end = c(start_year41), frequency = 12) # ts for city level 2
        
        All <- cbind (time_series_City, time_series_CityComparison) # combine province 1 & 2 and city 1 & 2
        
        model<-glm(time_series_City~time_series_CityComparison, data=All)
        autoplot(model, colour="red")
        
    })
    
    output$DataVisualCountVsProvplot <- renderPlot({
        
        country <- input$input_country105
        province <- input$input_province105
        
        start_date <- parse_date_time(input$selectyear[1],  "%Y-%m-%d")
        end_date <- parse_date_time(input$selectyear[2],  "%Y-%m-%d")
        
        housing_sp1 <- housing_canada %>% filter( GEO == country) # province level 1
        housing_sp2 <- housing_canada %>% filter( GEO == province) # province level 2
        
        start_year1 <- min(year(start_date))  # min start date of province level 1
        start_year11 <- min(year(end_date))  # min start date of province level 1
        start_year2 <- min(year(start_date))  # min start date of province level 2
        start_year21 <- min(year(end_date))  # min start date of province level 2
        
        time_series_Country <- ts(housing_sp1$VALUE, start = c(start_year1), end = c(start_year11), frequency = 12) # ts for province level 1
        time_series_Province <- ts(housing_sp2$VALUE, start = c(start_year2), end = c(start_year21), frequency = 12) # ts for province level 2
        
        All <- cbind (time_series_Country, time_series_Province) # combine province 1 & 2 and city 1 & 2
        
        model<-glm(time_series_Country~time_series_Province, data=All)
        autoplot(model, colour="red")
        
    })
    
})
