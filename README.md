## Visualising California's 100 Largest Fires (1877 - 2017)
  
This application was created as a final project for **ESM 244** (Advanced Data Science) -- a course at UCSB's [Bren School of Environmental Science and Management](https://www.bren.ucsb.edu/about/).  

The app allows users to visually explore the top 1000 fires in the last 140 years of California’s fire history. See where fires have occurred in the state over different time frames, which Ecoregions have had the biggest fires, and how total acres burned varies among different causal mechanisms.

Use the widgets on the left hand side of the dashboard to:

*select a date range for the map and explore fire perimeters across the state;
*select a cause of fire and have a look at acres burned over time;
*select the number of fires to include in the pie chart and view the proportion of fires in each Ecoregion. In this widget, 1 represents the largest fire in the state’s history and 1000 represents the smallest (of fires shown in this app). Note that a single fire can occur in multiple Ecoregions.

### Data Source
Data was gathered from Cal Fire's Fire and Resource Assessment Program [(FRAP)](http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download) as a geodatabase file that contained over 20,000 observations with information that included fire names, causes, area burned, and perimeter geometries. This geodatabase was then exported into QGIS and converted into a shapefile for easy import into R via the SF package.

### Required Packages
> library(shiny)  
> library(tidyverse)  
> library(sf)  
> library(leaflet)  
> library(varhandle)  
> library(DT)  
> library(shinydashboard)  
> library(RColorBrewer)
> library(plotly)
> library(ggrepel)


### Authors
* Kyle Monper
* Jenny Balmagia
* Camila Bobroff
