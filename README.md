## Visualising California's 100 Largest Fires (1877 - 2017)
  
This application was created as a final project for **ESM 244** (Advanced Data Science) -- a course at UCSB's [Bren School of Environmental Science and Management](https://www.bren.ucsb.edu/about/).  


### Data Source
Data was gathered from Cal Fire's Fire and Resource Assessment Program [(FRAP)](http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download) as a geodatabase file that contained information on fire names, causes, area burned, and perimeter geometries. This geodatabase was then exported into ArcMap and converted into a shapefile for easy import into R via the SF package

### Required Packages
> library(shiny)  
> library(tidyverse)  
> library(sf)  
> library(leaflet)  
> library(varhandle)  
> library(DT)  
> library(shinydashboard)  
> library(fontawesome)   

### Authors
* Kyle Monper
* Jenny Balmagia
* Camila Bobroff
