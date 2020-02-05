# eMBS Prepayment (Grabbing live prepayment data from eMBS and presenting data in chart format)

## Purpose
1. using R and Python web-scraping techniques to get data from websites.
2. presenting data using R shiny.
3. testing the reticulate package on shinyapps.io environment.

## Workflow
* Get Pool Level prepayment data from eMBS (https://www.embs.com), The page will be updated monthly.
  * Fannie Mae :  https://www.embs.com/public/html/FNM_eMBSFlashFull.htm
  * Freddie Mac:  https://www.embs.com/public/html/FHL_eMBSFlashFull.htm
    
* Get Interest Rate from Fred. We know prepayment speed is highly related to the changes of interest rate.
  * 30yr : http://www.freddiemac.com/pmms/pmms30.html
  * 15yr : http://www.freddiemac.com/pmms/pmms15.html
 
* Using dygraph package and highchart package for charting. dygraph is a powerful package to present time series data.
   
## Demo
  The live is at  https://dxdinfo.shinyapps.io/eMBSPrepay/
