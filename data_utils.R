
#' get needed data (prepayment and interest rate)
#'
#' @param mode either R or other. If it is R, then use R implementation, otherwise use Python implementation.
#'
#' @return
#' @export
#'
#' @examples
#'      init_data(mode="R")   # using R implementation
#'      init_data(mode="P")   # using Python implemenation through  reticulate package.
init_data <- function(mode="R")
{
  
  parseDate <- function(inDate)
  {
    inDate <- "Updated Jan 07 2020 4:44 PM EST"
    inDate <- trimws(gsub("Updated ", "", inDate))
    a <- strptime(inDate, "%b %d %Y")
  
    return(a)
  }
  
  data = list()
 
  if (mode=="R")
  {
      data_table_FNM <- parse_embs_data( "FNM")
      data_table_FHL <- parse_embs_data( "FHL")
      data[["date"]] <- data_table_FNM[[2]]
      data[["FNM"]] <- data_table_FNM[[1]]
      data[["FHL"]] <- data_table_FHL[[1]]
  } else
  {
    library(reticulate)
    print("using data from Python engine")
   
    if (!Sys.info()[['sysname']] == 'Windows'){
      # running on shinyapps.io, using the virtual environment.
      reticulate::virtualenv_create(envname = 'python35_env', 
                                    python = '/usr/bin/python3')
      reticulate::virtualenv_install('python35_env', 
                                     packages = c('numpy', 'pandas', 'bs4', 'requests'))  # <- Add other packages here, if needed
    }
   
    source_python("data_etl_py.py")
                      
    data_table_FNM_py = parse_embs_data_py("FNM")
    data_table_FHL_py = parse_embs_data_py("FHL")
    
    data[["date"]] <- data_table_FNM_py[[2]]
    data[["FNM"]]  <- data_table_FNM_py[[1]]
    data[["FHL"]]  <- data_table_FHL_py[[1]]
  }
  
  ir_from_date = Sys.Date() %m-% months(14)
  data[["date_date"]] <- parseDate(data[["date"]])
  data[["IR30"]] = get_ir_data_from_FRED(type="30", from_date=ir_from_date)
  data[["IR15"]] = get_ir_data_from_FRED(type="15", from_date=ir_from_date)
  
  return (data)
}

