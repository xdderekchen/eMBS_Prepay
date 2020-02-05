
#' get data from eMBS through web scraping
#'
#' @description get IR either from "https://www.embs.com/public/html/FNM_eMBSFlash.htm" or "https://www.embs.com/public/html/FHL_eMBSFlash.htm"
#'
#' @param source either FNM or FHL
#'
#' @return data.frame()
#' @export
#'
#' @examples
#'      parse_embs_data(source = "FNM")   #Fannie Mae data
#'      parse_embs_data(source = "FHL")   #Freddie Mac data
#'      
parse_embs_data <- function(source = "FNM")
{
   fnma_URL = "https://www.embs.com/public/html/FNM_eMBSFlash.htm"
   fred_URL = "https://www.embs.com/public/html/FHL_eMBSFlash.htm"
   
   url = ""
   if (source == "FNM") {
      url = fnma_URL
   } else if (source == "FHL")
   {
      url = fred_URL
   }
   if (url == ""){
      return (NULL)
   }
   
   raw_data <- xml2::read_html(url)
   data <- raw_data %>% html_nodes("p")
   updated_DateTime <- xml_text(data)

   tables <- raw_data %>% html_nodes("table")
   data   <- tables[[2]] %>% html_nodes("a")
   content_table <- bind_rows(lapply(data, 
                                  function(x) data.frame(as.list(xml_attrs(x)), 
                                                         name = xml_text(x), 
                                                         stringsAsFactors=FALSE)))

   data_table   <- tables[[3]] %>% html_table(fill = TRUE)

   data_table_ETL <- function(df, source) {
       #First row is the column names
       colnames <- as.character(unlist(df[1,]))
       names(df) <- sapply(colnames, function(x) ifelse(x=="", "Category", x))
       df <- df[-1,]
       df <- df[!apply(df == "", 1, all),]
       df$Category <-  sapply(df$Category, function(x) ifelse(x=="", NA, x))
       df <- tidyr::fill(df, Category)
       df <- df[!(df$Category == df$Coupon) ,  ]
       df$Agency <- source
       df$Term =   ifelse(df$Category %in% c("FNM30","FHLG30",   "Freddie Mac 30yr"), "30",
                     ifelse(df$Category %in% c("FNM20","FHLG20", "Freddie Mac 20yr"), "20",
                            ifelse(df$Category %in% c("FNM15","FHLG15", "Freddie Mac 15yr"), "15", 
                                   ifelse(df$Category %in% c("FNM10","FHLG10", "Freddie Mac 10yr"), "10",
                                          ifelse(df$Category %in% c("LIBOR"), df$Coupon, "")))))
    
    
       head_col = c("Agency", "Category", "Term", "Coupon", "Prodn Yr")
       data_col = setdiff(names(df), head_col)
       num_converter <- function(x)
       {
           return (as.numeric(gsub(",|%", "",x)))
       }
       df[data_col] <- sapply(df[data_col],num_converter)
 
       return ( df[, c(head_col, data_col)])
   }

   data_table <- data_table_ETL(data_table, source)
   return (list(data_table, updated_DateTime))
}


#' get interest rate from fred
#'
#' @description get IR either from "http://www.freddiemac.com/pmms/pmms15.html" or "http://www.freddiemac.com/pmms/pmms30.html"
#'
#'  @param type either 30 or 15
#' @param from_date like "2010-01-01", get data after this date.
#'
#' @return  data.frame()
#' @export
#'
#' @examples
get_ir_data_from_FRED <- function(type="30", from_date="2010-01-01")
{
   url30 = "http://www.freddiemac.com/pmms/pmms30.html"
   url15 = "http://www.freddiemac.com/pmms/pmms15.html"
   url =""
   if (type == "15")
   {
      url = url15
   } else if (type == "30")
   {
      url = url30
   }
   
   if (url == ""){
      return (NULL)
   }
   raw_data <- xml2::read_html(url)
   tables <- raw_data %>% html_nodes("table")

   mydata <- list()
   n = 1
   for (j in (1:length(tables)))
   {
      data0   <- tables[[j]] %>% html_table(fill = TRUE)
      for (i in seq(2, dim(data0)[2], by=2))
      {
          col_rate <- as.numeric(data0[3:14, i]) + as.numeric(data0[3:14, i+1]) / 4 
          col_date <- as.Date(paste(data0[1, i], data0[3:14, 1], "01"), format = "%Y %B %d")
          d <- data.frame( Date=col_date,  IR = col_rate )
          mydata[[n]] =  subset(d, as.Date(from_date) <= Date)
          n = n + 1
      }
   }

   ir_data <- rbindlist(mydata) 
   ir_data <- ir_data[order(Date,decreasing=FALSE),]
   ir_data <- ir_data[complete.cases(ir_data) , ]

   return (ir_data)
}
#get_ir_data_from_FRED(type="30", from_date="2010-01-01")

