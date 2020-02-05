import pandas as pd
from bs4 import BeautifulSoup
import requests


def parse_embs_data_py(source="FNM"): 
    """ get prepayment data (html) from eMBS websites and return clean data in formatted dataframe.

    Args:
      source: either FNM or FHL,
            when FNM: the source data is from https://www.embs.com/public/html/FNM_eMBSFlashFull.htm
            when FHL: the source data is from https://www.embs.com/public/html/FHL_eMBSFlashFull.htm

    Returns:
       parsed prepayment data in dataframe format. Ready to be used.
    """

    fnma_URL = "https://www.embs.com/public/html/FNM_eMBSFlashFull.htm"
    fred_URL = "https://www.embs.com/public/html/FHL_eMBSFlashFull.htm"
   
    embs_url = ""
    if source == "FNM":
       embs_url = fnma_URL
    elif source == "FHL":
       embs_url = fred_URL
       
    if embs_url == "":
        return None
        
    page = requests.get(embs_url)
    soup = BeautifulSoup(page.content, 'html.parser')
    src = soup.find_all('p')
    update_datetime = ""
    for paragraph in soup.find_all('p'):
        update_datetime = paragraph.string
        break
    
    print(update_datetime)
    src = soup.find_all('table')[2]
    colnames = []
    rows = []
    trs = src.find_all('tr')
    category = ""
    for tr in trs:
        the_th = [td.get_text(strip=True) for td in tr.find_all('th')]
        if len(the_th) > 1 and len(colnames) ==0:
            colnames = [ele for ele in the_th if ele]
        elif len(the_th) == 1:
            if the_th[0] != "":
                category = the_th[0]
        else:
            the_tr = [td.get_text(strip=True) for td in tr.find_all('td')]
            if len(the_tr) > 1:
                rows.append([category] + [ele for ele in the_tr if ele])  # Get rid of empty values
    df = pd.DataFrame(rows, columns=["Category"] + colnames)
    df.insert(0, "Agency", source)
    TermMapping = pd.DataFrame.from_dict(
        {"Category": ["FNM30", "FHLG30", "Freddie Mac 30yr",
                      "FNM20", "FHLG20", "Freddie Mac 20yr",
                      "FNM15", "FHLG15", "Freddie Mac 15yr",
                      "FNM10", "FHLG10", "Freddie Mac 10yr", 
                      "LIBOR"],
         "Term": ["30", "30", "30", "20", "20", "20", "15", "15", "15", '10', "10", "10", None]
         })
    df = df.merge(TermMapping, on="Category", how="inner")
    df.Term = df.Term.fillna(df.Coupon)
    cat_cols = ['Agency', 'Category', "Term", 'Coupon', 'Prodn Yr']
    num_cols = df.columns.drop(cat_cols)
    def str_num_convert(x):
        xx = x.astype(str).str.replace(',', '')
        xx = xx.astype(str).str.replace('%', '')
        return pd.to_numeric(xx, errors='coerce')

    df[num_cols] = df[num_cols].apply(str_num_convert)
    df = df[cat_cols + num_cols.to_list()]
    df.loc[ (df.Category == df.Coupon) & (df['Prodn Yr'] == "ALL"), ["Coupon"]] = "ALL"

    return df, update_datetime

    

def TEST_parse_embs_data_py():
   #fnma_URL = "https://www.embs.com/public/html/FNM_eMBSFlash.htm"
   #fred_URL = "https://www.embs.com/public/html/FHL_eMBSFlash.htm"
   FNMA_data, update_datetime_fnma = parse_embs_data_py( "FNMA")
   FRED_data, update_datetime_fred = parse_embs_data_py( "FRED")
   print(update_datetime_fnma)
   print(FNMA_data )
   print(FRED_data)
