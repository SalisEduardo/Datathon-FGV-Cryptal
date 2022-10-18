

from matplotlib.pyplot import get
import pandas as pd 
import time
from pycoingecko import CoinGeckoAPI # importa o SDK
from tqdm import tqdm # barra de progresso
from datetime import datetime, timezone

cg = CoinGeckoAPI() # cria um objeto SDK

def timestamp_miliseconds(dt,dt_format="%Y-%m-%d"):
    dt = datetime.strptime(dt, dt_format)

    dt_timestamp = dt.replace(tzinfo=timezone.utc).timestamp() 
    dt_timestamp_string = str(int(dt_timestamp))
    
    return(dt_timestamp_string)

def get_coin_prices(coin_id, coin_symbol, coin_name,start_date,end_date):

  start_date_timestamp = timestamp_miliseconds(start_date) 
  end_date_timestamp = timestamp_miliseconds(end_date) 
  # função do SDK que consulta esses dados
  coin_data = cg.get_coin_market_chart_range_by_id(
      id = coin_id,
      symbol = coin_symbol,
      name = coin_name,
      vs_currency = 'usd',
      from_timestamp = start_date_timestamp,
      to_timestamp = end_date_timestamp
  )
  df = pd.DataFrame(coin_data['prices'])
  df.columns = ['date',coin_symbol.upper()]

  df['date'] = df['date'].apply(lambda i: datetime.fromtimestamp(i/1000.0).strftime("%Y-%m-%d"))
  
  return(df)




list_symbol = [x.lower() for x in ["BNB","ADA","ETH","LTC","TRX","BTC","XRP","DOGE"]]
list_name = ['BNB','Cardano','Ethereum','Litecoin','TRON','Bitcoin','XRP','Dogecoin']
list_id = ['binancecoin','cardano','ethereum','litecoin','tron','bitcoin','ripple','dogecoin']


list_dfs = []
for symbol,name,id in zip(list_symbol,list_name,list_id):
    crypto_price =get_coin_prices(id,symbol,name,start_date='2018-01-01',end_date='2022-09-30')
    print(crypto_price.shape)
    crypto_price.to_excel(name.upper() + ".xlsx")
    list_dfs.append(crypto_price)

from functools import reduce
df_merged = reduce(lambda  left,right: pd.merge(left,right,on=['date'],
                                            how='outer'),list_dfs)


df_merged.to_csv("Crypto_prices.csv")

print(df_merged.shape)




