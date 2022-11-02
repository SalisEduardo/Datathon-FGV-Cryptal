from nbformat import write
import pandas as pd


import os

all_files = os.listdir("/home/eduardo/Documentos/Desafio FGV/Portfolio_strats/Heuristic KPIs")    
csv_files_2021 = list(filter(lambda f: f.endswith('2021.csv'), all_files))
csv_files_2022 = list(filter(lambda f: f.endswith('2022.csv'), all_files))


with pd.ExcelWriter('InvInef2021.xlsx') as writer:  
    for i in csv_files_2021:
        df = pd.read_csv(i)
        df.to_excel(writer,sheet_name=i.replace(".csv",""))

with pd.ExcelWriter('InvInef2022.xlsx') as writer:  
    for i in csv_files_2022:
        df = pd.read_csv(i)
        df.to_excel(writer,sheet_name=i.replace(".csv",""))

