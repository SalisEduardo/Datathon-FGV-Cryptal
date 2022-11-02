from nbformat import write
import pandas as pd
import os

all_files = os.listdir("/home/eduardo/Documentos/Desafio FGV/Portfolio_strats/Pesos")    
csv_files = list(filter(lambda f: f.endswith('.csv'), all_files))


with pd.ExcelWriter('pesos.xlsx') as writer:  
    for i in csv_files:
        df = pd.read_csv(i)
        df.to_excel(writer,sheet_name=i.replace(".csv",""))

