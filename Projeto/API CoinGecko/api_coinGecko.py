#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 4 17:54:14 2023

@author: Carlos Eduardo

Este programa coleta dados históricos de preços de fechamento para 
as 15 principais criptomoedas por capitalização de mercado no período desejado. 
Em seguida, os dados são salvos em um arquivo CSV.

Observações:

1 - É necessário definir o conjunto das 15 maiores criptomoedas através de seu
id.

2 - É necessário definir o período inicial e final de extração.

"""

import requests
import csv
from datetime import datetime
import time

# Define as 15 maiores criptos do período (SÍMBOLO)
top_cryptos = [
    "BTC", "ETH", "XRP", "USDT", "BCH", 
    "LTC", "EOS", "BNB", "BSV", "XMR", 
    "XLM", "TRX", "ADA", "XTZ", "LEO"
]

def get_historical_data(crypto_id, start_date, end_date):
    start_timestamp = int(start_date.timestamp())
    end_timestamp = int(end_date.timestamp())
    
    url = f"https://api.coingecko.com/api/v3/coins/{crypto_id}/market_chart/range"
    params = {
        "vs_currency": "brl",
        "from": start_timestamp,
        "to": end_timestamp
    }

    response = requests.get(url, params=params)
    
    data = response.json()
    
    print(f"Salvando dados para {crypto_id} de {start_date.strftime('%Y-%m-%d')} até {end_date.strftime('%Y-%m-%d')}")

    return data.get("prices", [])

def fetch_crypto_data_batch(crypto_batch, delay):
    historical_data = {}
    all_dates = set()

    for symbol in crypto_batch:
        
        # Define as 15 maiores criptos do período (SÍMBOLO e ID)
        symbol_to_id = {
            "BTC": "bitcoin", "ETH": "ethereum", "XRP": "ripple", "USDT": "tether", "BCH": "bitcoin-cash", 
            "LTC": "litecoin", "EOS": "eos", "BNB": "binancecoin", "BSV": "bitcoin-cash-sv", "XMR": "monero",
            "XLM": "stellar", "TRX": "tron", "ADA": "cardano", "XTZ": "tezos", "LEO": "leo-token"
        }

        crypto_id = symbol_to_id.get(symbol)
        if not crypto_id:
            print(f"Erro: símbolo {symbol} não encontrado.")
            continue
        
        symbol = symbol.upper()
        
        # Define o intervalo de tempo em cada experimento
        start_date = datetime(2020, 1, 1)
        end_date = datetime(2021, 1, 1)

        historical_data[symbol] = get_historical_data(crypto_id, start_date, end_date)
        
        # Adicionando um atraso antes da próxima solicitação
        time.sleep(delay)
        
        if historical_data[symbol]:
            for entry in historical_data[symbol]:
                all_dates.add(datetime.utcfromtimestamp(entry[0] // 1000).strftime('%Y-%m-%d'))
    
    return historical_data, all_dates

def main():
    
    # Divide as criptomoedas em três lotes de 5 criptomoedas cada
    batch1 = top_cryptos[0:5]
    batch2 = top_cryptos[5:10]
    batch3 = top_cryptos[10:15]

    # Coleta os dados para o primeiro lote
    batch1_data, batch1_dates = fetch_crypto_data_batch(batch1, delay=4)
    
    time.sleep(10)  
    
    # Coleta os dados para o segundo lote
    batch2_data, batch2_dates = fetch_crypto_data_batch(batch2, delay=4)
    
    time.sleep(10) 

    # Coleta os dados para o terceiro lote
    batch3_data, batch3_dates = fetch_crypto_data_batch(batch3, delay=4)

    all_dates = sorted(list(batch1_dates | batch2_dates | batch3_dates))
    
    with open("dados/dataset2021.csv", "w", newline="") as csvfile:
        csvwriter = csv.writer(csvfile)
        
        # Cabeçalho
        header = ["Data"] + list(batch1_data.keys()) + list(batch2_data.keys()) + list(batch3_data.keys())
        csvwriter.writerow(header)

        for date in all_dates:
            row = [date]
            for symbol in batch1_data.keys():
                if batch1_data[symbol]:
                    for entry in batch1_data[symbol]:
                        if date == datetime.utcfromtimestamp(entry[0] // 1000).strftime('%Y-%m-%d'):
                            row.append(entry[1])
                            break
                    else:
                        row.append("")
                else:
                    row.append("")
            
            for symbol in batch2_data.keys():
                if batch2_data[symbol]:
                    for entry in batch2_data[symbol]:
                        if date == datetime.utcfromtimestamp(entry[0] // 1000).strftime('%Y-%m-%d'):
                            row.append(entry[1])
                            break
                    else:
                        row.append("")
                else:
                    row.append("")
            
            for symbol in batch3_data.keys():
                if batch3_data[symbol]:
                    for entry in batch3_data[symbol]:
                        if date == datetime.utcfromtimestamp(entry[0] // 1000).strftime('%Y-%m-%d'):
                            row.append(entry[1])
                            break
                    else:
                        row.append("")
                else:
                    row.append("")
            
            csvwriter.writerow(row)

    print("Dados históricos salvos no arquivo 'dados/dataset2020.csv'.")

if __name__ == "__main__":
    main()