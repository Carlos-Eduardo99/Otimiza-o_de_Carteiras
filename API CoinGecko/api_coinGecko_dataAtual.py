#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 16 12:34:14 2023

@author: Carlos Eduardo

Este programa coleta dados históricos de preços de fechamento para 
as 15 principais criptomoedas por capitalização de mercado na data atual, 
abrangendo um período de 1 ano. Em seguida, os dados são salvos em um arquivo
CSV.

"""

import requests
import csv
from datetime import datetime, timedelta
import time

# Função para obter o histórico de dados
def get_historical_data(crypto_id, days):
    end_date = datetime.now()
    start_date = end_date - timedelta(days=days)
    end_timestamp = int(end_date.timestamp())
    start_timestamp = int(start_date.timestamp())
    
    url = f"https://api.coingecko.com/api/v3/coins/{crypto_id}/market_chart/range"
    params = {
        "vs_currency": "brl",
        "from": start_timestamp,
        "to": end_timestamp
    }

    response = requests.get(url, params=params)
    data = response.json()
    
    print(f"Salvando dados: {crypto_id}")

    return data.get("prices", [])

def fetch_crypto_data_batch(crypto_batch, delay):
    historical_data = {}
    all_dates = set()

    for crypto in crypto_batch:
        crypto_id = crypto["id"]
        symbol = crypto["symbol"].upper()
        historical_data[symbol] = get_historical_data(crypto_id, days=365) # define a quantidade de dias
        
        # Adicionando um atraso antes da próxima solicitação
        time.sleep(delay)
        
        if historical_data[symbol]:
            for entry in historical_data[symbol]:
                all_dates.add(datetime.utcfromtimestamp(entry[0] // 1000).strftime('%Y-%m-%d'))
    
    return historical_data, all_dates

def get_top_cryptos():
    url = "https://api.coingecko.com/api/v3/coins/markets"
    params = {
        "vs_currency": "brl",  # Valor em reais brasileiros
        "order": "market_cap_desc",  # Ordenar por capitalização de mercado descendente
        "per_page": 15,  # Limitar a 15 criptomoedas
        "page": 1  # Página 1
    }

    response = requests.get(url, params=params)
    data = response.json()
    
    return data

def main():
    top_cryptos = get_top_cryptos()

    # Divide as criptomoedas em três lotes de 5 criptomoeda
    batch1 = top_cryptos[0:5]
    batch2 = top_cryptos[5:10]
    batch3 = top_cryptos[10:15]

    # Coleta dados para o primeiro lote
    batch1_data, batch1_dates = fetch_crypto_data_batch(batch1, delay=4)
    
    time.sleep(10)

    # Coleta dados para o segundo lote
    batch2_data, batch2_dates = fetch_crypto_data_batch(batch2, delay=4)
    
    time.sleep(10)  

    # Coleta dados para o terceiro lote
    batch3_data, batch3_dates = fetch_crypto_data_batch(batch3, delay=4)

    all_dates = sorted(list(batch1_dates | batch2_dates | batch3_dates))
    
    with open("dados/dataset.csv", "w", newline="") as csvfile:
        csvwriter = csv.writer(csvfile)
        
        # Cabeçalho
        header = ["Data"] + list(batch1_data.keys()) + list(batch2_data.keys()) + list(batch3_data.keys())
        csvwriter.writerow(header)

        # Escrevendo os dados de preço para cada data
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

    print("Dados históricos de 1 ano salvos no arquivo 'dados/dataset.csv'.")

if __name__ == "__main__":
    main()