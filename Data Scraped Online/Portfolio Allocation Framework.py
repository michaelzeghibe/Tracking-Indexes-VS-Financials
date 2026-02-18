# -*- coding: utf-8 -*-
"""
Created on Tue Feb 17 14:12:38 2026

@author: micha
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Load prices from Excel files
files = {
    'ARKK': 'ARKK_PriceHistory.xlsx',
    'MAGS': 'MAGS_PriceHistory.xlsx',
    'QQQ': 'QQQ_PriceHistory.xlsx',
    'VOO': 'VOO_PriceHistory.xlsx'
}
prices = {}
for etf, file in files.items():
    df = pd.read_excel(file, skiprows=2)
    df['Date'] = pd.to_datetime(df['Date'] - 25569, unit='D')  # Convert Excel serial to datetime
    df.set_index('Date', inplace=True)
    prices[etf] = df['Price'].dropna()

# Align dates to common period
common_dates = prices['MAGS'].index.intersection(prices['ARKK'].index).intersection(prices['QQQ'].index).intersection(prices['VOO'].index)
df_prices = pd.DataFrame({etf: prices[etf].loc[common_dates] for etf in prices})

# Calculate daily returns
returns = df_prices.pct_change().dropna()

# Annualized metrics
mean_returns = returns.mean() * 252
cov_matrix = returns.cov() * 252

# Simulate portfolios
num_portfolios = 10000
results = np.zeros((3, num_portfolios))
for i in range(num_portfolios):
    weights = np.random.random(4)
    weights /= np.sum(weights)
    port_return = np.dot(weights, mean_returns)
    port_vol = np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights)))
    results[0, i] = port_return
    results[1, i] = port_vol
    results[2, i] = (port_return - 0.04) / port_vol  # Sharpe (risk-free 4%)

# Plot
plt.figure(figsize=(10, 6))
plt.scatter(results[1,:], results[0,:], c=results[2,:], cmap='viridis')
plt.colorbar(label='Sharpe Ratio')
plt.xlabel('Volatility')
plt.ylabel('Return')
plt.title('Efficient Frontier for ARKK, MAGS, QQQ, VOO (Real Data)')
plt.show()