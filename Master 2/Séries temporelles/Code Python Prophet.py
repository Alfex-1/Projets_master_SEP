# Packages
import pandas as pd
from prophet import Prophet
from sklearn.metrics import mean_squared_error, mean_absolute_error
import numpy as np
#from datetime import datetime
import matplotlib.pyplot as plt

# Data processing
df = pd.read_csv('valeurs_mensuelles.csv',sep=";")
df.describe()
df.columns=['ds','y']
df['ds'] = pd.to_datetime(df['ds'])

# Entraîner le modèle
model = Prophet(interval_width=0.95)
training_run = model.fit(df)

future=model.make_future_dataframe(periods=25,freq='M')

forecast = model.predict(future)

# Graphiques de tendance et de saisonnalité annuelle
plot1 = model.plot(forecast)

# Fusionner les prévisions et les observations réelles sur la colonne 'ds'
merged_data = pd.merge(df, forecast, on='ds', how='inner')

# Calculer le RMSE
rmse = round(np.sqrt(mean_squared_error(merged_data['y'], merged_data['yhat'])),1)
mae = round(mean_absolute_error(merged_data['y'], merged_data['yhat']),1)

print(f"RMSE: {rmse}")
print(f"MAE: {mae}")


# Visualisation des prédictions
fig, ax = plt.subplots(figsize=(12, 6))

# Prévisions
model.plot(forecast, ax=ax)
# Série originale
ax.plot(df['ds'], df['y'], color='red', label='Original Series')

ax.set_xlabel('Date')
ax.set_ylabel('Number of created companies')
ax.legend()
plt.show()