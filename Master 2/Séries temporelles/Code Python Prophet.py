# Packages
import pandas as pd
from prophet import Prophet
from sklearn.metrics import mean_squared_error, mean_absolute_error
import numpy as np
from datetime import datetime

#Data processing
df = pd.read_csv('valeurs_mensuelles.csv',sep=";")
df.describe()
df.columns=['ds','y']
df['ds'] = pd.to_datetime(df['ds'])

model = Prophet(interval_width=0.95)
training_run = model.fit(df)

future=model.make_future_dataframe(periods=25,freq='M')

future

forecast = model.predict(future)

forecast.tail()

plot1 = model.plot(forecast)

plot2 = model.plot_components(forecast)


# Restreindre la plage de dates pour les observations réelles
actual_data = df[df['ds'] >= forecast['ds'].min()]
actual_data = actual_data[actual_data['ds'] <= forecast['ds'].max()]

# Fusionner les prévisions et les observations réelles sur la colonne 'ds'
merged_data = pd.merge(actual_data, forecast, on='ds', how='inner')

# Calculer le RMSE
rmse = round(np.sqrt(mean_squared_error(merged_data['y'], merged_data['yhat'])),1)
mae = round(mean_absolute_error(merged_data['y'], merged_data['yhat']),1)

print(f"RMSE: {rmse}")
print(f"MAE: {mae}")


import pandas as pd
from prophet import Prophet
from sklearn.metrics import mean_squared_error, mean_absolute_error
import numpy as np
from datetime import datetime
import matplotlib.pyplot as plt

# Data processing
df = pd.read_csv('valeurs_mensuelles.csv', sep=";")
df.describe()
df.columns = ['ds', 'y']
df['ds'] = pd.to_datetime(df['ds'])

model = Prophet(interval_width=0.95)
training_run = model.fit(df)

future = model.make_future_dataframe(periods=25, freq='M')
forecast = model.predict(future)

# Plot
fig, ax = plt.subplots(figsize=(12, 6))

# Plot the forecast
model.plot(forecast, ax=ax)

# Plot the original series
ax.plot(df['ds'], df['y'], color='red', label='Original Series')

# Set labels and title
ax.set_xlabel('Date')
ax.set_ylabel('Number of created companies')

# Add legend
ax.legend()

# Show the plot
plt.show()