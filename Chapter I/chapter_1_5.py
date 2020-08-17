import pandas

from pandas_datareader import DataReader

from datetime import datetime

AMZN = DataReader("AMZN", "yahoo", datetime(2000,1,1), datetime(2020,7,1))

Prices = AMZN["Adj Close"]

import matplotlib.pyplot as plt

plt.plot(Prices)
plt.show()

