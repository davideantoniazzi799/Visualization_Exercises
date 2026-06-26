"""
CORRELATION HEATMAP CONTENT:
- Load the data
- Selecting only year 2019
- Calculate the correlation matrix
- Plotting correlation heatmap
- Commenting the plot
"""
from pathlib import Path
import seaborn as sns
import pandas as pd
from matplotlib import pyplot as plt

BASE_DIR = Path(__file__).resolve().parent.parent

DATA_DIR = BASE_DIR / 'Data'

# Load the data
wh_data = pd.read_csv(DATA_DIR/'wh_complete.csv')
#print(wh_data.columns)

# Selecting only year 2019
wh_data_2019 = wh_data.filter(regex='_2019$')
# print(wh_data_2019.columns)

# Calculate the correlation matrix
corr = wh_data_2019.select_dtypes('number').corr()

# Plotting correlation heatmap
plt.figure(figsize=(10, 8))

sns.heatmap(corr, 
            cmap='coolwarm', 
            annot=True,
            fmt='.2f',
            vmin=-1,
            vmax=1,
            linewidths=0.5)

plt.title(
    'Correlation Matrix - World Happiness Report 2019',
    fontsize=14,
    fontweight='bold'
)

plt.xticks(rotation=45, ha='right')
plt.yticks(rotation=0)

plt.tight_layout()

#plt.savefig(
#    'happiness_corr_map.png',
#    dpi=300
#)

plt.show()

"""
COMMENTING THE PLOT
The correlation heat map shows, as expected, the positive correlation that the Happiness Score as with the majority of the variables.
Specifically, GDP per capita, Life Expectancy, and Family showed the highest values of correlation to each other and to the Happiness Score.
Government Corruption and Freedom showed lower positive correlation with the Happiness variables.
Interestingly, Generosity did not show any correlation with the Happiness Score
"""