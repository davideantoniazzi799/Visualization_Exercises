"""
SCRIPT CONTENT
- horizontal bar chart representing top and bottom 10 countries for largest changes in Happiness Score (2015-2019)
    - Load data
    - Happiness Ranking Difference (from 2015 to 2019)
    - Computing Delta Happiness as (Happiness Score(2019)-Happiness Score(2015))/Happiness Score(2015) * 100
    - Selecting 10 countries with highest and lowest Delta Happiness
    - Printing Happiness Ranking Delta
    - Horizontal Bar Chart
    - Commenting the plot
"""

from pathlib import Path
import pandas as pd
from matplotlib import pyplot as plt

BASE_DIR = Path(__file__).resolve().parent.parent

DATA_DIR = BASE_DIR / 'Data'
IMAGE_DIR = BASE_DIR / 'Images'

# Load data
happ_score_data = pd.read_csv(DATA_DIR/'wh_complete.csv', 
                      usecols=['Country', 'Happiness_Score_2015', 'Happiness_Score_2019', 
                               'Happiness_Rank_2015', 'Happiness_Rank_2019'],
                      sep=',')

# Happiness Ranking Difference (from 2015 to 2019)
happ_score_data['Ranking_Difference'] = happ_score_data['Happiness_Rank_2015'] - happ_score_data['Happiness_Rank_2019']

# Computing Delta Happiness as Happiness Score(2019)-Happiness Score(2015)
happ_score_data['Delta_Happiness'] = round(((happ_score_data['Happiness_Score_2019'] - happ_score_data['Happiness_Score_2015']) / 
                              happ_score_data['Happiness_Score_2015']) * 100, 
                              2)

# Selecting 10 countries with highest and lowest Delta Happiness
top10_df = happ_score_data.nlargest(10, 'Delta_Happiness')
low10_df = happ_score_data.nsmallest(10, 'Delta_Happiness')

dh_df = pd.concat([top10_df, low10_df], ignore_index=True)
#print(dh_df.shape)
dh_df.sort_values(by='Delta_Happiness', ascending=False, inplace=True)

# Printing Happiness Ranking Delta
print(dh_df[['Country','Happiness_Rank_2015', 'Happiness_Rank_2019', 'Ranking_Difference']])

# HORIZONTAL BAR CHART
fig = plt.figure()

# Style
#print(plt.style.available)
plt.style.use('petroff10')

# Variables
countries = dh_df['Country']
delta_happiness = dh_df['Delta_Happiness']

# Colors
colors = [
    'forestgreen' if value > 0 else 'firebrick'
    for value in delta_happiness
]

# Plot
bars = plt.barh(countries, delta_happiness,
                color = colors)

for bar in bars:

    width = bar.get_width()

    # Text position
    if width >= 0:
        x_pos = width + 0.3
        ha = 'left'
    else:
        x_pos = width - 0.3
        ha = 'right'

    plt.text(
        x_pos,
        bar.get_y() + bar.get_height()/2,
        f'{width:.2f}%',
        va='center',
        ha=ha,
        fontsize=9
    )

plt.gca().invert_yaxis()

# Customizing
padding = 12
plt.xlim(
    delta_happiness.min() - padding,
    delta_happiness.max() + padding
)

plt.xlabel('Change in Happiness Score (%)')

fig.suptitle('Largest Changes in Happiness Score (2015-2019)',
             fontsize=16, 
             fontweight='bold')

# 0 Vertical line
plt.axvline(
    x=0,
    color='black',
    linestyle='--',
    linewidth=1
)

# Add caption
source = "Sources: https://www.kaggle.com/datasets/unsdsn/world-happiness/data"
fig.text(0.43, 0.005, 
         source, 
         color='#a2a2a2', 
         fontsize=8, 
         fontfamily="Arial")

# Add authorship
fig.text(0, 0.005, 
         "@DavideAntoniazzi", 
         color='#a2a2a2', 
         fontsize=8, 
         fontfamily="Arial")

# Set facecolor
fig.set_facecolor('white')

plt.tight_layout(rect=[0, 0.03, 1, 0.97])

plt.savefig(
    IMAGE_DIR/'delta_happiness.png',
    dpi=300,
    bbox_inches='tight'
)

plt.show()

"""
COMMENTING THE PLOT:
The plot shows the 20 countries with the largest change (negative or positive) in the happiness score from 2015 to 2019, in percentage.
As we can see, these countries are placed either in the African, South American, Caribbean, South Asian, or South-Eastern Asian Regions.
Benin presents the largest positive change in the happiness score thorugh the period, whereas Venezuala the most negative.
It is noticable that these changes are reflected also in the Happiness Rankings from 2015 to 2019,
where countries that showed these large changes also moved similarly in the rankings through the period, either up or down.
"""