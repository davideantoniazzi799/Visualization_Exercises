"""
SCRIPT CONTENT
- scatter plot GDP per Capita vs. Happiness Score
    - Color palette
    - Region-color mapping
    - Variables by year
    - Figure and shared axes
    - Scatter plots
    - Commenting the plot
"""

from pathlib import Path
import pandas as pd
from matplotlib import pyplot as plt

BASE_DIR = Path(__file__).resolve().parent.parent

DATA_DIR = BASE_DIR / 'Data'

#Load data
wh_data = pd.read_csv(DATA_DIR/'wh_complete.csv')

#Style
plt.style.use('petroff10')

#Color palette
hex_10_palette = [
    "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"
]

# Region-color mapping
regions = wh_data['Region'].unique()

color_map = {
    region: hex_10_palette[i]
    for i, region in enumerate(regions)
}

# Variables by year
variables_years = [
    ('GDP_capita_2015', 'Happiness_Score_2015', '2015'),
    ('GDP_capita_2016', 'Happiness_Score_2016', '2016'),
    ('GDP_capita_2017', 'Happiness_Score_2017', '2017'),
    ('GDP_capita_2018', 'Happiness_Score_2018', '2018'),
    ('GDP_capita_2019', 'Happiness_Score_2019', '2019')
]

# Figure and shared axes
fig, axes = plt.subplots(
    nrows=2,
    ncols=3,
    figsize=(16, 10),
    sharex=True,
    sharey=True
)

axes = axes.flatten()

# Scatter plots
for ax, (gdp_col, happy_col, year) in zip(axes, variables_years):

    for region, sub_df in wh_data.groupby('Region'):

        ax.scatter(
            sub_df[gdp_col],
            sub_df[happy_col],
            color=color_map[region],
            alpha=0.5,
            edgecolors='black',
            linewidths=0.3,
            s=40
        )

    # Correlation coefficient
    corr = wh_data[[gdp_col, happy_col]].corr().iloc[0, 1]

    ax.set_title(
        year,
        fontsize=12,
        fontweight='bold'
    )

    ax.text(
        0.05,
        0.92,
        f"r = {corr:.2f}",
        transform=ax.transAxes,
        fontsize=10
    )

# Remove empty subplot
fig.delaxes(axes[5])

# Global title
fig.suptitle(
    'GDP per Capita vs Happiness Score by Region (2015-2019)',
    fontsize=18,
    fontweight='bold'
)

# Shared axis labels
fig.supxlabel(
    'GDP per Capita',
    fontsize=12
)

fig.supylabel(
    'Happiness Score',
    fontsize=12
)

# Global legend
handles = [
    plt.Line2D(
        [0],
        [0],
        marker='o',
        color='w',
        markerfacecolor=color_map[region],
        markeredgecolor='black',
        markersize=8,
        label=region
    )
    for region in regions
]

fig.legend(
    handles=handles,
    title='Regions',
    loc='lower right',
    bbox_to_anchor=(0.98, 0.02),
    ncol=2,
    fontsize='small',
    frameon=True
)

plt.tight_layout(rect=[0.03, 0.05, 1, 0.95])

#plt.savefig(
#    'scatter_plot.png',
#    dpi=300
#)

plt.show()

"""
COMMENTING THE PLOT:
It is clear from each plot that there is a positive correlation between the Happiness Score and the GDP per capita.
It is possible to notice some differences among the regions.
Generally, Western Europe, North America, and Australia and New Zealand have the highest values for both variables along the period.
Some countries from the Middle East and Northern African regions also reported a high value of both variables, while some others reported less.
The Sub-Saharan Africa region showed the lowest records for both variables.
"""