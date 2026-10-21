"""
SCRIPT CONTENT
The script is divided in sections, which can be expaned by clicking the down arrow at the left of the #region comment
The script presentes the following sections:
- Libraries
- Data File Path
- Uploading Data
- Data Processing (Binning and Grouping)
- Making the Subplots (2x2 Line Charts)

This script focuses on creating the second visualization. The graph represents the frequency of seismic events divided by magnitude classes recorded in the period 1950-2025.
The visualization considers only earthquake with a reported magnitude equal or higher of 4.5. 
It is important to specify that the reported number of earthquakes with a light and moderate magnitude (up to 5.5 in this analysis)
might show less observations in the early part of the considered period due to less advanced recording instruments.
Additionally, the plot shows for each magnitude class in which year the highest number of events was reported.
The plot is exported in PDF format in the 'Image' folder, named as '2_Earthquakes_Trends_By_Magnitude_1950_2025.pdf'
"""

# region Libraries
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
# endregion

# region Data File Path
BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / 'Data'
# endregion

# region Uploading data
earthquakes = pd.read_csv(DATA_DIR / 'Earthquakes_1950_2025.csv', sep=",")
# endregion

# region Data Processing (Binning and Grouping)
earthquakes["year"] = pd.to_datetime(earthquakes["date"]).dt.year

#Defining the bins (4.5-5.0 | >5.0-5.5 | >5.5-6.0 | >6.0-7.0 | >7.0+)
bins = [4.5, 5.0, 5.5, 6.0, 7.0, np.inf]
labels = ["4.5 - 5.0 (Light)", 
          ">5.0 - 5.5 (Moderate)", 
          ">5.5 - 6.0 (Strong)", 
          ">6.0 - 7.0 (Very strong)", 
          ">7.0+ (Major)"]

#Assigning the magnitude class to each earthqake
earthquakes["mag_class"] = pd.cut(earthquakes["mag"], 
                                  bins=bins, 
                                  labels=labels, 
                                  include_lowest=True)

#Grid Year x Class to avoid gaps (years with 0 events for example)
all_years = range(1950, 2026)
full_grid = pd.MultiIndex.from_product([all_years, labels], 
                                       names=["year", "mag_class"]).to_frame().reset_index(drop=True)

#Yearly frequency for seismic class
counts = earthquakes.groupby(["year", "mag_class"], 
                             observed=False).size().reset_index(name="count")

#Assigning 0 to possible gaps
yearly_trends = pd.merge(full_grid, counts, 
                         on=["year", "mag_class"], 
                         how="left").fillna(0)
# endregion

# region Making the Subplots (2x2 Line Charts)
sns.set_theme(style="ticks")
plt.rcParams["font.family"] = "sans-serif"

#Making 2x2 figure
fig, axes = plt.subplots(5, 1, figsize=(12, 14), sharex=True)

#Color palette transitioning from blue to dark red (reflecting severity)
colors = ["#68809a", 
          "#4a7c59",
          "#e9c46a", 
          "#f4a261", 
          "#e76f51"]

for i, label in enumerate(labels):
    ax = axes[i]
    sub_data = yearly_trends[yearly_trends["mag_class"] == label]
    #Plotting the trend line
    ax.plot(sub_data["year"], 
            sub_data["count"], 
            color=colors[i], 
            linewidth=2)
    ax.fill_between(sub_data["year"], 
                    sub_data["count"], 
                    color=colors[i], 
                    alpha=0.12)
    #Highest earthquake count of that class
    max_row = sub_data.loc[sub_data["count"].idxmax()]
    ax.scatter(max_row["year"], 
               max_row["count"], 
               color=colors[i], 
               s=45, 
               zorder=5,
               edgecolor="white", 
               linewidth=0.8)
    #Dynaminc placement of the annotation
    y_offset = sub_data["count"].max() * 0.08 if sub_data["count"].max() > 0 else 5
    ax.text(max_row["year"], 
            max_row["count"] + y_offset, f"Peak: {int(max_row['count'])} ({int(max_row['year'])})",
            fontsize=9, 
            fontweight="bold", 
            color="#2a2a2a", 
            ha="center")
    #Graphic formatting of the individual panels
    ax.set_title(label, 
                 fontsize=12, 
                 fontweight="bold", 
                 loc="left", 
                 pad=4, 
                 color="#2c3e50")
    ax.yaxis.grid(True, 
                  linestyle="--", 
                  alpha=0.3, 
                  color="gray")
    ax.xaxis.grid(False)
    ax.set_ylim(0, sub_data["count"].max() * 1.25)
    ax.tick_params(axis="both", 
                   labelsize=10,
                   labelbottom=True if i == 4 else False)
    sns.despine(ax=ax, 
                trim=True)

#Custominzing X axis ticks in the bottom panel
axes[-1].set_xlim(1948, 2027)
axes[-1].set_xticks(range(1950, 2026, 10))
axes[-1].tick_params(axis="x", labelbottom=True)

#Customizing main title
fig.suptitle("Temporal Evolution of Global Seismic Frequency\n"
             "Annual comparison of the number of earthquakes, broken down by seismic class | 1950-2025",
             fontsize=17, 
             fontweight="bold", 
             color="#1a1a1a", 
             x=0.04, 
             ha="left", 
             va="top")

#Axis titles
fig.text(0.5, 0.05, 
         "Year", 
         ha="center", 
         fontsize=13, 
         fontweight="bold", 
         color="#1a1a1a")
fig.text(0.02, 0.5, 
         "Annual Number of Recorded Events", 
         va="center", 
         rotation="vertical", 
         fontsize=13, 
         fontweight="bold", 
         color="#1a1a1a")

plt.tight_layout(rect=[0.04, 0.08, 0.98, 0.94])
plt.subplots_adjust(hspace=0.3)

#Caption
caption_text = ("Data Source: U.S. Geological Survey (USGS) Earthquake Catalog\n"
                "Notes: Recorded events with magnitude ≥ 4.5 | 'Peak' indicates the specific year that registered the highest number of earthquake observations within that magnitude class.\n"
                "Author: Davide Antoniazzi")
plt.figtext(0.98, 0.01, 
            caption_text, 
            fontsize=8.5, 
            color="dimgray", 
            ha="right", 
            va="bottom", 
            style="italic")

#Exporting
IMAGE_DIR = BASE_DIR / 'Images'
#IMAGE_DIR.mkdir(parents=True, exist_ok=True)
path_pdf = IMAGE_DIR / "2_Earthquakes_Trends_By_Magnitude_1950_2025.pdf"
plt.savefig(path_pdf, 
            bbox_inches="tight", 
            dpi=300)

plt.show()
# endregion