"""
SCRIPT CONTENT
The script is divided in sections, which can be expaned by clicking the down arrow at the left of the #region comment
The script presentes the following sections:
- Libraries
- Data File Path
- Uploading Data
- Data Cleaning
- Scatter plot magnitude-depth earthquakes

This script focuses on creating the fourth visualization. 
The graph represents the relationship between magnitude and focal depth (in km) of seismic events recorded in the period 1950-2025.
The visualization considers only earthquake with a reported magnitude equal or higher of 4.5. 
The points are colored by class of focal depth.
The plot is exported in PDF format in the 'Image' folder, named as '4_Magnitude_Depth_Scatter.pdf'
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

# region Data Cleaning
#Deleting earthquakes without value for depth
eq_cleaned = earthquakes[(earthquakes['depth'].notna()) & 
                         (earthquakes['depth'] > 0)].copy()

eq_map = eq_cleaned.dropna(subset=["longitude", "latitude", "mag", "depth"]).copy()

#Keep only valid geographic coordinates and magnitudes
eq_map = eq_map[eq_map["longitude"].between(-180, 180)
                & eq_map["latitude"].between(-90, 90)
                & (eq_map["mag"] >= 4.5)
                & (eq_map["depth"] >= 0)]
# endregion

# region Scatter plot magnitude-depth earthquakes
#Binning
#Defining the bins for the depth (in km) (0-5.0 | 5.0-15.0 | 15.0-30.0 | 30.0-60.0 | 60.0-120.0 | 120.0-240.0 | 240.0-480.0 | 480.0+)
bins = [0, 5.0, 15.0, 30.0, 60.0, 120, 240, 480, np.inf]
labels = ["0 - 5.0", 
          ">5.0 - 15.0", 
          ">15.0 - 30.0", 
          ">30.0 - 60.0", 
          ">60.0 - 120.0",
          ">120.0 - 240.0",
          ">240.0 - 480.0",
          ">480.0+"]

#Assigning the magnitude class to each earthqake
eq_map["depth_class"] = pd.cut(eq_map["depth"], 
                             bins=bins, 
                             labels=labels, 
                             include_lowest=True,
                             right=True)

#Setting the style
sns.set_theme(style="white")
plt.rcParams["font.family"] = "sans-serif"

fig, ax = plt.subplots(figsize=(11, 6.5), dpi=300)

#Defining one color for each depth class
palette = sns.color_palette("inferno", n_colors=len(labels))

#Plotting each depth class separately
for label, color in zip(labels, palette):
    subset = eq_map[eq_map["depth_class"] == label]

    ax.scatter(subset["depth"],
               subset["mag"],
               s=5,
               color=color,
               edgecolors="none",
               alpha=0.7,
               label=label,
               rasterized=True)

#Axis formatting
ax.set_xlabel("Focal depth (km)", 
              fontsize=11, 
              labelpad=10)
ax.set_ylabel("Magnitude", 
              fontsize=11, 
              labelpad=10)

ax.set_xlim(left=0)
ax.set_ylim(bottom=4.5)

ax.yaxis.grid(True,
              linestyle="--",
              linewidth=0.5,
              color="#aab4bd",
              alpha=0.5)
ax.xaxis.grid(False)
sns.despine(ax=ax)

#Depth class legend
legend = ax.legend(title="Focal depth (km)",
                   loc="upper right",
                   bbox_to_anchor=(1, 1),
                   frameon=True,
                   framealpha=0.97,
                   facecolor="white",
                   edgecolor="#b8c2cc",
                   fontsize=9,
                   title_fontsize=10,
                   labelspacing=0.7,
                   handletextpad=0.7,
                   borderpad=0.9,
                   markerscale=2.0)
legend.get_title().set_fontweight("bold")

#Title
fig.suptitle("Global Earthquakes: Magnitude vs Focal Depth",
             x=0.08,
             y=0.975,
             fontsize=19,
             fontweight="bold",
             color="#202830",
             ha="left")
fig.text(0.08, 0.915,
         "Recorded events with magnitude ≥ 4.5 | 1950-2025",
         ha="left",
         va="center",
         fontsize=13,
         color="#5c6770")

#Source and data notes
fig.text(0.08, 0.035,
         "Focal depth: depth below Earth's surface where the earthquake originates.",
         ha="left",
         va="bottom",
         fontsize=8.5,
         color="#68737d",
         style="italic")
fig.text(0.97, 0.025,
         "Source: USGS Earthquake Catalog\n"
         f"Events plotted: {len(eq_map):,} | Events with missing depth excluded\n"
         "Author: Antoniazzi Davide",
         ha="right",
         va="bottom",
         fontsize=8,
         color="#68737d",
         style="italic")

#Layout
plt.tight_layout(rect=[0.04, 0.08, 0.98, 0.87])

#Export
IMAGE_DIR = BASE_DIR / 'Images'
#IMAGE_DIR.mkdir(parents=True, exist_ok=True)
path_pdf = IMAGE_DIR / "4_Magnitude_Depth_Scatter.pdf"
fig.savefig(path_pdf,
            bbox_inches="tight",
            facecolor="white")

plt.show()
# endregion