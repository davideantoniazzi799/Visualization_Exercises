"""
SCRIPT CONTENT
The script is divided in sections, which can be expaned by clicking the down arrow at the left of the #region comment
The script presentes the following sections:
- Libraries
- Data File Path
- Uploading Data
- Filtering year 2000-2025
- Making the plot

This script focuses on creating the first visualization. The graph represents the frequency of several magnitude levels recorded for earthquakes happened in the period 2000-2025.
The applied data is a filtered result of the complete dataset exported in the first script.
The visualization considers only earthquake with a reported magnitude equal or higher of 4.5.
The plot is exported in PDF format in the 'Image' folder, named as '1_Earthquakes_Frequency_Distribution.pdf'
"""

# region Libraries
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.lines import Line2D
import seaborn as sns
# endregion

# region Data File Path
BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / 'Data'
# endregion

# region Uploading data
earthquakes = pd.read_csv(DATA_DIR / 'Earthquakes_1950_2025.csv', sep=",")
# endregion

# region Filtering year 2000-2025
earthquakes = earthquakes[pd.to_datetime(earthquakes["data"]).dt.year.between(2000, 2025)]
# endregion

# region Making the plot
#Setting the style for the plot
sns.set_theme(style="ticks")
plt.rcParams["font.family"] = "sans-serif"

fig, ax = plt.subplots(figsize=(11, 6.5))

#Making the histogram:
bin_breaks = np.arange(4.5, 9.6, 0.1)
#bins = number of bars
#kde = True adds a density line
sns.histplot(data=earthquakes,
             x="mag",
             bins=bin_breaks,
             kde=True,
             color="#5c768d",
             edgecolor="white",
             linewidth=0.6,
             alpha=0.85,
             ax=ax)

#Customizing KDE line
if len(ax.lines) > 0:
    ax.lines[0].set_color("crimson")
    ax.lines[0].set_linewidth(2.5)

#Axis formatting
ax.set_xlim(4.4, 9.5)
ax.set_xticks([4.5, 5, 6, 7, 8, 9])
ax.set_xticklabels(["4.5", "5", "6", "7", "8", "9"])

ax.yaxis.grid(True, linestyle="--", alpha=0.4, color="gray")
ax.xaxis.grid(False)
sns.despine(trim=True)

#Adding a legend to explain the histogram and KDE line
legend_elements = [Patch(facecolor="#5c768d", 
                         edgecolor="white",
                         alpha=0.85, 
                         label="Earthquake frequency"),
                Line2D([0], [0], 
                       color="crimson", 
                       linewidth=2.2,
                       label="KDE (estimated density)")]

ax.legend(handles=legend_elements,
          loc="upper right",
          frameon=True,
          framealpha=0.9,
          facecolor="white",
          edgecolor="lightgray",
          fontsize=9)

#Personalizing the text
#Title
ax.set_title("Global Earthquake Magnitude Distribution\n"
             "Recorded events with magnitude ≥ 4.5 | 2000-2025",
             loc="left",
             fontsize=14,
             fontweight="bold",
             pad=18,
             color="#1a1a1a")

#Axis title
ax.set_xlabel("Reported Magnitude", fontsize=11, labelpad=12)
ax.set_ylabel("Number of Recorded Earthquakes", fontsize=11, labelpad=10)
ax.tick_params(axis="both", labelsize=10)

#Caption
caption_text = ("Source: U.S. Geological Survey (USGS), Earthquake Catalog\n"
                "Period: 2000-2025 | Minimum reported magnitude: 4.5\n"
                "Author: Davide Antoniazzi")
plt.figtext(0.98, 0.015, caption_text, fontsize=8, color="dimgray", ha="right", va = "bottom", style="italic")

plt.tight_layout()
plt.subplots_adjust(bottom=0.22)

#Exporting the plot
IMAGE_DIR = BASE_DIR / 'Images'
#IMAGE_DIR.mkdir(parents=True, exist_ok=True) #to create 'Image' folder
path_pdf = IMAGE_DIR / "1_Earthquakes_Frequency_Distribution.pdf"
plt.savefig(path_pdf, bbox_inches="tight")

plt.show()
# endregion