"""
SCRIPT CONTENT
The script is divided in sections, which can be expaned by clicking the down arrow at the left of the #region comment
The script presentes the following sections:
- Libraries
- Data File Path
- Uploading Data
- Data Cleaning
- Plotting the World Earthquakes Map

This script focuses on creating the third visualization. 
The map represents the visualization of each earthquake that was recorded in the period 1950-2025 in the world with magnitude equal or higher to 4.5.
The earthquakes are geolocalized based on their longitude and latitude, colored based on the depth of the focal point (in km).
The size of each point depends on the magnitude of that specific seismic event.
The plot is exported in PDF format in the 'Image' folder, named as '3_Global_Earthquakes_Map.pdf'
"""

# region Libraries
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from matplotlib.lines import Line2D
import seaborn as sns
import cartopy.crs as ccrs
import cartopy.feature as cfeature
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
# endregion

# region Plotting the World Earthquakes Map
#Setting the style
sns.set_theme(style="white")
plt.rcParams["font.family"] = "sans-serif"

#Preparing the data
eq_map = eq_cleaned.dropna(subset=["longitude", "latitude", "mag", "depth"]).copy()

#Keep only valid geographic coordinates and magnitudes
eq_map = eq_map[eq_map["longitude"].between(-180, 180)
                & eq_map["latitude"].between(-90, 90)
                & (eq_map["mag"] >= 4.5)
                & (eq_map["depth"] >= 0)]

#Marker area: small points for dense global coverage
sizes = 1.5 + (eq_map["mag"] - 4.5) ** 2 * 1.8

#Color scale for earthquake depth
depth_norm = Normalize(vmin=0, vmax=700, clip=True)

#Creating the figure
fig = plt.figure(figsize=(14, 8.5), dpi=300)
ax = fig.add_axes([0.035, 0.16, 0.93, 0.72],
                  projection=ccrs.Robinson(central_longitude=0))

#Map background
ax.set_global()
ax.add_feature(cfeature.OCEAN,
               facecolor="#f7f9fb",
               zorder=0)
ax.add_feature(cfeature.LAND,
               facecolor="#e8ecef",
               edgecolor="none",
               zorder=1)
ax.add_feature(cfeature.COASTLINE,
               edgecolor="#aab4bd",
               linewidth=0.4,
               zorder=2)
ax.add_feature(cfeature.BORDERS,
               edgecolor="#c4cbd1",
               linewidth=0.25,
               linestyle=":",
               zorder=2)

#Subtle graticule
gl = ax.gridlines(crs=ccrs.PlateCarree(),
                  draw_labels=False,
                  linewidth=0.35,
                  color="#aab4bd",
                  alpha=0.35,
                  linestyle="--",
                  zorder=2)

gl.xlocator = plt.FixedLocator(np.arange(-180, 181, 60))
gl.ylocator = plt.FixedLocator(np.arange(-60, 61, 30))

#Plotting earthquake epicentres
scatter = ax.scatter(eq_map["longitude"],
                     eq_map["latitude"],
                     s=sizes,
                     c=eq_map["depth"],
                     cmap="inferno",
                     norm=depth_norm,
                     alpha=0.80,
                     edgecolors="none",
                     transform=ccrs.PlateCarree(),
                     rasterized=True,
                     zorder=3)

#Magnitude legend
legend_magnitudes = [5, 7, 9]
legend_elements = [
    Line2D([0], [0],
           marker="o",
           linestyle="none",
           markerfacecolor="#536575",
           markeredgecolor="none",
           markersize=np.sqrt(1.5 + (mag - 4.5) ** 2 * 1.8) * 1.15,
           label=f"M {mag:.1f}")
           for mag in legend_magnitudes]
legend = ax.legend(handles=legend_elements,
                   title="Magnitude",
                   loc="lower left",
                   bbox_to_anchor=(0.025, 0.035),
                   bbox_transform=ax.transAxes,
                   frameon=True,
                   framealpha=0.97,
                   facecolor="white",
                   edgecolor="#b8c2cc",
                   fontsize=10,
                   title_fontsize=11,
                   labelspacing=1,
                   handletextpad=1,
                   borderpad=1)
legend.get_title().set_fontweight("bold")
legend.set_zorder(5)

#Depth colorbar
cax = fig.add_axes([0.30, 0.105, 0.40, 0.022])
cbar = fig.colorbar(scatter,
                    cax=cax,
                    orientation="horizontal",
                    extend="max")
cbar.set_label("Focal depth (km)",
               fontsize=11,
               fontweight="bold",
               labelpad=7)
cbar.set_ticks([0, 100, 300, 500, 700])
cbar.ax.tick_params(labelsize=10, length=3, width=0.8)

# Title
fig.suptitle("Global Distribution of Earthquakes",
             x=0.5,
             y=0.97,
             fontsize=19,
             fontweight="bold",
             color="#202830",
             ha="center")
fig.text(0.5, 0.925,
         "Recorded events with magnitude ≥ 4.5 | 1950-2025",
         ha="center",
         va="center",
         fontsize=12,
         color="#5c6770")

#Source and data notes
fig.text(0.97, 0.025,
         "Source: USGS Earthquake Catalog\n"
         f"Events plotted: {len(eq_map):,} | Events with missing depth excluded\n"
         "Author: Antoniazzi Davide",
         ha="right",
         va="bottom",
         fontsize=8,
         color="#68737d",
         style="italic")

#Export
#IMAGE_DIR.mkdir(parents=True, exist_ok=True)
IMAGE_DIR = BASE_DIR / 'Images'
path_pdf = IMAGE_DIR / "3_Global_Earthquakes_Map.pdf"
fig.savefig(path_pdf,
            bbox_inches="tight",
            facecolor="white")

plt.show()
# endregion