import rasterio                   
import numpy as np
import matplotlib.pyplot as plt
import rioxarray  
import geopandas as gpd
from rasterio.enums import Resampling
from rasterstats import zonal_stats

countries = gpd.read_file("ne_50m_admin_0_countries.shp")
countries.head()
kenya = countries.loc[countries['SOVEREIGNT'] == "Kenya"]

kenya.plot()
plt.show()
# import rasters
raster2000 = rioxarray.open_rasterio("raster2000.tif", masked=True)
raster2005 = rioxarray.open_rasterio("raster2005.tif", masked=True)
raster2010 = rioxarray.open_rasterio("raster2010.tif", masked=True)
raster2015 = rioxarray.open_rasterio("raster2015.tif", masked=True)

raster2000.rio.crs
kenya.crs

print('raster2000: ', raster2000.rio.crs)
print('kenya: ', kenya.crs)

# clip rasters to the region of kenya
kenya2000 = raster2000.rio.clip(kenya.geometry.values, from_disk=True) #from_disk = True makes this operation faster
kenya2005 = raster2005.rio.clip(kenya.geometry.values, from_disk=True)
kenya2010 = raster2010.rio.clip(kenya.geometry.values, from_disk=True)
kenya2015 = raster2015.rio.clip(kenya.geometry.values, from_disk=True)

# check plot 
kenya2015.plot()
plt.show()

# reproject raster to include only urban areas and the rest 0
kenya2000urban = kenya2000.where(kenya2000 != 190, 1)
kenya2000urban = kenya2000urban.where(kenya2000urban == 1, np.nan)

kenya2005urban = kenya2005.where(kenya2005 != 190, 1)
kenya2005urban = kenya2005urban.where(kenya2005urban == 1, np.nan)

kenya2010urban = kenya2010.where(kenya2010 != 190, 1)
kenya2010urban = kenya2010urban.where(kenya2010urban == 1, np.nan)

kenya2015urban = kenya2015.where(kenya2015 != 190, 1)
kenya2015urban = kenya2015urban.where(kenya2015urban == 1, np.nan)



# plot kenya2000urban and kenya which is a shape file on the same file
fig, ax = plt.subplots()
kenya2000urban.plot()
kenya.plot(ax=ax, facecolor="none", edgecolor="black", linewidth=1)
plt.show()



# count number of pixels
kenya2015urban.rio.to_raster('kenya2015urban.tif') #Save as a raster 
kenya.to_file('kenya.shp') #Save as a shp
urban_count2000 = zonal_stats('kenya.shp', 'kenya2000urban.tif', stats = 'count',nodata=np.nan)
urban_count2005 = zonal_stats('kenya.shp', 'kenya2005urban.tif', stats = 'count',nodata=np.nan)
urban_count2010 = zonal_stats('kenya.shp', 'kenya2010urban.tif', stats = 'count',nodata=np.nan)
urban_count2015 = zonal_stats('kenya.shp', 'kenya2015urban.tif', stats = 'count',nodata=np.nan)

data = np.asarray(kenya2015urban).flatten()
np.count_nonzero(~np.isnan(data))
# urban land expansion

kenya2000 = kenya2000 / 10
kenya_change = kenya2000 + kenya2015*10
#count pixels. numbers from 1900 until 1920 shows the previous land-use classes that are now urban land in 2015


# I would like to save the raster kenya2000 as a tif file on my computer
kenya2000.rio.to_raster('kenya2000.tif') #Save as a raster
kenya2005.rio.to_raster('kenya2005.tif') #Save as a raster
kenya2010.rio.to_raster('kenya2010.tif') #Save as a raster
kenya2015.rio.to_raster('kenya2015.tif') #Save as a raster