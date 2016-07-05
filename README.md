### nearth

R tools for easy navigation and reading of locally stored data files from [Natural Earth](http://www.naturalearthdata.com/)

#### Requirements

+ [R](https://www.r-project.org/)

+ [Natural Earth vector and raster data files](http://www.naturalearthdata.com/)
  Download and extract the data you desire.  Place vector and raster data in different directories.   
  
For the vector dataset it is pretty easy to [download](http://www.naturalearthdata.com/downloads/) the entire suite of data.  Rasters, on the other hand, are quite big and I decided to download just a few. 

#### Suggested R packages

+ [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html)
+ [raster](https://cran.r-project.org/web/packages/raster/index.html) 


#### Installation

It is easy to install with [devtools](https://cran.r-project.org/web/packages/devtools/index.html)

```R
library(devtools)
install_github("btupper/nearth")
```

#### Configuration within R

The `nearth` package will find files for you by name.  To simplify this process, you should do the following either upon startup of R or before you use `nearth`.

```R
options(NEARTH_VECTOR_PATH = "/Users/Shared/data/natural_earth/vector",
        NEARTH_RASTER_PATH = "/Users/Shared/data/natural_earth/raster")
```

I find it easiest to add this to my ~/.Rprofile configuration file so these are defined when R starts.  Here's more information on ~/.Rprofile from [Quick-R](http://www.statmethods.net/interface/customizing.html).

#### Usage

```R
library(nearth)

# find a vector file by name
vfile <- find_nearth_vectors(name = 'ne_50m_coastline')

#find a raster file by name
rfile <- find_nearth_rasters(name = 'NE1_50M_SR_W')
```

If the `rgdal` and `raster` packages are installed, you can read the Natural Earth products using `read_nearth`.  *NOTE: you can provide just the name of the shapefile as shown below rather than the full filepath specification.*

```R
library(nearth)
library(rgdal)
library(raster)

v <- read_nearth(name = 'ne_50m_coastline', what = 'vector')
# note that we geta list back
sp::spplot(v[[1]])

# read and crop the vectors to a bounding box [left, right, bottom, top]
# cropping can make for faster graphics in some cases
v <- read_nearth(name = 'ne_50m_coastline', what = 'vector', bb = c(-73,-62,39,45))

r <- read_nearth(name = 'NE1_50M_SR_W', what = 'raster', form = 'brick')
raster::plotRGB(r)

```
