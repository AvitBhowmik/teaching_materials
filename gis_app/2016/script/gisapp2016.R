
##########################################################################################
##########################################################################################
##							Spatial Interpolation with R 								##
##								GIS Application											##
##							   March 29-30, 2016										##
##								Avit K. Bhowmik											##
##########################################################################################
##########################################################################################

## Set the working directory

setwd("/Users/avitbhowmik/Teaching/teaching_materials/gis_app/2016/data")


## Read and visualize "Shapefiles" - spatial objects

## Load the spatial package

library(maptools)

## Spatial Polygons, i.e. Area

bd.boundary <- readShapePoly("bd_boundary")

class(bd.boundary)

summary(bd.boundary)

## Visit http://spatialreference.org/ref/epsg/25832/

proj4string(bd.boundary) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(bd.boundary)

## Access coordinates and data

bd.boundary@bbox

bd.boundary@data

bd.boundary@data$NAME_OBSOL


## Point spatial data from comma-separated (or text) values. Visualizing on polygons.

bd.trace.metal <- read.csv("bd_trace_metal.csv", sep=",", header=TRUE)

class(bd.trace.metal)

head(bd.trace.metal)

tail(bd.trace.metal)


######################################################

## Exercise 1: Is bd.trace.metal a space-time data? ##
## If so, what are the space-time attributes of it? ##

######################################################

## Convert into SpatialPointsDataFrame

bd.trace.metal <- bd.trace.metal[,c(1:2,11)]

coordinates(bd.trace.metal) <- ~LONG_DEG+LAT_DEG
proj4string(bd.trace.metal) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

class(bd.trace.metal)

bd.trace.metal@coords

spplot(bd.trace.metal, xlim=c(87.9, 92.9), ylim=c(20.6, 26.8),
	sp.layout=list("sp.polygons", bd.boundary, col="gray"),
	col.regions=colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(100),
	scales=list(draw=T), colorkey=T)

bd.trace.metal <- bd.trace.metal[!is.na(bd.trace.metal@data$Zn),]


# Spatial Grids, i.e. Rasters

library(raster)

bd.elv <- raster("bd_elv.tif")

class(bd.elv)

plot(bd.elv)

bd.elv.grid <- as(bd.elv, "SpatialGridDataFrame")

class(bd.elv.grid)

head(bd.elv.grid@data)

spplot(bd.elv.grid)

plot(bd.boundary, add=T)

spplot(bd.elv, sp.layout=list("sp.polygons", bd.boundary),
	col.regions=topo.colors(100), scales=list(draw=T))


bd.soc <- raster("bd_soc.tif")
bd.soc.grid <- readAsciiGrid("bd_soc.asc")
proj4string(bd.soc.grid) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bd.scc <- raster("bd_scc.tif")
bd.ph <- raster("bd_ph.tif")
bd.wc <- raster("bd_wc.tif")
bd.pop.dens <- raster("bd_pop_dens.tif")


class(bd.predictors)

head(bd.predictors@data)

spplot(bd.predictors)

# Clipping the raster to our analysis extent.

Landau_region_raster <- raster(Clipped_LandauGrid)

writeRaster(Landau_region_raster, "Landau_region_raster", format="GTiff")

###################################################################################################

# Exercise 1: Read and Visualize the output spatial objects from the last two days.

###################################################################################################





# Create a wide table and store installed power in individual year column for each building

library(reshape)

inst_pow_wt <- as.data.frame(cast(inst_pow_st, X+Y~inst_year, fun.aggregate=mean, value="inst_power"))

head(inst_pow_wt)

inst_pow_wt[, c("1992", as.character(1994:1997),"1999")] <- NaN

inst_pow_wt <- inst_pow_wt[,c("X", "Y", as.character(1991:2013))]

sp <- SpatialPoints(inst_pow_wt[,1:2], proj4string=CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"))

library(xts)

time <- as.xts(ts(data=1:23,start=1991,end=2013,frequency=1))

data <- as.data.frame(melt(inst_pow_wt[,as.character(1991:2013)])$value)

names(data) <- "Installed_power_Landau"

inst_pow_stfdf <- STFDF(sp, time, data)

str(inst_pow_stfdf)

stplot(inst_pow_stfdf, 1991:2013, col.regions=bpy.colors(1000,), sp.layout=list("sp.lines", Landau.region.boundary, col="gray"), xlab="Longitudes", ylab="Latitudes", scales=list(draw=T), layout=c(6,4))


## Spatial data

coordinates(GISAppData) = ~X + Y

proj4string(GISAppData)  <- CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

plot(GISAppData)

head(GISAppData@coords)

head(GISAppData@data)

spplot(GISAppData["rad_mean"], sp.layout=list("sp.polygons", Landau.region), col.regions=bpy.colors(), colorkey=TRUE)

# Write shape files

writePointsShape(GISAppData, "GISAppData")

#########################################################################################################################

# Exercise 3: Plot the other attributes of GISAppData using "spplot" and
# check (visually) if there is a spatial structure.

#########################################################################################################################

# Area of our region

GISAppData <- readShapePoints("GISAppData")

library(rgeos)

gArea(Landau.region)/1000000

# Distance between roof tops

dist_bet_rt <- spDists(GISAppData@coords, longlat=TRUE)

max(dist_bet_rt)

min(dist_bet_rt[which(dist_bet_rt!=0)])

##########################################################################################################################

# Exercise 4: Explore R objects and rgeos package
# What are the location of the buildings (street names) with the highest and lowest mean radiation?
# Plot them on a map of Landau region.
# Calculate the geographic distance between them.

##########################################################################################################################

# Solve 4

GISAppData@data[which(GISAppData@data$rad_mean==max(GISAppData@data$rad_mean)),"adress"]

GISAppData@data[which(GISAppData@data$rad_mean==min(GISAppData@data$rad_mean)),"adress"]

plot(Landau.region)

plot(GISAppData[which(GISAppData$rad_mean==max(GISAppData$rad_mean)),], add=T, col="red")

plot(GISAppData[which(GISAppData$rad_mean==min(GISAppData$rad_mean)),], add=T, col="red")

gDistance(GISAppData[which(GISAppData@data$adress=="Schmiedstrae 5"),], GISAppData[which(GISAppData@data$adress=="Breslauer Strae 5a"),])





#############################################################################################################

#Exercise 5: How are the mean radiations and building ages across k-means clusters? 

#############################################################################################################

## Geostatistics



bd.trace.metal@data$elv <- extract(bd.elv, bd.trace.metal)
bd.trace.metal@data$soc <- extract(bd.soc, bd.trace.metal)
bd.trace.metal@data$scc <- extract(bd.scc, bd.trace.metal)
bd.trace.metal@data$ph <- extract(bd.ph, bd.trace.metal)
bd.trace.metal@data$wc <- extract(bd.wc, bd.trace.metal)
bd.trace.metal@data$popdens <- extract(bd.pop.dens, bd.trace.metal)

bd.trace.metal <- bd.trace.metal[!is.na(bd.trace.metal@data$soc),]

# Spatial structure

summary(lm(bd.trace.metal@data$Pb~bd.trace.metal@coords[,1]+bd.trace.metal@coords[,2]))

summary(lm(Pb~elv, data=bd.trace.metal@data))

###########################################################################################################

# Exercise 6: Tell me the status of the spatial structure in peak yield.

###########################################################################################################

## Variogram modelling

library(gstat)

# For slope 

var.Zn <- variogram(log(Zn)~1, bd.trace.metal)

var.Pb <- variogram(Pb~elv+soc+scc+ph+wc+popdens, bd.trace.metal, cutoff=550)

plot(var.Zn)

## Explore variogram models
vgm()

# Exponential model

vmod.Zn.exp <- vgm(psill=0.8, model="Exp", range=250, nugget=0.4)

vmod.Pb.exp <- fit.variogram(object=var.Pb, model=vgm(psill=1, model="Exp", range=400,
	nugget=0.4), fit.sills=TRUE, fit.ranges=TRUE, fit.method=7)

plot(var.Zn, vmod.Zn.exp)

# Spherical model

vmod.Pb.sph <- fit.variogram(object=var.Pb, model=vgm(psill=3, model="Sph", range=400,
	nugget=0.5), fit.sills=TRUE, fit.ranges=TRUE, fit.method=7)

plot(var.Pb, vmod.Pb.sph)

vmod.Pb.gau <- fit.variogram(object=var.Pb, model=vgm(psill=1, model="Gau", range=400,
	nugget=0.5), fit.sills=TRUE, fit.ranges=TRUE, fit.method=7)

plot(var.Pb, vmod.Pb.gau)

vmod.Pb.pow <- fit.variogram(object=var.Pb, model=vgm(psill=1, model="Pow", range=1,
	nugget=0.5), fit.sills=TRUE, fit.ranges=TRUE, fit.method=6)

plot(var.Pb, vmod.Pb.pow)

# Evaluate two fitted models by their goodness of fit (Sum of Squared Error)
attr(vmod.Pb.exp, "SSErr")
attr(vmod.Pb.sph, "SSErr")
attr(vmod.Pb.gau, "SSErr")
attr(vmod.Pb.pow, "SSErr")

###########################################################################################################

# Exercise 7: Fit a variogram for the aspect.

###########################################################################################################

## Spatial interpolation: Kriging

bd.elv.pts <- rasterToPoints(bd.elv)
bd.soc.pts <- rasterToPoints(bd.soc)
bd.scc.pts <- rasterToPoints(bd.scc)
bd.ph.pts <- rasterToPoints(bd.ph)
bd.wc.pts <- rasterToPoints(bd.wc)
bd.pop.dens.pts <- rasterToPoints(bd.pop.dens)

bd.elv.soc <- merge(bd.elv.pts, bd.soc.pts, by=c("x", "y"))
bd.elv.soc.scc <- merge(bd.elv.soc, bd.scc.pts, by=c("x", "y"))
bd.elv.soc.scc.ph <- merge(bd.elv.soc.scc, bd.ph.pts, by=c("x", "y"))
bd.elv.soc.scc.ph.wc <- merge(bd.elv.soc.scc.ph, bd.wc.pts, by=c("x", "y"))
bd.elv.soc.scc.ph.popdens <- merge(bd.elv.soc.scc.ph.wc, bd.pop.dens.pts, by=c("x", "y"))

predictors <- bd.elv.soc.scc.ph.popdens
colnames(predictors) <- c("x", "y", "elv", "soc", "scc", "ph", "wc", "popdens")

coordinates(predictors) <- ~x+y

proj4string(predictors) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

gridded(predictors) = TRUE

class(predictors)

spplot(predictors["soc"])

krg_Zn <- krige(log(Zn)~1, bd.trace.metal, newdata=bd.soc.grid,
	model=vmod.Zn.exp)

krg_Pb <- krige(Pb~elv+soc+scc+ph+wc+popdens, bd.trace.metal, newdata=predictors,
	model=vmod.Pb.exp)

Landau.region.boundary <- as(Landau.region, "SpatialLines")

spplot(krg_Pb["var1.pred"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))


?writePolyShape

?writeAsciiGrid

?writeRaster

writeAsciiGrid (lm.Production2010.Surface, "Production2010.asc")

Production.2010<-readAsciiGrid("Production2010.asc" )

spplot(Production.2010, col.regions=bpy.colors())

Production_2010<-raster(Production.2010)

writeRaster(Production_2010, filename="Production_2010", format="GTiff")

m<-raster("Production_2010.tif")

plot(m)

################################# End ###################################################################
