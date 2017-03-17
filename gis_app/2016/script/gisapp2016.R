
##########################################################################################
##########################################################################################
##							Spatial Interpolation with R 								##
##								GIS Application											##
##							   March 29-30, 2016										##
##								Avit K. Bhowmik											##
##########################################################################################
##########################################################################################

##########################################################################################
##										Day 1			 								##
##									March 29, 2016										##
##########################################################################################

## Set the working directory

setwd("/Users/avitbhowmik/Teaching/teaching_materials/gisapp2016/data")


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

## Access coordinates and data

bd.boundary@bbox

bd.boundary@data

bd.boundary@data$UNREG1


# Plot shapefile

plot(bd.boundary)

spplot(bd.boundary["UNREG1"])


## Point spatial data from comma-separated (or text) values. Visualizing on polygons.

bd.trace.metal <- read.csv("bd_trace_metal.csv", sep=",", header=TRUE)

class(bd.trace.metal)

head(bd.trace.metal)

tail(bd.trace.metal)


######################################################

## Exercise 1: Is bd.trace.metal a space-time data? ##
## If so, what are the space-time attributes of it? ##

######################################################

## Convert into SpatialPointsDataFrame and visualize them

coordinates(bd.trace.metal) <- ~LONG_DEG+LAT_DEG
proj4string(bd.trace.metal) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

class(bd.trace.metal)

head(bd.trace.metal@coords)

head(bd.trace.metal@data)

spplot(bd.trace.metal, xlim=c(87.9, 92.9), ylim=c(20.6, 26.8),
	sp.layout=list("sp.polygons", bd.boundary, col="gray"),
	col.regions=colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(100),
	scales=list(draw=T), colorkey=T)

bd.trace.metal <- bd.trace.metal[!is.na(bd.trace.metal@data$Pb),]

## Save it as a shape file

writePointsShape(bd.trace.metal, "bd_trace_metal")


# Spatial Grids, i.e. Rasters

library(raster)

bd.elv <- raster("bd_elv.tif")

bd.elv[Which(bd.elv<=0, cells=TRUE)] <- NA

bd.elv.grid <- as(bd.elv, "SpatialGridDataFrame")

class(bd.elv)

plot(bd.elv)

plot(bd.boundary, add=T)

plot(bd.trace.metal, add=T, col="red")

spplot(bd.elv.grid, sp.layout=list("sp.lines", as(bd.boundary, "SpatialLines")),
	col.regions=topo.colors(100), scales=list(draw=T))


################################################################

## Exercise 2: Load the other rasters from the data directory ##
## Explore them from the plot ##

################################################################

bd.soc <- raster("bd_soc.tif")
bd.scc <- raster("bd_scc.tif")
bd.ph <- raster("bd_ph.tif")
bd.wc <- raster("bd_wc.tif")
bd.pop.dens <- raster("bd_pop_dens.tif")

## Extract values of the spatial predictors at the ground water sampling points

plot(bd.scc)

bd.trace.metal@data$elv <- extract(bd.elv, bd.trace.metal)
bd.trace.metal@data$soc <- extract(bd.soc, bd.trace.metal)

head(bd.trace.metal@data)

#############################################################

## Exercise 3: Extract values of SCC, pH, WC and POP_DENS ##

#############################################################

bd.trace.metal@data$scc <- extract(bd.scc, bd.trace.metal)
bd.trace.metal@data$ph <- extract(bd.ph, bd.trace.metal)
bd.trace.metal@data$wc <- extract(bd.wc, bd.trace.metal)
bd.trace.metal@data$popdens <- extract(bd.pop.dens, bd.trace.metal)

## Remove NA values from data

bd.trace.metal <- bd.trace.metal[!is.na(bd.trace.metal@data$soc),]
bd.trace.metal <- bd.trace.metal[!is.na(bd.trace.metal@data$popdens),]

## Remove duplicated from data

zerodist(bd.trace.metal, zero=0.008333333)
bd.trace.metal <- remove.duplicates(bd.trace.metal)

writePointsShape(bd.trace.metal, "bd_trace_metal")


##########################################################################################
##										Day 2			 								##
##									March 30, 2016										##
##########################################################################################

## Read the shapefile with lead concentration and predictor values

library(maptools)

setwd("/Users/avitbhowmik/Teaching/teaching_materials/gisapp2016/data")

bd.trace.metal <- readShapePoints("bd_trace_metal")

head(bd.trace.metal@data)

# Distance between sampling points

dis.pts <- spDists(bd.trace.metal@coords, longlat=TRUE)

max(dis.pts)

min(dis.pts[which(dis.pts!=0)])

##################################################################

# Exercise 4: Explore R objects and rgeos package
# What are the location (coordinates) of the
# sampling points with the highest and lowest Pb concentration?
# Plot them on a map of Bangladesh.
# Calculate the geographic distance between them.

##################################################################

# Solve 4

bd.trace.metal@coords[which(bd.trace.metal@data$Pb==max(bd.trace.metal@data$Pb)),]

bd.trace.metal@coords[which(bd.trace.metal@data$Pb==min(bd.trace.metal@data$Pb)),]

bd.boundary <- readShapePoly("bd_boundary")

plot(bd.boundary)

plot(bd.trace.metal[which(bd.trace.metal@data$Pb==max(bd.trace.metal@data$Pb)),], add=T, col="red")

plot(bd.trace.metal[which(bd.trace.metal@data$Pb==min(bd.trace.metal@data$Pb)),], add=T, col="red")

library(rgeos)

gDistance(bd.trace.metal[which(bd.trace.metal@data$Pb==max(bd.trace.metal@data$Pb)),],
bd.trace.metal[which(bd.trace.metal@data$Pb==min(bd.trace.metal@data$Pb)),])

## Check for Skewness

library(moments)

skewness(bd.trace.metal@data$Pb)

## High positive skewness, hence we will log transform the data before
## any statistical analysis

# Spatial trend in the data, stationarity

summary(lm(log(bd.trace.metal@data$Pb)~bd.trace.metal@coords[,1]+bd.trace.metal@coords[,2]))

summary(lm(log(Pb)~elv+soc+scc+ph+wc+popdens, data=bd.trace.metal@data))

#################################################################################
## Fitting variogram
#################################################################################

## Variogram modelling

library(gstat)

## Without predictors

var.Pb <- variogram(log(Pb)~1, bd.trace.metal)

plot(var.Pb)

## With spatial predictors

var.Pb <- variogram(log(Pb)~elv+soc+scc+ph+wc+popdens+LONG_DEG+LAT_DEG,
	bd.trace.metal, cutoff=700)

plot(var.Pb)

## Explore variogram models

vgm()

## Fit an exponential model

vmod.Pb.exp <- fit.variogram(object=var.Pb, model=vgm(psill=0.4, model="Exp",
	range=150, nugget=0.3), fit.sills=TRUE, fit.ranges=TRUE, fit.method=7)
	
plot(var.Pb, vmod.Pb.exp)

attr(vmod.Pb.exp, "SSErr")

## Fit a power model

vmod.Pb.pow <- fit.variogram(object=var.Pb, model=vgm(psill=0.4, model="Pow", range=1.5,
	nugget=0.4), fit.sills=TRUE, fit.ranges=TRUE, fit.method=6)

plot(var.Pb, vmod.Pb.pow)

attr(vmod.Pb.pow, "SSErr")


##################################################################

# Exercise 5: Fit a spherical model and check the error statistics

##################################################################

# Spherical model

vmod.Pb.sph <- fit.variogram(object=var.Pb, model=vgm(psill=0.4, model="Sph", range=4,
	nugget=0.4), fit.sills=TRUE, fit.ranges=TRUE, fit.method=7)

plot(var.Pb, vmod.Pb.sph)

attr(vmod.Pb.sph, "SSErr")

###########################################################################################################

# Spatial interpolation: Kriging

###########################################################################################################

## Prepare prediction grid

library(raster)

bd.elv.grid <- as(raster("bd_elv.tif"), "SpatialGridDataFrame")
bd.soc.grid <- as(raster("bd_soc.tif"), "SpatialGridDataFrame")
bd.scc.grid <- as(raster("bd_scc.tif"), "SpatialGridDataFrame")
bd.ph.grid <- as(raster("bd_ph.tif"), "SpatialGridDataFrame")
bd.wc.grid <- as(raster("bd_wc.tif"), "SpatialGridDataFrame")
bd.pop.dens.grid <- as(raster("bd_pop_dens.tif"), "SpatialGridDataFrame")

bd.predictors.grid <- bd.elv.grid

bd.predictors.grid@data <- data.frame(bd.predictors.grid@data, bd.soc.grid@data,
	bd.scc.grid@data, bd.ph.grid@data, bd.wc.grid@data, bd.pop.dens.grid@data,
	coordinates(bd.predictors.grid))
	
head(bd.predictors.grid@data)

colnames(bd.trace.metal@data)

colnames(bd.predictors.grid@data) <- c("elv", "soc", "scc", "ph", "wc", "popdens",
	"LONG_DEG", "LAT_DEG")

spplot(bd.predictors.grid[2:5])

## Adjusting projection systems and removing duplicates

proj4string(bd.trace.metal) <- proj4string(bd.predictors.grid)

plot(bd.trace.metal)


## Weighted least square prediction

krg_wls <- krige(log(Pb)~elv+soc+scc+ph+wc+popdens+LONG_DEG+LAT_DEG,
bd.trace.metal, newdata=bd.predictors.grid)

krg_wls@data$var1.pred <- exp(krg_wls@data$var1.pred)

spplot(krg_wls["var1.pred"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))

## Universal Kriging or Kriging with external drift

krg_Pb <- krige(log(Pb)~elv+soc+scc+ph+wc+popdens+LONG_DEG+LAT_DEG,
bd.trace.metal, newdata=bd.predictors.grid, model=vmod.Pb.exp)

krg_Pb@data$var1.pred <- exp(krg_Pb@data$var1.pred)

spplot(krg_Pb["var1.pred"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))

## Calculate the hazard quotient (HQ) values by comparing to
## the thresholds for drinking water, and acute and chronic risk

krg_Pb@data$hq_drink <- krg_Pb@data$var1.pred/10
krg_Pb@data$hq_ecoacute <- krg_Pb@data$var1.pred/65
krg_Pb@data$hq_ecochron <- krg_Pb@data$var1.pred/2.5
	
spplot(krg_Pb[3:5], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))


################################# End #######################################
#############################################################################
#############################################################################



