### R code from vignette source 'over.Rnw'

###################################################
### code chunk number 1: over.Rnw:105-107 (eval = FALSE)
###################################################
## A %over% B
## over(A, B)


###################################################
### code chunk number 2: over.Rnw:117-118 (eval = FALSE)
###################################################
## A[B,]


###################################################
### code chunk number 3: over.Rnw:121-122 (eval = FALSE)
###################################################
## A[!is.na(over(A,B)),]


###################################################
### code chunk number 4: over.Rnw:125-141
###################################################
library(sp)
x = c(0.5, 0.5, 1.0, 1.5)
y = c(1.5, 0.5, 0.5, 0.5)
xy = cbind(x,y)
dimnames(xy)[[1]] = c("a", "b", "c", "d")
pts = SpatialPoints(xy)

xpol = c(0,1,1,0,0)
ypol = c(0,0,1,1,0)
pol = SpatialPolygons(list(
	Polygons(list(Polygon(cbind(xpol-1.05,ypol))), ID="x1"),
	Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
	Polygons(list(Polygon(cbind(xpol,ypol - 1.0))), ID="x3"),
	Polygons(list(Polygon(cbind(xpol + 1.0, ypol))), ID="x4"),
	Polygons(list(Polygon(cbind(xpol+.4, ypol+.1))), ID="x5")
	))


###################################################
### code chunk number 5: over.Rnw:146-158
###################################################
if (require(RColorBrewer, quietly = TRUE)) {
 pal = brewer.pal(5, "Set2")
 col = paste0(pal, "4D")
} else {
 pal = 1:5
 col = pal
}
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=pal, axes=TRUE, col = col)
points(pts, col='red')
text(c(-1,0.1,0.1,1.9,0.45), c(0.05,0.05,-.95,0.05,0.15), 
	c("x1", "x2", "x3", "x4", "x5"))
text(coordinates(pts), pos=1, row.names(pts))


###################################################
### code chunk number 6: over.Rnw:165-166
###################################################
over(pts, pol)


###################################################
### code chunk number 7: over.Rnw:173-174
###################################################
over(pts, pol, returnList = TRUE)


###################################################
### code chunk number 8: over.Rnw:180-181
###################################################
pts[pol]


###################################################
### code chunk number 9: over.Rnw:186-189
###################################################
over(pol, pts)
over(pol, pts, returnList = TRUE)
row.names(pol[pts])


###################################################
### code chunk number 10: over.Rnw:194-197
###################################################
if (require(rgeos, quietly = TRUE)) {
 over(pol, pol, returnList = TRUE)
}


###################################################
### code chunk number 11: over.Rnw:216-225
###################################################
zdf = data.frame(z1 = 1:4, z2=4:1, f = c("a", "a", "b", "b"),
	row.names = c("a", "b", "c", "d"))
zdf
ptsdf = SpatialPointsDataFrame(pts, zdf)

zpl = data.frame(z = c(10, 15, 25, 3, 0), zz=1:5, 
	f = c("z", "q", "r", "z", "q"), row.names = c("x1", "x2", "x3", "x4", "x5"))
zpl
poldf = SpatialPolygonsDataFrame(pol, zpl)


###################################################
### code chunk number 12: over.Rnw:229-230
###################################################
over(pts, poldf)


###################################################
### code chunk number 13: over.Rnw:238-239
###################################################
over(pts, poldf[1:2], fn = mean)


###################################################
### code chunk number 14: over.Rnw:244-245
###################################################
over(pts, poldf, returnList = TRUE)


###################################################
### code chunk number 15: over.Rnw:249-251
###################################################
over(pol, ptsdf)
over(pol, ptsdf[1:2], fn = mean)


###################################################
### code chunk number 16: over.Rnw:269-272
###################################################
l1 = Lines(Line(coordinates(pts)), "L1")
l2 = Lines(Line(rbind(c(1,1.5), c(1.5,1.5))), "L2")
L = SpatialLines(list(l1,l2))


###################################################
### code chunk number 17: over.Rnw:296-300
###################################################
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=2:6, axes=TRUE)
text(c(-1,0.1,0.1,1.1,0.45), c(0,0,-1.05,0,0.1), c("x1", "x2", "x3", "x4", "x5"))
lines(L, col = 'green')
text(c(0.52, 1.52), c(1.5, 1.5), c("L1", "L2"))


###################################################
### code chunk number 18: over.Rnw:308-318
###################################################
if (require(rgeos, quietly = TRUE)) {
over(pol, pol)
over(pol, pol,returnList = TRUE)
over(pol, L)
over(L, pol)
over(L, pol, returnList = TRUE)
over(L, L)
over(pts, L)
over(L, pts)
}


###################################################
### code chunk number 19: over.Rnw:323-333
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
Pt = list(x = c(178274.9,181639.6), y = c(329760.4,333343.7))
sl = SpatialLines(list(Lines(Line(cbind(Pt$x,Pt$y)), "L1")))
image(meuse.grid)
if (require(rgeos, quietly = TRUE)) {
 xo = over(sl, geometry(meuse.grid), returnList = TRUE)
 image(meuse.grid[xo[[1]], ],col=grey(0.5),add=T)
 lines(sl)
}


###################################################
### code chunk number 20: over.Rnw:344-349
###################################################
g = SpatialGrid(GridTopology(c(0,0), c(1,1), c(3,3)))
p = as(g, "SpatialPolygons")
px = as(g, "SpatialPixels")
plot(g)
text(coordinates(g), labels = 1:9)


###################################################
### code chunk number 21: over.Rnw:353-359
###################################################
if (require(rgeos, quietly = TRUE)) {
over(g,g)
over(p,p)
over(p,g)
over(g,p)
}


###################################################
### code chunk number 22: over.Rnw:364-368
###################################################
if (require(rgeos, quietly = TRUE)) {
over(px[5], g, returnList = TRUE)
over(p[c(1,5)], p, returnList = TRUE)
}


###################################################
### code chunk number 23: over.Rnw:384-388
###################################################
if (require(rgeos, quietly = TRUE)) {
over(px[5], g, returnList = TRUE, minDimension = 0)
over(p[c(1,5)], p, returnList = TRUE, minDimension = 0)
}


###################################################
### code chunk number 24: over.Rnw:408-411
###################################################
if (require(rgeos, quietly = TRUE)) {
over(p, p, minDimension = 0)
}


###################################################
### code chunk number 25: over.Rnw:415-426
###################################################
x2 = x1 = cbind(c(0,1,1,0,0), c(0,0,1,1,0))
x1[,1] = x1[,1]+0.5
x1[,2] = x1[,2]+0.25
sp = SpatialPolygons(list(
 Polygons(list(Polygon(x1)), "x1"),
 Polygons(list(Polygon(x2)), "x2")))
pt = SpatialPoints(cbind(0.5,0.5)) # on border of x1
row.names(pt) = "pt1"
plot(sp, axes = TRUE)
text(c(0.05, 0.55, 0.55), c(0.9, 1.15, 0.5), c("x1","x2", "pt"))
plot(pt, add=TRUE, col='red', pch=16)


###################################################
### code chunk number 26: over.Rnw:432-440
###################################################
if (require(rgeos, quietly = TRUE)) {
over(pt,sp)
over(pt,sp,minDimension=0)
over(pt,sp,returnList=TRUE)
rgeos::overGeomGeom(pt,sp)
rgeos::overGeomGeom(pt,sp,returnList=TRUE)
rgeos::overGeomGeom(pt,sp,returnList=TRUE,minDimension=0)
}


###################################################
### code chunk number 27: over.Rnw:456-464
###################################################
if (require(rgeos, quietly = TRUE)) {
over(p[5], p, returnList=TRUE, minDimension=0)
over(p[5], p, returnList=TRUE, minDimension=1)
over(p[5], p, returnList=TRUE, minDimension=2)
rgeos::overGeomGeom(pt, pt, minDimension=2) # empty
rgeos::overGeomGeom(pt, pt, minDimension=1) # empty
rgeos::overGeomGeom(pt, pt, minDimension=0)
}


###################################################
### code chunk number 28: over.Rnw:470-476
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
off = gridparameters(meuse.grid)$cellcentre.offset + 20
gt = GridTopology(off, c(400,400), c(8,11))
SG = SpatialGrid(gt)
agg = aggregate(meuse.grid[3], SG, mean)


###################################################
### code chunk number 29: over.Rnw:486-488
###################################################
image(agg)
points(meuse.grid, pch = 3, cex=.2, col = "#80808080")


###################################################
### code chunk number 30: over.Rnw:497-502
###################################################
if (require(rgeos, quietly = TRUE)) {
sl.agg = aggregate(meuse.grid[,1:3], sl, mean)
class(sl.agg)
as.data.frame(sl.agg)
}


###################################################
### code chunk number 31: over.Rnw:515-526
###################################################
if (require(rgeos, quietly = TRUE)) {
g = SpatialGrid(GridTopology(c(5,5), c(10,10), c(3,3)))
p = as(g, "SpatialPolygons")
p$z = c(1,0,1,0,1,0,1,0,1)
cc = coordinates(g)
p$ag1 = aggregate(p, p, mean)[[1]]
p$ag1a = aggregate(p, p, mean, minDimension = 0)[[1]]
p$ag2 = aggregate(p, p, mean, minDimension = 1)[[1]]
p$ag3 = aggregate(p, p, mean, minDimension = 2)[[1]]
p$ag4 = aggregate(p, p, areaWeighted=TRUE)[[1]]
}


###################################################
### code chunk number 32: over.Rnw:530-549
###################################################
if (require(rgeos, quietly = TRUE)) {
pts = cbind(c(9,21,21,9,9),c(9,9,21,21,9))
sq = SpatialPolygons(list(Polygons(list(Polygon(pts)), "ID")))
rnd2 = function(x) round(x, 2)
l = list(
	list("sp.text", cc, rnd2(p$z), which = 1),
	list("sp.text", cc, rnd2(p$ag1), which = 2),
	list("sp.text", cc, rnd2(p$ag1a), which = 3),
	list("sp.text", cc, rnd2(p$ag2), which = 4),
	list("sp.text", cc, rnd2(p$ag3), which = 5),
	list("sp.text", cc, rnd2(p$ag4), which = 6),
	list(sq, col = 'green', which = 6, first = FALSE, lwd = 2)
)
spplot(p, names.attr = c("source", "default aggregate", "minDimension=0", 
	"minDimension=1", "minDimension=2", "areaWeighted=TRUE"), layout = c(3,2), 
	as.table=TRUE, col.regions=bpy.colors(151)[50:151], cuts=100, 
	sp.layout = l, scales = list(draw = TRUE))
} else
	plot(1)


###################################################
### code chunk number 33: over.Rnw:566-574
###################################################
if (require(rgeos, quietly = TRUE)) {
round(c(
  aggDefault = aggregate(p, sq, mean)[[1]],
  aggMinDim0 = aggregate(p, sq, mean, minDimension = 0)[[1]],
  aggMinDim1 = aggregate(p, sq, mean, minDimension = 1)[[1]],
  aggMinDim2 = aggregate(p, sq, mean, minDimension = 2)[[1]],
  areaWeighted = aggregate(p, sq, areaWeighted=TRUE)[[1]]), 3)
}


