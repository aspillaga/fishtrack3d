## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">"
)

## ----plot_bathy, echo = TRUE, fig.align = "center", fig.width = 5.8, fig.height = 5, out.width = "70%", warnings = FALSE, messages = FALSE----
library(fishtrack3d)
library(raster)
library(plyr)
library(ks)

plot(bathymetry, col = terrain.colors(100))
points(receivers$long.utm, receivers$lat.utm, pch = 16, cex = 0.8)
text(receivers$long.utm, receivers$lat.utm, labels = receivers$id, pos = 4)

## ----download_data, eval = FALSE-----------------------------------------
#  path <- "https://github.com/aspillaga/fishtrack3d/raw/master/vignettes/data.zip"
#  
#  # Download and unzip the file in the working directory
#  download.file(path, destfile = "/data.zip")
#  unzip("./data.zip", exdir = "./data", junkpaths = TRUE)

## ----load_range-test-----------------------------------------------------
head(range_test)

## ----glm-----------------------------------------------------------------
range.mod <- glm(det.ratio ~ dist.m, data = range_test, 
                 family = quasibinomial(logit))

## ----glm_plot, fig.align = "center", fig.height = 4.2, fig.width = 5.9, out.width = "80%"----
plot(det.ratio ~ dist.m, data = range_test, xlim = c(0, 400),
     ylab = "Percentage of detections", 
     xlab = "Distance from receiver (m)")
lines(1:400, predict(range.mod, data.frame(dist.m = 1:400), type = "response"),
      type = "l", col = "firebrick", lwd = 2)

## ----dist_shadow, eval = FALSE-------------------------------------------
#  library(rgrass7)
#  
#  # In the bathymetry raster, NA values correspond to emerged areas.
#  # We will exaggerate the height of these areas to make sure
#  # everything behind them is removed in the viewshed analysis
#  elevation <- bathymetry
#  elevation[is.na(elevation)] <- 1e+10
#  elevation <- as(elevation, "SpatialPixelsDataFrame")
#  
#  
#  # Initialize GRASS session
#  initGRASS("/Applications/GRASS-7.0.app/Contents/MacOS/",
#            home = tempdir(), override = TRUE)
#  
#  # Load the elevation raster in GRASS
#  writeRAST(x = elevation, vname = "elevation", overwrite = TRUE)
#  
#  # Set the region for the analysis
#  execGRASS("g.region", parameters = list(raster = "elevation"))
#  
#  # Loop for each receiver
#  viewshed <- lapply(receivers$id, function(i) {
#  
#    # Coordinates of the receivers
#    coord <- as.numeric(receivers[receivers$id == i, 2:3])
#  
#    # Execute the 'viewshed' analysis
#    #=================================
#    execGRASS("r.viewshed", flags = c("overwrite", "b", "quiet"),
#                parameters = list(input = "elevation",
#                                  output = "viewshed",
#                                  coordinates = coord,
#                                  target_elevation = 500))
#  
#    # Export raster from GRASS and assign NA values and a projection
#    rast.tmp <- raster(readRAST("viewshed"))
#    proj4string(rast.tmp) <- proj4string(elevation)
#    rast.tmp[rast.tmp == 0 | is.na(bathymetry)] <- NA
#  
#  
#    # Calculate distances from receivers to each raster cell
#    #========================================================
#    distances <- sqrt((coordinates(rast.tmp)[, 1] - coord[1])^2 +
#                        (coordinates(rast.tmp)[, 2] - coord[2])^2)
#    rast.tmp[!is.na(rast.tmp)] <- distances[!is.na(values(rast.tmp))]
#  
#    return(rast.tmp)
#  })
#  
#  raster::names(viewshed) <- raster::validNames(receivers$id)
#  viewshed <- raster::stack(viewshed)

## ----viewshed_plot, echo = FALSE,  fig.align = "center", fig.height = 7.8, fig.width = 6----
library(raster)

# Plot the islands
islands <- bathymetry
values(islands) <- ifelse((is.na(values(bathymetry))), 1, NA)

par(mfrow = c(5, 4), mar = c(0.3, 0.3, 0.3, 0.3))

for (i in names(viewshed)) {
  image(viewshed[[i]], axes = FALSE, ann = FALSE, asp = 1)
  indx <- validNames(receivers$id) == i
  image(islands, col = tCol("black", 40), add = TRUE)
  points(receivers[indx, 2:3], pch = 16, cex = 2)
  text(receivers[indx, 2:3], labels = receivers$id[indx], pos = 4, cex = 2)
  bbox <- bbox(viewshed)
  rect(xleft = bbox[1, 1], xright = bbox[1, 2], ybottom = bbox[2, 1], 
       ytop = bbox[2, 2])
}

## ----data_load, eval = TRUE, warning = FALSE-----------------------------
head(tracking)

# We will split the data into a list to make it easier to apply the
# method to both individuals at the same time
tracking.list <- split(tracking, tracking$tag.id)

# Names of the individuals
names(tracking.list)

## ----30minTable, eval = TRUE, message = FALSE----------------------------
t30min <- lapply(tracking.list, thinData, time.int = "30min")

# We thin the data twice to later check the variability
t30min.2 <- lapply(tracking.list, thinData, time.int = "30min")

head(t30min[["dentex18"]])

head(t30min.2[["dentex18"]])

## ----spatialCronoPlot, eval = TRUE, echo = TRUE, message = FALSE, fig.align = "center", fig.width = 8.8, fig.height = 7, out.width="70%", dev="png"----
par(mfrow = c(2, 1), mar = c(3.1, 4.1, 3.1, 5.1))
for (i in names(tracking.list)) {
  t30.tmp <- list(t30min[[i]], t30min.2[[i]])
  for(x in 1:length(t30.tmp)) {
    spatChronPlot(time.stamp = t30.tmp[[x]]$time.stamp,
                  rec.id = factor(t30.tmp[[x]]$rec.id, levels = receivers$id))
    title(main = paste(i, "- Simulation", x))
  }
}

## ----depth, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 6.8, fig.height = 5, out.width="75%"----
par(mfrow = c(2, 1), mar = c(3.1, 4.6, 2.1, 2.1))
for (i in names(tracking.list)) {
  plot(t30min[[i]]$time.stamp, -t30min[[i]]$depth, type = "l",
       xlab = "Date", ylab = "Depth (m)", col = tCol("steelblue", 30), main = i)
  lines(t30min.2[[i]]$time.stamp, -t30min.2[[i]]$depth, 
        col = tCol("firebrick", 40))
  legend("bottomleft", legend = c("Simulation 1", "Simulation 2"), cex = 0.9,
         col = tCol(c("steelblue", "firebrick"), 30), lty = 1, bty = "n")
}

## ----depth_cost_matrix, echo = TRUE, eval = FALSE------------------------
#  # Create the list of transition matrices
#  depths <- 0:70 # Vector of minimum depths to compute the matrices
#  
#  depth_cost_list <- lapply(depths, function(x) {
#    leastCostMap(topo = bathymetry, min.depth = x)
#  })
#  
#  names(depth_cost_list) <- depths
#  
#  # Load the 'depth.cost.list' object from the downloaded files (OPTIONAL)
#  # load("./data/depth_cost_list.rda")

## ----path_reconstrunction, eval = FALSE, message = FALSE-----------------
#  synthetic.path <- lapply(t30min, function(data.frame) {
#  
#    # Remove receiver NAs and missing depths (if there are NAs).
#    # We will run the example with only the first 50 positions
#    df <- subset(data.frame, !is.na(depth))[1:50, ]
#  
#    path <- syntPath(track.data = df, topo = bathymetry, dist.rec = viewshed,
#                     ac.range.mod = range.mod, depth.cost.list = depth_cost_list,
#                     max.vel = 1, check = F)
#  })
#  
#  head(synthetic.path[[1]])

## ----get_reconstr_from_simu, eval = TRUE, include = FALSE----------------

# To avoid computing the simulations above, we take them directly from the
# 'synt_path_x100' object
load("./data/synt_path_x100.rda")
synthetic.path <- lapply(synt_path_x100, function(l) {
  return(l[[1]][1:50, ])
})

head(synthetic.path[[1]])

## ----path_plot2D, eval = TRUE, fig.align = "center", fig.width = 3.8, fig.height = 3.5, fig.show = "hold"----
# Plot in two dimensions
#========================
op <- par(mar = rep(0.2, 4))
image(bathymetry, col = terrain.colors(50), asp = 1, ann = FALSE, 
      axes = FALSE)
lines(synthetic.path[[1]][, c("x", "y")], col = "firebrick", lwd = 1.2)
lines(synthetic.path[[2]][, c("x", "y")], col = "dodgerblue", lwd = 1.2)
legend("topright", legend = names(synthetic.path), lty = 1, bg = "white",
       col = c("firebrick", "dodgerblue"))
par(op)

## ----path_plot3D, eval = FALSE-------------------------------------------
#  
#  # Plot in three dimensions (with the 'rgl' package)
#  #===================================================
#  library(rgl)
#  
#  # Create the matrix to plot the bathymetry
#  x <- unique(coordinates(bathymetry)[, 1])
#  y <- unique(coordinates(bathymetry)[, 2])
#  z <- matrix(values(bathymetry), ncol = length(x))
#  z[is.na(z)] <- 0
#  
#  # Prepare the color scale for bathymetry
#  zlim <- range(z, na.rm = TRUE)
#  zlen <- round(zlim[2] - zlim[1] + 1)
#  col <- c(terrain.colors(zlen)[z - zlim[1] + 1])
#  
#  # Set the display matrix (manually obtained to get the desired view)
#  mat <- matrix(c(0.48, 0.88, -0.06, 0.00, -0.30, 0.23, 0.93, 0.00,
#                  0.83, -0.42, 0.37, 0.00, 0.00, 0.00, 0.00, 1.00),
#                nrow = 4, byrow = TRUE)
#  
#  open3d(scale = c(1, 1, 10), windowRect = c(0, 0, 600, 400))
#  rgl.viewpoint(userMatrix = mat, zoom = 0.65, fov = 30)
#  rgl.pop("lights")
#  light3d(specular="black")
#  surface3d(x, y, z, col = col)
#  lines3d(x = synthetic.path[[1]]$x, y = synthetic.path[[1]]$y,
#          z = -synthetic.path[[1]]$depth, lwd = 2, col = "firebrick")
#  lines3d(x = synthetic.path[[2]]$x, y = synthetic.path[[2]]$y,
#          z = -synthetic.path[[2]]$depth, lwd = 2, col = "dodgerblue")

## ----3d_path_vis, eval = FALSE, echo = FALSE-----------------------------
#  rgl.snapshot("./3d_traj.png", fmt = "png", top = TRUE)

## ----simulations, eval = FALSE-------------------------------------------
#  # Set the parallel back end (only for UNIX systems)
#  library(doMC)
#  doMC::registerDoMC(cores = 18) # Set number of cores to parallelize
#  parallel = TRUE # Set to false if the parallel back end is not used
#  
#  synt_path_x100 <- lapply(tracking.list, function(df) {
#  
#    sim.ind <- plyr::llply(1:100, .parallel = parallel, function(x) {
#  
#      # Print individual and current simulation number
#      cat(df$tag.id[1], "synthetic track no.", x, "\n")
#  
#      t30min <- thinData(df, time.int = "30min", depth.range = c(0, 69))
#      t30min <- subset(t30min, !is.na(depth))
#  
#      synthetic.path <- syntPath(track.data = t30min, topo = bathymetry,
#                                 dist.rec = viewshed, ac.range.mod = range.mod,
#                                 depth.cost.list = depth_cost_list, max.vel = 1)
#      return(synthetic.path)
#    })
#  })

## ----voxelization, eval = FALSE------------------------------------------
#  rast3d.list <- lapply(synt_path_x100, function(s) {
#    voxelize(synt.list = s, raster = bathymetry, depth.int = 0:69, max.lag = 24)
#  })

## ----save_rast3d_list, eval = FALSE, echo = FALSE------------------------
#  # We can save the 'rast3d' object so we do not have to create it every time we
#  # compile the vignette
#  save(rast3d.list, file = "./data/rast3d_list.rda", compress = "xz")

## ----raster_plot, eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 6, fig.height = 3.5----
load("./data/rast3d_list.rda")

par(mfrow = c(2, 3), mar = c(0.1, 0.1, 1.4, 0.1), oma = c(1, 1, 1, 2))

l_ply(1:length(rast3d.list), function(r) {
  
  # We find the layer with the biggest probability
  depths <- names(rast3d.list[[r]])[which.max(cellStats(rast3d.list[[r]], 
                                                        sum)) - c(2, 0, -2)]
  lim <- range(values(rast3d.list[[r]][[depths]]))
  
  l_ply(depths, function(d) {
     legend <- ifelse(d == depths[length(depths)], TRUE, FALSE)
     plot(rast3d.list[[r]][[d]], zlim = lim, 
          main = paste(names(rast3d.list)[[r]], d, "m"), legend = legend, 
          axes = FALSE, box = FALSE)
     lim <- bbox(rast3d.list[[r]][[d]])
     rect(lim[1, 1], lim[2, 1], lim[1, 2], lim[2, 2])
  })
})

## ----kde, eval = FALSE---------------------------------------------------
#  library(ks)
#  
#  kde.list <- lapply(rast3d.list, function(r) {
#  
#    # Convert the RasterStack into a table
#    table <- as.data.frame(r, xy = TRUE)
#    table <- ldply(3:ncol(table), function(n) {
#      return(cbind(table[, 1:2], depth = colnames(table)[n], val = table[, n]))
#    })
#    table <- table[table$val > 0, ]
#    table$depth <- -as.numeric(substr(table$depth, start = 2, stop = 6))
#  
#    # Kernel density estimation
#    kde.tmp <- kde(x = table[, 1:3], w = table$val, compute.cont = FALSE)
#    return(kde.tmp)
#  })

## ----save_kde, eval = FALSE, echo = FALSE--------------------------------
#  # We can save the 'kde' object so we do not have to create it every time we
#  # compile the vignette
#  save(kde.list, file = "./data/kde.rda", compress = "xz")

## ----load_kde, eval = TRUE, echo = FALSE---------------------------------
load("./data/kde_list.rda")

## ----plot_kde, eval = FALSE, echo = TRUE---------------------------------
#  library(rgl)
#  
#  # Generate a matrix to plot the bathymetry
#  x <- unique(coordinates(bathymetry)[, 1])
#  y <- unique(coordinates(bathymetry)[, 2])
#  z <- t(as.matrix(bathymetry))
#  z[is.na(z)] <- 0
#  
#  # Set the color scale for depth
#  zlim <- range(z, na.rm = TRUE)
#  zlen <- round(zlim[2] - zlim[1] + 1)
#  col <- c(gray.colors(zlen)[z - zlim[1] + 1])
#  
#  # Set the display matrix
#  mat <- matrix(c(0.48, 0.88, -0.06, 0.00, -0.30, 0.23, 0.93, 0.00, 0.83, -0.42,
#                  0.37, 0.00, 0.00, 0.00, 0.00, 1.00), nrow = 4, byrow = TRUE)
#  
#  open3d(scale = c(1, 1, 10), windowRect = c(0, 0, 600, 400))
#  rgl.viewpoint(userMatrix = mat, zoom = 0.65, fov = 30)
#  
#  rgl.pop("lights")
#  light3d(specular="black")
#  surface3d(x, y, z, col = col)
#  
#  plot(kde.list[[1]], cont = c(25, 75), add = TRUE, axes = FALSE,
#       col.fun = topo.colors, box = FALSE)
#  plot(kde.list[[2]], cont = c(25, 75), add = TRUE, axes = FALSE,
#       col.fun = heat.colors, box = FALSE)

## ----save_kde_plot, echo = FALSE, eval = FALSE---------------------------
#  rgl.snapshot("./3d_contour.png", fmt = "png", top = TRUE)

## ----volumeUD, message = FALSE-------------------------------------------
# Compute UD volumes
ud.vol.list <- llply(kde.list, function(k) {
    rast.tmp <- predictKde(kde = k, raster = bathymetry, depths = 0.5:69.5)
    rast.tmp <- volumeUD(rast.tmp)
    return(rast.tmp)
})

## ----paraview, message = FALSE, eval = FALSE-----------------------------
#  # Tables to import into paraview
#  contour.table <- lapply(ud.vol.list, function(r) {
#  
#    # Convert the RasterStack into a table
#    cont.tab <- as.data.frame(r, xy = TRUE)
#    cont.tab <- ldply(3:ncol(cont.tab), function(n) {
#      return(cbind(cont.tab[, 1:2], depth = colnames(cont.tab)[n],
#                   vol = cont.tab[, n]))
#    })
#  
#    cont.tab$depth <- -as.numeric(substr(cont.tab$depth, start = 2, stop = 6))
#    return(cont.tab)
#  })
#  
#  # Save the tables to import in paraview
#  for (i in names(contour.table)) {
#    write.csv(contour.table[[i]], row.names = FALSE,
#              file = paste0("../", i, ".csv"))
#  }

## ----overlap, message = FALSE--------------------------------------------
overlap.50 <- overlap3d(ud.vol.list, level = 0.5, symmetric = FALSE)
overlap.95 <- overlap3d(ud.vol.list, level = 0.95, symmetric = FALSE)

## ----overlap50-----------------------------------------------------------
overlap.50

## ----overlap95-----------------------------------------------------------
overlap.95

