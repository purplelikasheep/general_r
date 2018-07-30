library(rgdal)
library(raster)
library(rgeos)
library(foreach)
library(doSNOW)
library(rgdal)



organaze_dt <- function(i, ext, r, id_shp, id_imagem){
    xy <- xyFromCell(r, ext[[i]][,'cell'])

    cell <- ext[[i]][, 1]
    weight <- ext[[i]][, 3]

    id <- id_shp[i,]
    a <- cbind.data.frame(
        id,
        cell,
        weight,
        xy,
        id_imagem,
        row.names=NULL
    )

    return(a)
}

get_alreadydone <- function(dsn){
        alreadydone <- list.files(dsn)
        # take extension out
        alreadydone <- substr(alreadydone, 1, nchar(alreadydone)-4)
        alreadydone <- unique(alreadydone)

        return(alreadydone)
}

get_extent_raster <- function(r0){
    r <- r0[[1]]
    ext_r <- extent(r)
    ext_r <- as(ext_r, 'SpatialPolygons')
    ext_r@proj4string <-CRS( proj4string(r))

    return(ext_r)
}

shape_proj <- function(raster, shape){
    prj_shape <- proj4string(shape)
    prj_raster <- proj4string(r)

    if (prj_shape != prj_raster){
        print('reprojecting shape to raster projection')
        print(paste('shape prj', prj_shape))
        print(paste('raster prj', prj_raster))
        s <- spTransform(shape, CRS(prj_raster))
    } else {
        print('both shape and raster have the same projection')
        s <- shape
    }

    return(s)
}

subset_shp_to_raster <- function(raster, shape){
    ext_r <- get_extent_raster(raster)
    ind <- gContains(ext_r, shape, byid=T)
    shpinho <- shape[as.vector(ind),]

    return(shpinho)
}

handle_shp <- function(raster, shape){
    shp <- shape_proj(r, shp)
    shpinho <- subset_shp_to_raster(raster, shp)

    return(shpinho)
}

get_info <- function(shpinho, r, id_imagem, id_field){
    ext <- extract(
        r[[1]],
        shpinho,
        weights=T,
        normalizeWeights=F,
        cellnumbers=T
    )
    id_shp <- shpinho@data[id_field]
    dt <- lapply(1:length(ext), organaze_dt , ext, r, id_shp, id_imagem)
    dt2 <- do.call(rbind.data.frame, dt)

    return(dt2)
}

l <- list(
    # path working directory
    wd='/media/data/suzando/ma5/shp',

    # shape info
    dsn_in='talhoes',
    lyr_in='test',
    id_field='UP',

    # tifs path
    path_tifs='/media/data/cHLS/v2/r5',

    # directory out
    dsn_out='talhoes_points',

    # path generics script
    path_generics='/home/rviegas/Dropbox/general_r/generics.R',

    # number of cores to be used
    n_cores=1
)

funcs <- c(
    'organaze_dt',
    'get_alreadydone',
    'get_extent_raster',
    'shape_proj',
    'subset_shp_to_raster',
    'handle_shp',
    'get_info'
)

make_points <- function(l){

    setwd(l[['wd']])
    source(l[['path_generics']])

    shp <- readOGR(l[['dsn_in']], l[['lyr_in']], verbose=F)
    tifs <- get_names(l[['path_tifs']], '\\.tif$')

    cello <- makeCluster(l[['n_cores']], outfile = "")
    registerDoSNOW(cello)

    l <- foreach(
        i=1:length(tifs$full_name),
        .errorhandling='stop',
        .packages = c('rgeos', 'rgdal',"raster"),
        .inorder=F,
        .multicombine=T,
        .export=funcs,
        .combine='rbind.data.frame'
    )%dopar%{

        id_imagem <- tifs$basename_no_extension[i]
        alreadydone <- get_alreadydone(l[['dsn_out']])

        if (!(any(grepl(id_imagem, alreadydone)))){
            print(i)

            r <- stack(tifs$full_name[i])
            shpinho <- handle_shp(r, shp)

            info <- get_info(shpinho, r, id_imagem, l[['id_field']])

            coordinates(info) <- ~x+y
            proj4string(info) <-  proj4string(r)
            writeOGR(
                info,
                dsn=l[['dsn_out']],
                layer=id_imagem,
                driver='ESRI Shapefile'
            )
        } else {
            print('alreadydone')
        }
    }

    stopCluster(cello)

}
