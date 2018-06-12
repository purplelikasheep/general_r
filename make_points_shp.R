library(rgdal)
library(raster)
library(rgeos)
library(foreach)
library(doSNOW)
library(rgdal)

source('generics.R')


organaze_dt <- function(i, ext, r, id, id_imagem){

    xy <- xyFromCell(r, ext[[i]][,'cell'])
    cell <- ext[[i]][, 1]
    weight <- ext[[i]][, 3]

    ID <- id[i]

    a <- cbind.data.frame(
        ID,
        cell,
        weight,
        xy,
        id_imagem
    )

    return(a)

}


wd <- '/media/data/sao_domingos_de_prata'
setwd(wd)
shp <- readOGR('shp/plantios_sao_domingos_v8', 'plantios_sao_domingos_v8')
path_tifs <- '/run/media/rviegas/My Passport/rafael/sao_domingos_de_prata/grid'
tifs <- get_names(path_tifs, '\\.tif$')
dsn_out <- 'shp/points'

i <- 6


# for ( i in 1:length(tifs$full_name)){

cello <- makeCluster(4, outfile = "")
registerDoSNOW(cello)

l <- foreach(
    i=1:length(tifs$full_name),
    .errorhandling='stop',
    .packages = c('rgeos', 'rgdal',"raster"),
    .inorder=F,
    .multicombine=T,
    .combine='rbind.data.frame'
)%dopar%{

    id_imagem <- tifs$basename_no_extension[i]

    alreadydone <- list.files(dsn_out)
    alreadydone <- substr(alreadydone, 1, nchar(alreadydone)-4)
    alreadydone <- unique(alreadydone)

    if (!(any(grepl(id_imagem, alreadydone)))){


        print(i)

        r <- stack(tifs$full_name[i])
        r <- r[[1]]
        ext_r <- extent(r)
        ext_r <- as(ext_r, 'SpatialPolygons')
        ext_r@proj4string <-CRS( proj4string(r))
        s <- spTransform(shp,CRS( proj4string(r)) )

        ind <- gContains(ext_r,s, byid=T)
        shpinho <- s[as.vector(ind),]

        ext <- extract(
            r,
            shpinho,
            weights=T,
            normalizeWeights=F,
            cellnumbers=T

        )


        id <- shpinho@data$id

        dt <- lapply(1:length(ext), organaze_dt , ext, r, id, id_imagem)

        dt2 <- do.call(rbind.data.frame, dt)

        l2 <- dt2
        coordinates(l2) <- ~x+y
        proj4string(l2) <-  proj4string(r)
        writeOGR(l2,dsn_out , id_imagem, driver='ESRI Shapefile')

    } else {
        print('alreadydone')
    }


}
stopCluster(cello)

write.csv(l, 'a')


l2 <- l
coordinates(l2) <- ~x+y
proj4string(l2) <- proj4string(shp)
writeOGR(l2, 'shp', 'points_vol_teresina', driver='ESRI Shapefile')
