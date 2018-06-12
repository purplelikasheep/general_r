


library('rgdal')
library('raster')
library('rgeos')
library('foreach')
library('doSNOW')
library(rgdal)

#load some functions
source('generics.R')

# usando os ids que o fabio selecionou como referencia
# levando os ids do fabio para os meus ids
load('/media/data/sao_domingos_de_prata/resf.RData')
f <- function(a){
    d <- strsplit(a, '_')[[1]]
    d[3] <- paste0(d[3], '-217074')
    d[4] <- paste0(d[4], '_cHLS_desiredVars')
    d2 <-paste0(d[2:4],collapse='_')
    d <- paste(d[1], d2, sep='-')
    return(d)
}
resf$key <-unlist(lapply(resf$key, f))


# funcao que gera o grid e salva na pasta indicada como shp
make_grid <- function(j, shpinho, r, pts, id_image, alreadydone){
    new_ids <- c(
        "11558-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "11923-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "11924-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "12063-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "100001-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100002-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100003-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100004-S02B_23KQT-217074_20170723_cHLS_desiredVars"
       )
    print(j)

    up <- shpinho[j,]
    id_up <- up@data$id
    name_grid0 <- paste(
        floor(id_up),
        id_image,
        sep = '-'
    )
    id_up <- as.character(id_up)
    name_grid <- paste(
        id_up,
        id_image,
        sep = '-'
    )

    name_grid <- gsub("[.]", "_", name_grid)

    if (any(grepl(name_grid0, c(resf$key, new_ids)))){
        if (!(any(grepl(name_grid, alreadydone)))){

            ext_up <- extent(up) + rep(c(-80, 80), 2)

            r_up <- crop(r, ext_up)
            values(r_up) <- NA

            pts_up <- pts@data[,1] == id_up
            pts_up <- subset(pts, pts_up)

            cells <- cellFromXY(r_up, coordinates(pts_up))

            r_up[cells] <- pts_up@data$cell

            grid_up <- rasterToPolygons(r_up)
            names(grid_up) <- 'cell'


            writeOGR(grid_up, dsn_out, name_grid, driver='ESRI Shapefile')
            out <- name_grid
        } else {
            print('id alreadydone')
            out <- 'id alreadydone'
        }
    }else{
        print('not to do')
        out <- 'not to do'

    }
    return(out)
}

get_alreadydone <- function(){
    alreadydone <- c(
        list.files('grids_v4_5', full.names=T),
        list.files('grids_v4_4', full.names=T),
        list.files('grids_v4_3', full.names=T),
        list.files('grids_v4_2', full.names=T),
        list.files('grids_v4_1', full.names=T)
    )
    alreadydone <- substr(alreadydone, 1, nchar(alreadydone)-4)
    alreadydone <- unique(alreadydone)
    print(length(alreadydone))

    return(alreadydone)
}

#paramentros de entrada
wd <- '/media/data/sao_domingos_de_prata'
dsn_ups <- 'shp/plantios_sao_domingos_v8'
lyr_ups <- 'plantios_sao_domingos_v8'
path_tifs <- '/run/media/rviegas/My Passport/rafael/sao_domingos_de_prata/grid'
dsn_pts <- 'shp/points'
dsn_out <- 'grids_v4_5'


setwd(wd)

shp_ups <- readOGR(dsn_ups, lyr_ups)

# path_tifs <- path_tifs
tifs <- get_names(path_tifs, '\\.tif$')#[c(2, 4, 7, 10, 13, 17, 19), ]
tifs$key <- substr(tifs$basename_no_extension, 1, 26)

#points shps by raster
path_shps <- dsn_pts
shps_pts <- get_names(path_shps, '\\.shp$')

# do_not_path <- '/media/data/sao_domingos_de_prata/shp/shp_com_nuvem_mg_v2'
# l <- get_names(do_not_path, '.shp')
# dummy <- matrix(unlist(strsplit(l$basename_no_extension, '_')), nrow=5)[1:3,]
# l$key <- apply(dummy, 2, paste0, collapse='_')
#
# i <- 1
#
# for (i in 1:length(do_not)){
#
#     ndn <- names(do_not)[i]
#     sat <- substr(ndn,3, 4)
#     date <- substr(ndn,19, 28)
#     tile <- substr(ndn,6, 10)
#     sat <- paste0('S', sat)
#     tile <- paste0('T', tile)
#     n <- paste(sat, date, tile, sep ='_')
#     ind <- n == l$key
#     if (any(ind)){
#         dummy <- readOGR(do_not_path, l$basename_no_extension[n == l$key], verbose=F)
#         do_not[[ndn]] <- as.character(dummy@data$id)
#     }
#
# }





i <- 1
for ( i in 1:length(tifs$full_name)){
    print(i)

    # pegando limite dos talhoes
    r <- stack(tifs$full_name[i])
    r <- r[[1]]
    ext_r <- extent(r)
    ext_r <- as(ext_r, 'SpatialPolygons')
    ext_r@proj4string <-CRS( proj4string(r))
    ind <- gContains(ext_r,shp_ups, byid=T)
    shpinho <- shp_ups[as.vector(ind),]

    # pegando pontos para a imagem em questao
    dummy_shp <- shps_pts$basename_no_extension
    dummy_tif <- tifs$basename_no_extension[i]
    ind  <- which(
        dummy_tif == dummy_shp
    )
    pts <- readOGR(
        path_shps, shps_pts$basename_no_extension[ind],
        verbose=F
    )

    #pegando quais arquivos ja foram feitos
    alreadydone <- get_alreadydone()

    id_image <- tifs$basename_no_extension[i]

    cello <- makeCluster(4, outfile = "")
    registerDoSNOW(cello)

    l <- foreach(
        j=1:length(shpinho),
        .errorhandling='stop',
        .packages = c('rgeos', 'rgdal',"raster"),
        .inorder=F,
        .multicombine=T,
        .combine='rbind.data.frame'
    )%dopar%{

        out <- make_grid(j, shpinho, r, pts, id_image, alreadydone)
    # l <- lapply(6900:length(shpinho), make_grid,
    #      shpinho, r, pts, id_image, alreadydone)
    print(out)

    }
    stopCluster(cello)


}

alreadydone <- get_alreadydone()
a <- matrix(unlist(strsplit(alreadydone, '-')), nrow=3)[1,]
a <- matrix(unlist(strsplit(a, '/')), nrow=2)[2,]
a <- gsub("_", ".", a)
ids <- as.character(shp_ups@data$id)
to_do <- ids[!(ids %in% a)]
print(paste('a fazer', length(to_do)))

b <- matrix(unlist(strsplit(resf$key, '-')), nrow=3)[1,]
c <- as.character(floor(as.numeric(ids)))
not_resf <- c[!(c %in% b)]

new_ids <- c(
        "11558-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "11923-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "11924-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "12063-S02A_23KQT-217074_20160713_cHLS_desiredVars",
        "100001-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100002-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100003-S02B_23KQT-217074_20170723_cHLS_desiredVars",
        "100004-S02B_23KQT-217074_20170723_cHLS_desiredVars"
       )
to_do <- c(resf$key, new_ids)

alreadydone <- get_alreadydone()
a <- matrix(unlist(strsplit(alreadydone, '-')), nrow=3)

ids <- matrix(unlist(strsplit(a[1,], '/')), nrow=2)[2,]
ids <- gsub("_", ".", ids)

fids <- as.character(floor(as.numeric(ids)))

image <- paste(a[2,], a[3,], sep='-')

key <- paste(fids, image, sep='-')


t <- cbind(ids, fids, alreadydone, image, key)

overdid <- t[!(t[,5] %in% to_do),]
fin <- unlist(lapply(c('.shp', '.dbf', '.prj', '.shx'),function(x)paste0(overdid[,3], x)))



# fin <- unlist(lapply(c('.shp', '.dbf', '.prj', '.shx'),function(x)paste0(dsn_out, '/',wanted, x)))
# fout <- '/run/media/rviegas/My Passport/rafael/grids_sddp'
# file.copy(fin, fout)
#
#
#
#
# l0 <- c( list.files('grids_v4_1', full.names=T),
#  # list.files('grids_v32', full.names=T),
#  #  list.files('grids_v33', full.names=T),
#  #   list.files('grids_v34', full.names=T),
#  #    list.files('grids_v35', full.names=T)
# )
# l0 <- substr(l0, 1, nchar(l0)-4)
# l0 <- unique(l0)
# l <-  matrix(unlist(strsplit(l0, split='/')), nrow=2)[2,]
# u <- l0[!(l %in% resf$key)]
# v <- unlist(lapply(c('.shp', '.dbf', '.prj', '.shx'),function(x)paste0(u, x)))
#
#
#
# date <- matrix(unlist(strsplit(l, split='_')), nrow=6)[4,]
# date <- as.Date(date, format='%Y%m%d')
# id <- matrix(unlist(strsplit(l, split='-')), nrow=3)[1,]
# id <-  matrix(unlist(strsplit(id, split='/')), nrow=2)[2,]
# t <- cbind(l, id, date)
#
# ids <- unique(id)
#
# for ( i in 1:length(ids)){
#
#     d <- t[t[,2]==ids[i],]
#         if (sum(t[,2]==ids[i])>1) stop('pao')
#         # d <- d[!(d[,3]==max(d[,3])),]
#
# }
#
#
#
#
# #
# # alreadydone <- c(list.files('shp/grids'),list.files('shp/grids2'))
# # a <- matrix(unlist(strsplit(alreadydone, '-')), nrow=3)
# #
# # for (i in 1:13907){
# #     if (!(i %in% a)){
# #         print(i)
# #     }
# # }
