library('rgdal')
library('raster')
library('rgeos')
library('foreach')
library('doSNOW')




source('generics.R')



wd <- '/media/data/sao_domingos_de_prata'
path_shps <- 'shp/points'
path_tifs <- '/run/media/rviegas/My Passport/rafael/sao_domingos_de_prata/vars'
path_tabelas_randomForst <- 'tabelas_randomForest'

vars <- c(
    'blue_savg',
    'bndvi',
    'brightness',
    'ctvi',
    'eta',
    'fifth',
    'fourth',
    'gli',
    'green_savg',
    'h',
    'if',
    'nbr_diss',
    'nbr_dvar',
    'nbr_savg',
    'ndvi_savg',
    'nir',
    'nir_savg',
    'pndvi',
    'rdi',
    'red',
    'red_savg',
    'ri',
    'sixth',
    'swir1',
    'swir1_savg',
    'vari',
    'water'
)



setwd(wd)

shps <- get_names(path_shps, '\\.shp$')


tifs <- get_names(path_tifs, '\\.tif$')


i <- 6


# for ( i in 1:length(tifs$full_name)){
#
cello <- makeCluster(4, outfile="")
registerDoSNOW(cello)

l <- foreach(
    # i=1:2,
    i=1:length(tifs$full_name),
    .packages=c('rgeos', 'rgdal', "raster"),
    .inorder=F,
    .multicombine=T,
    .combine='rbind.data.frame') %dopar% {


    print(i)

    dummy_shp <- shps$basename_no_extension
    dummy_tif <- tifs$basename_no_extension[i]

    ind  <- which(
        dummy_tif == dummy_shp
    )
    shp <- readOGR(path_shps, shps$basename_no_extension[ind])

    r <- stack(tifs$full_name[i])
    names(r) <- vars

    ext <- extract(
        r,
        shp
    )

    a <- cbind.data.frame(
        shp@data,
        ext
    )

    name_out <- paste(
        path_tabelas_randomForst,
        tifs$basename_no_extension[i],
        sep='/'
    )

    write.csv(a, name_out)
    a


}
stopCluster(cello)

write.csv(l, 'tables/all.csv')
