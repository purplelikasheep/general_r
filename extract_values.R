library('rgdal')
library('raster')
library('rgeos')
library('foreach')
library('doSNOW')




source('/home/rviegas/Dropbox/general_r/generics.R')



wd <- '/media/data/sao_domingos_de_prata'
path_shps <- '/media/data/suzando/ma5/shp/talhoes_points'
path_tifs <- '/media/data/cHLS/v2/noise/out/noise_mask_all'
path_tabelas <- '/media/data/cHLS/v2/noise/out/summary_csvs'

vars <- c(
    'noise'
)



setwd(wd)

shps <- get_names(path_shps, '\\.shp$')


tifs <- get_names(path_tifs, '\\.tif$')


i <- 6


# for ( i in 1:length(tifs$full_name)){
#
shp <- readOGR(path_shps,'LC08_23MKR-222063_20140808_cHLS', verbose=F)


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

    name_out <- paste(
        path_tabelas,
        tifs$basename_no_extension[i],
        sep='/'
    )

    name_out <- paste0(
        name_out,
        '.csv'
    )

    alreadydone <- get_names(path_tabelas, '\\.csv$')


    if (!(grepl(name_out, alreadydone))){

        # dummy_shp <- shps$basename_no_extension
        # dummy_tif <- tifs$basename_no_extension[i]
        #
        # ind  <- which(
        #     dummy_tif == dummy_shp
        # )
        # shp <- readOGR(path_shps, shps$basename_no_extension[ind])

        r <- stack(tifs$full_name[i])
        names(r) <- vars

        shp <- spTransform(shp, proj4string(r))
        ext <- extract(
            r,
            shp
        )

        a <- cbind.data.frame(
            shp@data,
            ext
        )


        write.csv(a, name_out)
        a
    }

}
stopCluster(cello)

# write.csv(l, 'tables/all.csv')
