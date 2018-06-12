
get_names <- function(path, pattern){

    if(missing(pattern)) {
        full_names <- list.files(path, full.names=T)
    } else {
        full_names <- list.files(path, full.names=T, pattern=pattern)
    }

    basenames <- basename(full_names)

    basenames_no_extension <- strsplit(basenames, split='[.]')
    basenames_no_extension <- unlist(basenames_no_extension)
    basenames_no_extension <- matrix(basenames_no_extension, nrow=2)
    basenames_no_extension <- basenames_no_extension[1, ]

    dt <- data.frame(
        full_name=full_names,
        basename=basenames,
        basename_no_extension=basenames_no_extension,
        stringsAsFactors=F
    )

    return(dt)

}


asdate <- function(x, fmt){
    dt <- as.Date(
        as.character(x),
        format=fmt
    )
    return(dt)
}

spatialFilter <- function(r, fileOut, agricKind){

    size <- 5
	w <- matrix(data=1, ncol=size, nrow=size)
	r <- focal(r, w=w, fun = max, na.rm=T, pad = T, padValue = 1)
	r <- reclassify(r, c(-Inf, 15, 0, 15, Inf, agricKind), filename = fileOut, format = 'GTiff')
	return(r)

}
