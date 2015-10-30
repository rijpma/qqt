mfx <- function(fit, link){
      if (link=='logit') return(mean(dlogis(predict(fit, type='link'))) * coef(fit))
      if (link=='probit') return(mean(dnorm(predict(fit, type='link'))) * coef(fit))
}
vstrsplit <- function(strs, split){
    spltlist <- strsplit(strs, split)
    splittedstrs <- do.call(rbind, spltlist)
    return(splittedstrs)
}
reglist <- function(modellist, clustervar='hhid'){
    # creates list of models, standard errors, and p-values for texreg
    
    lgtmdls <- which(sapply(modellist, is, 'glm'))
    cftsts <- lapply(modellist, function(x) coeftest(x, vcov=vcovCL(x, clustervar)))

    se <- lapply(cftsts, function(x) x[, 2])
    pv <- lapply(cftsts, function(x) x[, 4])

    if (any(lgtmdls)){
        margfx <- logitmfxest2(modellist[[lgtmdls]], clustervar1='hhid', atmean=F)$mfx
        modellist$mfx <- modellist[[lgtmdls]]
        coefnames <- names(modellist$mfx$coefficients)
        modellist$mfx$coefficients <- margfx[coefnames, 1]
        se$mfx <- margfx[coefnames, 2]
        pv$mfx <- 2 * pt(-abs(margfx[coefnames, 1]/ margfx[coefnames, 2]), df = Inf)
        names(modellist$mfx$coefficients) <- coefnames
  }
  out <- list(models=modellist, se=se, pv=pv)

  return(out)
}

lowfreqind <- function(vrb, freq){
    lowfreq <- vrb %in% names(table(vrb)[table(vrb) < freq])
    return(lowfreq)
}

uniformise_string <- function(string, maxdist=0.2){
    str_srtd <- names(sort(-table(string)))
    n_start <- length(str_srtd)
    strmat <- stringdistmatrix(str_srtd, str_srtd, method='jw', p=0.1)
    fill <- NULL
    while(nrow(strmat) > 0){
        ind <- strmat[1, ] < maxdist
        similar_strs <- str_srtd[ind]
        str_srtd <- str_srtd[!ind]
        strmat <- strmat[!ind, !ind, drop=FALSE]
        string[string %in% similar_strs] <- similar_strs[1]
        if (length(similar_strs) > 1){
            cat(similar_strs, sep=', ')
            cat('----->')
            cat(similar_strs[1], '\n')
        }
    }
    cat('From', n_start, ' to ', length(unique(string)), '\n')
    return(string)
}

patr_transform <- function(x, positive=TRUE, absolute_min=NULL){
    if (positive) out <- round(10 * x / max(x, na.rm=T))
    if (!positive) out <- 10 - round(10 * x / max(x, na.rm=T))
    if (!is.null(absolute_min)){
        x[x < absolute_min] <- absolute_min
        out <- round(10 * (x - absolute_min) / (max(x, na.rm=TRUE) - absolute_min))
    } 
    return(out)
}

vcovCL <- function(fit, cluster1name, cluster2name=NULL){
    # there is also mfx:::clusterVCV but it requires a dataset + formula 
    # rather than a fitted model
    library(sandwich)
    cluster1 <- fit$model[, cluster1name]
    cluster2 <- fit$model[, cluster2name]
    cluster12 <- paste0(fit$model[, cluster1name], fit$model[, cluster2name])

    N <- length(cluster1)
    K <- fit$rank
    
    M1 <- length(unique(cluster1))
    dfc1 <- (M1 / (M1 - 1)) * ((N - 1) / (N - K))
    u1j <- apply(estfun(fit), 2, function(x) tapply(x, cluster1, sum))
    vc1 <- dfc1 * sandwich(fit, meat=crossprod(u1j) / N)
    out <- vc1
    if (!is.null(cluster2name)){
        M2 <- length(unique(cluster2))
        M12 <- length(unique(cluster12))
        dfc2 <- (M2 / (M2 - 1)) * ((N - 1) / (N - K))
        dfc12 <- (M12 / (M12 - 1)) * ((N - 1) / (N - K))
        u2j <- apply(estfun(fit), 2, function(x) tapply(x, cluster2, sum))
        u12j <- apply(estfun(fit), 2, function(x) tapply(x, cluster12, sum))
        vc2 <- dfc2 * sandwich(fit, meat=crossprod(u2j) / N)
        vc12 <- dfc12 * sandwich(fit, meat=crossprod(u12j) / N)
        out <- vc1 + vc2 - vc12
    }
    return(out)
}

logitmfxest2 <- function (fit, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
    clustervar2 = NULL, start = NULL, control = list()){
    # modification of mfx:::logitmfxest by Alan Fernihough
    # so that it will take fits with log() arguments etc.
    # replaces formula + data by fit and retrieve data with fit$model

    if (is.null(clustervar1) & !is.null(clustervar2)) {
        stop("use clustervar1 arguement before clustervar2 arguement")
    }
    if (!is.null(clustervar1)) {
        if (is.null(clustervar2)) {
            if (!(clustervar1 %in% names(fit$model))) {
                stop("clustervar1 not in data.frame object")
            }
        }
        if (!is.null(clustervar2)) {
            if (!(clustervar1 %in% names(fit$model))) {
                stop("clustervar1 not in data.frame object")
            }
            if (!(clustervar2 %in% names(fit$model))) {
                stop("clustervar2 not in data.frame object")
            }
        }
    }

    x1 = model.matrix(fit)
    if (any(alias <- is.na(coef(fit)))) {
        x1 <- x1[, !alias, drop = FALSE]
    }
    xm = as.matrix(colMeans(x1))
    be = as.matrix(na.omit(coef(fit)))
    k1 = length(na.omit(coef(fit)))
    xb = t(xm) %*% be
    fxb = ifelse(atmean == TRUE, plogis(xb) * (1 - plogis(xb)), 
        mean(plogis(x1 %*% be) * (1 - plogis(x1 %*% be))))
    vcv = vcov(fit)
    if (robust) {
        if (is.null(clustervar1)) {
            vcv = vcovHC(fit, type = "HC0")
        }
        else {
            if (is.null(clustervar2)) {
                vcv = vcovCL(fit, cluster1name = clustervar1, 
                  cluster2name = NULL)
            }
            else {
                vcv = vcovCL(fit, cluster1name = clustervar1, 
                  cluster2name = clustervar2)
            }
        }
    }
    if (robust == FALSE & is.null(clustervar1) == FALSE) {
        if (is.null(clustervar2)) {
            vcv = vcovCL(fit, cluster1name = clustervar1, 
                cluster2name = NULL)
        }
        else {
            vcv = vcovCL(fit, cluster1name = clustervar1, 
                cluster2name = clustervar2)
        }
    }
    mfx = data.frame(mfx = fxb * be, se = NA)
    if (atmean) {
        gr = (as.numeric(fxb)) * (diag(k1) + as.numeric(1 - 2 * 
            plogis(xb)) * (be %*% t(xm)))
        mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))
    }
    else {
        gr = apply(x1, 1, function(x) {
            as.numeric(as.numeric(plogis(x %*% be) * (1 - plogis(x %*% 
                be))) * (diag(k1) - (1 - 2 * as.numeric(plogis(x %*% 
                be))) * (be %*% t(x))))
        })
        gr = matrix(apply(gr, 1, mean), nrow = k1)
        mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))
    }
    temp1 = apply(x1, 2, function(x) length(table(x)) == 1)
    const = names(temp1[temp1 == TRUE])
    mfx = mfx[row.names(mfx) != const, ]
    temp1 = apply(x1, 2, function(x) length(table(x)) == 2)
    disch = names(temp1[temp1 == TRUE])
    if (length(disch) != 0) {
        for (i in 1:length(disch)) {
            if (atmean) {
                disx0 = disx1 = xm
                disx1[disch[i], ] = max(x1[, disch[i]])
                disx0[disch[i], ] = min(x1[, disch[i]])
                mfx[disch[i], 1] = plogis(t(be) %*% disx1) - 
                  plogis(t(be) %*% disx0)
                gr = dlogis(t(be) %*% disx1) %*% t(disx1) - dlogis(t(be) %*% 
                  disx0) %*% t(disx0)
                mfx[disch[i], 2] = sqrt(gr %*% vcv %*% t(gr))
            }
            else {
                disx0 = disx1 = x1
                disx1[, disch[i]] = max(x1[, disch[i]])
                disx0[, disch[i]] = min(x1[, disch[i]])
                mfx[disch[i], 1] = mean(plogis(disx1 %*% be) - 
                  plogis(disx0 %*% be))
                gr = as.numeric(dlogis(disx1 %*% be)) * disx1 - 
                  as.numeric(dlogis(disx0 %*% be)) * disx0
                avegr = as.matrix(colMeans(gr))
                mfx[disch[i], 2] = sqrt(t(avegr) %*% vcv %*% 
                  avegr)
            }
        }
    }
    mfx$discretechgvar = ifelse(rownames(mfx) %in% disch, 1, 
        0)
    output = list(fit = fit, mfx = mfx)
    return(output)
}


ciplot <- function(x, y, ci, ...){
    yrange <- c(min(y - 3*ci, na.rm=T), max(y + 3*ci, na.rm=T))
    yrange[yrange < 0] <- 0
    plot(x, y, lwd=2, ylim=yrange, ...)
    lines(x, y + 2*ci, ...)
    lines(x, y - 2*ci, ...)
}

panel_approx <- function(y, timevar, indexvar){
    # na.approx for panel data: 
    # does not interpolate between observations in two different countries etc.

    out <- y
    for (i in unique(indexvar)){
        if (sum(!is.na(y[indexvar==i])) > 1){
            out[indexvar==i] <- na.approx(object=y[indexvar==i], x=timevar[indexvar==i], na.rm=F)
        }
      }
  return(out)
}

grepr <- function(pattern, x, ...){
    idx <- grep(pattern, x, ...)
    return(x[idx])
}

regexprr <- function(pattern, x){
    rgx <- regexpr(pattern, x)
    out <- substr(x, rgx[1], rgx[1] + attr(rgx, 'match.length') - 1)
    return(out)
}

geocode <- function(loc, reg='', bounds=''){
    # barebones version of  geocode function on:
    # https://github.com/dkahle/ggmap

    require(jsonlite)
    if (length(loc) > 1) loc <- loc[1] # geocode api takes only one location
    loc <- loc
    base <- 'http://maps.googleapis.com/maps/api/geocode/json?address='
    request <- paste0(base, loc, '&region=', reg)
    request <- paste0(request, '&bounds=', bounds)
    request <- URLencode(request)
    result <- readLines(url(request))
    closeAllConnections()
    Sys.sleep(0.2) # max 5 calls per sec
    result <- paste(result, collapse='')
    result <- fromJSON(result)
    if (result$status != 'OK'){
        out <- data.frame(loc=loc, lat=NA, lon=NA, loc_frmtd=NA)
        return(out)
    } else {
        out <- data.frame(loc=loc,
                   lat=result$results$geometry$location$lat, 
                   lon=result$results$geometry$location$lng,
                   loc_frmtd=result$results$formatted_address,
                   stringsAsFactors=FALSE)
        return(out)
    }
}

factor2char <- function(dat){
    factors <- sapply(dat, class) == 'factor'
    dat[factors] <- sapply(dat[factors], as.character)
    return(dat)
}

makegq <- function(dat){
    # recreate group quarters variable from NAPP:
    # https://www.nappdata.org/napp-action/variables/GQ

    rltvs <- aggregate(persid ~ hhid, data=dat[dat$head | dat$son | dat$daughter | dat$ext | dat$spouse ,], length)
    entirehh <- aggregate(persid ~ hhid, data=dat, length)  
    out <- data.frame(hhid=dat$hhid, 
        all=entirehh$persid[match(dat$hhid, entirehh$hhid)], 
        relatives=rltvs$persid[match(dat$hhid, rltvs$hhid)], 
        stringsAsFactors=F)
    out$relatives[is.na(out$relatives)] <- 0

    if (!all.equal(dat$hhid, out$hhid)) stop('hhid order broken')

    out$nonrelatives <- out$all - out$relatives
    out$share_norelhead <- (out$nonrelatives) / (out$all)

    out$gq <- NA
    out$gq <- ifelse(out$nonrelatives >= 10 & out$share_norelhead <= 0.8, 5, out$gq)
    out$gq <- ifelse(out$nonrelatives >= 10 & out$share_norelhead > 0.8, 6, out$gq)
    out$gq <- ifelse(out$nonrelatives < 10, 2, out$gq)
    out$gq <- ifelse(out$nonrelatives < 5, 1, out$gq) 

    return(out$gq)
}

calcsumstats <- function(person, hh, hhidvar='hhid', sample){
    # calculate summary statistics for dataset

    hhid <- person[, hhidvar]
    person$person <- 1:nrow(person)
    out <- NULL

    out$nhh <- length(unique(hhid))
    out$hhsize <- nrow(person) / out$nhh

    out$sexratiototal <- sum(person$female, na.rm=T) / sum(person$male, na.rm=T)
    out$sexratio05 <- sum(person$female[person$age < 6], na.rm=T) / sum(person$male[person$age < 6], na.rm=T)
    out$pop20 <- sum(person$age < 20, na.rm=T) / sum(!is.na(person$age))
    out$meanage <- mean(person$age, na.rm=T)
    out$medianage <- quantile(person$age, 0.5, na.rm=T)

    out$fsmam <- calcsmam(person$age[person$female], !person$married[person$female])
    out$msmam <- calcsmam(person$age[person$male], !person$married[person$male])
    out$maalh <- calcsmam(person$age, person$son|person$daughter, maxage=29)
    # todo: check calculation maalh with e.g.
    # http://www.leeds.ac.uk/esrcfutureofwork/downloads/workingpaperdownloads/paper12.pdf
    out$sag <- out$msmam - out$fsmam

    children <- person[person$child, c(hhidvar, 'age')]
    children <- children[order(children$hhid, -children$age), ]
    children <- children[!duplicated(children$hhid), ]
    mothers <- person[person$spouse, c(hhidvar, 'age')]
    out$motheragefirstchild <- mean(mothers$age[match(children$hhid, mothers$hhid)] - children$age, na.rm=T)

    out$singles16plus <- sum(!person$married[!person$child], na.rm=T) / sum(!person$child, na.rm=T)
    nchild <- aggregate(person ~ hhid, data=person[person$son | person$daughter & !(is.na(person$daughter) | is.na(person$son)), ], length)$person
    out$nchild <- mean(nchild)
    out$enrol615 <- calcenrol615(person$enrol, person$age)
    out$ageheap <- calcwhipple(na.omit(person$age))
    out$literacy7plus <- sum(person$literate[person$age > 6], na.rm=T) / sum(!is.na(person$literate[person$age > 6]))

    # sumstats for Szoltisek and Gruber's patriarchy index
    out$femhhh <- sum(person$head & person$female & person$age >= 20, na.rm=T) / sum(person$head & person$age >= 20, na.rm=T)
    out$yngbrd <- sum(person$married & person$female & person$age >= 15 & person$age <= 19, na.rm=T) / sum(person$female & person$age >= 15 & person$age <= 19, na.rm=T)
    out$femnonkin <- sum(person$female[!(person$son|person$daughter|person$head|person$spouse|person$lateral|person$upward|person$ext) & person$age >= 20 & person$age <= 34], na.rm=T) / sum(person$female & person$age >= 20 & person$age <= 34, na.rm=T)
    husbands <- person[person$head & person$male, c('age', hhidvar)]
    spouses <- person[person$spouse & person$female, c('age', hhidvar)]
    agediffs <- husbands$age - spouses$age[match(husbands$hhid, spouses$hhid)]
    out$olderwives <- sum(agediffs < 0, na.rm=T) / sum(!is.na(agediffs))

    out$oldmen <- sum(person$male & !person$head & person$age >= 65, na.rm=T) / sum(person$male & person$age >= 65, na.rm=T)
    out$neolocals <- sum(person$male & person$head & person$age <= 29 & person$age >= 20, na.rm=T) / sum(person$male & person$age <= 29 & person$age >= 20, na.rm=T)
    marriedsons <- person$hhid[person$son & person$married] # more than one married son in hh
    out$oldjoints <- sum(person$age >= 65 & person$head & person$hhid %in% marriedsons[duplicated(marriedsons)], na.rm=T) / sum(person$age >= 65 & person$head, na.rm=T)
    laterals <- person$hhid[person$lateral]
    out$oldlats <- sum(person$age >= 65 & person$head & person$hhid %in% laterals, na.rm=T) / sum(person$age >= 65 & person$head, na.rm=T)
    # 1 for canada

    marrieddaughters <- person$hhid[person$daughter & person$married]
    out$mardghtr <- sum(person$age >=65 & person$hhid %in% marrieddaughters, na.rm=T) / sum( person$age >= 65, na.rm=T)

    person <- person[order(person[, hhidvar], -person$age), ]
    lastchld <- person[!duplicated(person[, hhidvar], fromLast=TRUE) & (person$son|person$daughter), c(hhidvar, 'male')]
    out$lastboy <- sum(lastchld$male, na.rm=T) / nrow(lastchld)
    out$sexratio04 <- sum(person$male[person$age <= 4], na.rm=T) / sum(person$female[person$age <= 4], na.rm=T)

    out$sample_children <- nrow(hh)
    hh <- hh[!duplicated(hh[, hhidvar]), ] # one row for each hh
    out$sample_households <- nrow(hh)

    out$sample_motheragefirstchild <- mean(hh$motheragefirstchild, na.rm=T)
    out$sample_latext <- sum(hh$nlat > 0, na.rm=T) / length(na.omit(hh$nlat))
    out$sample_upwext <- sum(hh$nupw > 0, na.rm=T) / length(na.omit(hh$nupw))
    out$sample_allextd <- sum(hh$nextd > 0, na.rm=T) / length(na.omit(hh$nextd))
    out$sample_servants <- sum(hh$servant, na.rm=T) / length(na.omit(hh$servant))
    out$sample_hhwithtwins <- sum(hh$twin, na.rm=T) / length(na.omit(hh$twin))

    out$religion <- max(table(person$religion)) / sum(table(person$religion))

    out <- lapply(out, round, 2)

    out$mainreligion <- names(which.max(table(person$religion)))
    out$schoolage <- paste(range(person$age[person$schoolage], na.rm=T), collapse='-')
    out$sample <- unique(person$smpl)
    out$year <- paste(range(person$year, na.rm=T), collapse='-')

    return(out)    
}

calcwhipple <- function(ages, n=5){
    ages <- ages[ages >=23 & ages <= 62]
    heapers <- ages %% n == 0
    whipple <- sum(heapers) / length(ages) * (n * 100)
    return(whipple)
}

calcenrol615 <- function(enrol, age){
    tab <- table(enrol[age > 5 & age < 16])
    return(unname(tab['TRUE'] / sum(tab)))
}

calcsmam <- function(ages, single, maxage=54, minage=15){
    keep <- complete.cases(ages, single)
    ages <- ages[keep]
    single <- single[keep]
    agescat <- trunc(ages / 5) * 5
    sp <- aggregate(single, by=list(ages=agescat), function(x) sum(x)/length(x))
    # minages <- min(sp$ages[sp$x < 0.99], 15) # occasional miscoding
    sp <- sp[sp$ages >= minage & sp$ages <= maxage, ]
    A <- minage + sum(sp$x[-length(sp$x)]) * 5
    N <- maxage - minage + 1
    B <- mean(sp$x[(nrow(sp) - 1):nrow(sp)])
    C <- 1 - B
    D <- max(sp$ages) * B
    return((A - D) / C)
}

hhaggregate <- function(dat, extravrbs=NULL){
    # adds household info to each row that is a child

    cat(dim(dat), 'original \n -> ')
    dat[, nlat := sum(lateral), by=hhid] # na.rm not necessary because grepl doesn't produce NAs
    dat[, nupw := sum(upward), by=hhid]
    dat[, nextd := sum(ext), by=hhid]
    dat[, nserv := sum(servant), by=hhid]

    setkey(dat, hhid)
    vrbs <- c('hhid', 'age', 'occupation', 'religion')
    vrbs <- c(vrbs, extravrbs)
    dat <- dat[dat[head==TRUE, vrbs, with=F]]
    setnames(dat, gsub('i\\.', 'father', names(dat)))
    dat <- dat[dat[spouse==TRUE, vrbs, with=F]]
    setnames(dat, gsub('i\\.', 'mother', names(dat)))

    dat <- dat[son | daughter & !is.na(son | daughter)]
    cat(dim(dat), 'only keep son/daughter \n -> ')

    dat[, nsib := sum(son|daughter, na.rm=T) - 1, by=hhid]
    dat[, nbro := sum(son, na.rm=T) - son, by=hhid]
    dat[, nsis := sum(daughter, na.rm=T) - daughter, by=hhid]
    dat[, twin := duplicated(age[son|daughter], incomparables=NA) 
                | duplicated(age[son|daughter], fromLast=T, incomparables=NA), by=hhid]
    dat[, hhtwin := any(twin), by=hhid]
    dat[, ageoldest := as.numeric(max(c(age[son|daughter], -1), na.rm=T)), by=hhid]
    dat[ageoldest < 0, ageoldest := NA] # fix -Inf return when all age are NA
    dat[, birthorder := rank(-age, ties.method='first', na.last='keep'), by=hhid]
    dat[, motheragefirstchild := motherage - ageoldest]
    dat[, spousagegap := fatherage - motherage]
    
    dat <- dat[motherage < 49 & motherage >= 18, ]
    cat(dim(dat), 'due to motherage \n -> ')
    dat <- dat[motheragefirstchild >= 14]
    cat(dim(dat), 'due to motheragefirstchild')

    return(dat)
}

rm_dualhh <- function(dat){
    # removes hh with more than one head or spouse

    duplhead <- dat$hhid[dat$head][duplicated(dat$hhid[dat$head])]
    duplspouse <- dat$hhid[dat$spouse][duplicated(dat$hhid[dat$spouse])]
    duplids <- unique(c(duplhead, duplspouse))
    cat(dim(dat), '->')
    if (length(duplids) > 0){
        dat <- dat[!dat$hhid %in% duplids, ]
    }
    cat(dim(dat))
    return(dat)
}

group_strings <- function(string){
    # tabulate group of strings based on jw string distance

    strtab <- sort(-table(string))
    strmat <- stringdistmatrix(names(strtab), names(strtab), method='jw', p=0.1)
    fill <- NULL
    while(nrow(strmat) > 0){
        ind <- strmat[1, ] < 0.2
        similar_strs <- strtab[ind]
        strtab <- strtab[!ind]
        strmat <- strmat[!ind, !ind, drop=FALSE]
        fill <- rbind(fill, data.frame(similar_strs))
        cat(dim(fill), '--')
    }
    clip <- pipe('pbcopy', 'w')
    write.table(fill, row.names=TRUE, quote=FALSE, file=clip)
    close(clip)
    return(fill)
}


## old stuff

# dotplot <- function(x, ses, labels, mar=c(4.5, 7.5, 3.5, 0.5), ...){
#   opar <- par(mar=mar)
#   # d <- d[order(x), ]
#   # xl = range(c(x + ses, x - ses), na.rm=T)
#   dotchart(x, pch=19, xlim=xl, ...)
#   # grid()
#   axis(side=2, seq_along(x), labels, las=2)
#   arrows((x - ses), 1:length(x), 
#          (x + ses), 1:length(x), angle=90, code=3, length=0.05)
#   par(opar)
# }




# logitmfx2 <- function (fit, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
#   clustervar2 = NULL, start = NULL, control = list()){
#     # if (is.null(formula)) {
#     #     stop("formula is missing")
#     # }
#     # if (!is.data.frame(data)) {
#     #     stop("data arguement must contain data.frame object")
#     # }
#     # if (is.null(clustervar1) & !is.null(clustervar2)) {
#     #     stop("use clustervar1 arguement before clustervar2 arguement")
#     # }
#     # if (!is.null(clustervar1)) {
#     #     if (is.null(clustervar2)) {
#     #         if (!(clustervar1 %in% names(data))) {
#     #             stop("clustervar1 not in data.frame object")
#     #         }
#     #         # data = data.frame(model.frame(formula, data, na.action = NULL), 
#     #         #     data[, clustervar1])
#     #         # names(data)[dim(data)[2]] = clustervar1
#     #         # data = na.omit(data)
#     #         # this is where it breaks, but maybe it works with update.formula
#     #         # or with names(data) <- c(names(data))
#     #         # but that would take the double log etc.
#     #         # easiest would be to just make inclusion of the cvar mandatory
#     #         # alternatively, have clustervars separate

#     #     }
#     #     if (!is.null(clustervar2)) {
#     #         if (!(clustervar1 %in% names(data))) {
#     #             stop("clustervar1 not in data.frame object")
#     #         }
#     #         if (!(clustervar2 %in% names(data))) {
#     #             stop("clustervar2 not in data.frame object")
#     #         }
#     #         # data = data.frame(model.frame(formula, data, na.action = NULL), 
#     #         #     data[, c(clustervar1, clustervar2)])
#     #         # names(data)[c(dim(data)[2] - 1):dim(data)[2]] = c(clustervar1, 
#     #         #     clustervar2)
#     #         # data = na.omit(data)
#     #     }
#     # }
#     # fit = glm(formula, data = data, family = binomial(link = "logit"), 
#     #     x = T, start = start, control = control)
#     x1 = model.matrix(fit)
#     if (any(alias <- is.na(coef(fit)))) {
#         x1 <- x1[, !alias, drop = FALSE]
#     }
#     xm = as.matrix(colMeans(x1))
#     be = as.matrix(na.omit(coef(fit)))
#     k1 = length(na.omit(coef(fit)))
#     xb = t(xm) %*% be
#     fxb = ifelse(atmean == TRUE, plogis(xb) * (1 - plogis(xb)), 
#         mean(plogis(x1 %*% be) * (1 - plogis(x1 %*% be))))
#     vcv = vcov(fit)
#     if (robust) {
#         if (is.null(clustervar1)) {
#             vcv = vcovHC(fit, type = "HC0")
#         }
#         else {
#             if (is.null(clustervar2)) {
#                 vcv = vcovCL(fit, cluster1name = clustervar1, 
#                                   cluster2name = NULL)
#             }
#             else {
#                 vcv = vcovCL(fit, cluster1name = clustervar1, 
#                                   cluster2name = clustervar2)
#             }
#         }
#     }
#     if (robust == FALSE & is.null(clustervar1) == FALSE) {
#         if (is.null(clustervar2)) {
#             vcv = vcovCL(fit, cluster1name = clustervar1, 
#                               cluster2name = NULL)
#         }
#         else {
#             vcv = vcovCL(fit, cluster1name = clustervar1, 
#                               cluster2name = clustervar2)
#         }
#     }
#     mfx = data.frame(mfx = fxb * be, se = NA)
#     if (atmean) {
#         gr = (as.numeric(fxb)) * (diag(k1) + as.numeric(1 - 2 * 
#             plogis(xb)) * (be %*% t(xm)))
#         mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))
#     }
#     else {
#         gr = apply(x1, 1, function(x) {
#             as.numeric(as.numeric(plogis(x %*% be) * (1 - plogis(x %*% 
#                 be))) * (diag(k1) - (1 - 2 * as.numeric(plogis(x %*% 
#                 be))) * (be %*% t(x))))
#         })
#         gr = matrix(apply(gr, 1, mean), nrow = k1)
#         mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))
#     }
#     temp1 = apply(x1, 2, function(x) length(table(x)) == 1)
#     const = names(temp1[temp1 == TRUE])
#     # mfx = mfx[row.names(mfx) != const, ]
#     temp1 = apply(x1, 2, function(x) length(table(x)) == 2)
#     disch = names(temp1[temp1 == TRUE])
#     if (length(disch) != 0) {
#         for (i in 1:length(disch)) {
#             if (atmean) {
#                 disx0 = disx1 = xm
#                 disx1[disch[i], ] = max(x1[, disch[i]])
#                 disx0[disch[i], ] = min(x1[, disch[i]])
#                 mfx[disch[i], 1] = plogis(t(be) %*% disx1) - 
#                   plogis(t(be) %*% disx0)
#                 gr = dlogis(t(be) %*% disx1) %*% t(disx1) - dlogis(t(be) %*% 
#                   disx0) %*% t(disx0)
#                 mfx[disch[i], 2] = sqrt(gr %*% vcv %*% t(gr))
#             }
#             else {
#                 disx0 = disx1 = x1
#                 disx1[, disch[i]] = max(x1[, disch[i]])
#                 disx0[, disch[i]] = min(x1[, disch[i]])
#                 mfx[disch[i], 1] = mean(plogis(disx1 %*% be) - 
#                   plogis(disx0 %*% be))
#                 gr = as.numeric(dlogis(disx1 %*% be)) * disx1 - 
#                   as.numeric(dlogis(disx0 %*% be)) * disx0
#                 avegr = as.matrix(colMeans(gr))
#                 mfx[disch[i], 2] = sqrt(t(avegr) %*% vcv %*% 
#                   avegr)
#             }
#         }
#     }
#     mfx$discretechgvar = ifelse(rownames(mfx) %in% disch, 1, 
#         0)
#     output = list(fit = fit, mfx = mfx)
#     return(output)
# }


### bin
# hhAggregate <- function(de, extravrbs=c()){
#   nsib <- aggregate(persid ~ hhid, data=de[de$son | de$daughter, ], length)
#   names(nsib) <- c('hhid', 'nsib')
#   hist(nsib$nsib, breaks=max(nsib$nsib) + 1)
#   nbro <- aggregate(persid ~ hhid, data=de[de$son, ], length)
#   names(nbro) <- c('hhid', 'nbro')
#   nsis <- aggregate(persid ~ hhid, data=de[de$daughter, ], length)
#   names(nsis) <- c('hhid', 'nsis')
#   nlat <- aggregate(persid ~ hhid, data=de[de$lateral, ], length)
#   names(nlat) <- c('hhid', 'nlat')
#   nupw <- aggregate(persid ~ hhid, data=de[de$upward, ], length)
#   names(nupw) <- c('hhid', 'nupw')
#   nextd <- aggregate(persid ~ hhid, data=de[de$ext, ], length)
#   names(nextd) <- c('hhid', 'nextd')
#   nserv <- aggregate(persid ~ hhid, data=de[de$servant, ], length)
#   names(nserv) <- c('hhid', 'nserv')
#   twin <- aggregate(age ~ hhid, data=de[de$son | de$daughter, ], function(x) any(duplicated(x)))
#   names(twin) <- c('hhid', 'twin')
#   ageoldestchild <- aggregate(age ~ hhid, data=de[de$son | de$daughter, ], max)
#   names(ageoldestchild) <- c('hhid', 'ageoldest')
#   vrbs <- c('hhid', 'age', 'occupation', 'religion')
#   vrbs <- c(vrbs, extravrbs)
#   fatherinfo <- de[de$head, vrbs]
#   names(fatherinfo)[-1] <- paste0('father', names(fatherinfo[-1]))
#   motherinfo <- de[de$spouse, vrbs]
#   names(motherinfo)[-1] <- paste0('mother', names(motherinfo[-1]))
#   # cull dupls because inst. can have > 1 head/wife -> data chaos

#   children <- de[(de$son | de$daughter) & !is.na(de$son | de$daughter), ]
#   children <- children[order(children$hhid, children$age), ]
#   bo <- tapply(-children$age, children$hhid, order)
#   children$birthorder <- unlist(bo)
#   children <- children[children$birthorder < 12, ]

#   x <- merge(children, nsib, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nbro, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nsis, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nlat, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nupw, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nextd, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, nserv, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, twin, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, ageoldestchild, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, motherinfo, by='hhid', all.x=T)
#   cat(dim(x), '- ')
#   x <- merge(x, fatherinfo, by='hhid', all.x=T)
#   cat(dim(x), '- ')

#   x <- x[x$motherage < 49 & x$motherage >= 18 & !is.na(x$motherage), ]
#   x$motheragefirstchild <- x$motherage - x$ageoldest
#   x <- x[x$motheragefirstchild >= 14 & !is.na(x$motheragefirstchild), ]

#   x$nsib <- x$nsib - 1
#   x$nserv[is.na(x$nserv)] <- 0
#   x$servant <- x$nserv > 0
#   x$twin[is.na(x$twin)] <- FALSE
#   x$nbro[is.na(x$nbro)] <- 0
#   x$nsis[is.na(x$nsis)] <- 0
#   x$nsis[is.na(x$nsis)] <- 0
#   x$nlat[is.na(x$nlat)] <- 0
#   x$nupw[is.na(x$nupw)] <- 0
#   x$nextd[is.na(x$nextd)] <- 0

#   x$spousagegap <- x$fatherage - x$motherage
#   x <- x[!is.na(x$enrol), ]
#   print(range(x$age, na.rm=TRUE))
#   print(dim(x))
#   return(x)
# }
# QQTPlot <- function(x, enrol=T, literate=F){
#   if (enrol){
#     par(mfrow=c(2, 3), mar=mar, font.main=1)
#     xa <- aggregate(enrol ~ nsib, data=x, mean)
#     xb <- aggregate(enrol ~ nbro, data=x, mean)
#     xs <- aggregate(enrol ~ nsis, data=x, mean)
#     xa <- xa[xa$nsib < 7, ]
#     xb <- xb[xb$nbro < 7, ]
#     xs <- xs[xs$nsis < 7, ]
#     yl <- range(c(xa$enrol, xb$enrol, xs$enrol))
#     plot(enrol ~ nsib, data=xa[xa$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. by number of siblings')
#     plot(enrol ~ nbro, data=xb[xb$nbro < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. by number of brothers')
#     plot(enrol ~ nsis, data=xs[xs$nsis < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. by number of sisters')

#     xa <- aggregate(enrol ~ nsib, data=x, mean)
#     xb <- aggregate(enrol ~ nsib, data=x[x$male, ], mean)
#     xs <- aggregate(enrol ~ nsib, data=x[x$female, ], mean)
#     xa <- xa[xa$nsib < 7, ]
#     xb <- xb[xb$nsib < 7, ]
#     xs <- xs[xs$nsib < 7, ]
#     yl <- range(c(xa$enrol, xb$enrol, xs$enrol))
#     plot(enrol ~ nsib, data=xa[xa$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. all by number of siblings')
#     plot(enrol ~ nsib, data=xb[xb$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. boys by number of siblings')
#     plot(enrol ~ nsib, data=xs[xs$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Enrol. girls by number of siblings')  
#   }
#   if (literate){
#     par(mfrow=c(2, 3), mar=mar, font.main=1)
#     xa <- aggregate(literate ~ nsib, data=x, mean)
#     xb <- aggregate(literate ~ nbro, data=x, mean)
#     xs <- aggregate(literate ~ nsis, data=x, mean)
#     xa <- xa[xa$nsib < 7, ]
#     xb <- xb[xb$nbro < 7, ]
#     xs <- xs[xs$nsis < 7, ]
#     yl <- range(c(xa$literate, xb$literate, xs$literate))
#     plot(literate ~ nsib, data=xa[xa$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) by number of siblings')
#     plot(literate ~ nbro, data=xb[xb$nbro < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) by number of brothers')
#     plot(literate ~ nsis, data=xs[xs$nsis < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) by number of sisters')

#     xa <- aggregate(literate ~ nsib, data=x, mean)
#     xb <- aggregate(literate ~ nsib, data=x[x$male, ], mean)
#     xs <- aggregate(literate ~ nsib, data=x[x$female, ], mean)
#     xa <- xa[xa$nsib < 7, ]
#     xb <- xb[xb$nsib < 7, ]
#     xs <- xs[xs$nsib < 7, ]
#     yl <- range(c(xa$literate, xb$literate, xs$literate))
#     plot(literate ~ nsib, data=xa[xa$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) all by number of siblings')
#     plot(literate ~ nsib, data=xb[xb$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) boys by number of siblings')
#     plot(literate ~ nsib, data=xs[xs$nsib < 12, ], bty='l', type='l', col=2, ylim=yl, main='Lit. (>7y) girls by number of siblings')  
#   }
# }

# vcovCL <- function(fit, cluster1name, cluster2name=NULL){
#     # there is also mfx:::clusterVCV but it requires a dataset rather than a fitted model
#     library(sandwich)
#     cluster1 <- fit$model[, cluster1name]
#     cluster2 <- fit$model[, cluster2name]
#     cluster12 <- paste0(fit$model[, cluster1name], fit$model[, cluster2name])

#     N <- length(cluster1)
#     K <- fit$rank
    
#     M1 <- length(unique(cluster1))
#     dfc1 <- (M1 / (M1 - 1)) * ((N - 1) / (N - K))
#     u1j <- apply(estfun(fit), 2, function(x) tapply(x, cluster1, sum))
#     vc1 <- dfc1 * sandwich(fit, meat=crossprod(u1j) / N)
#     out <- vc1
#     if (!is.null(cluster2name)){
#         M2 <- length(unique(cluster2))
#         M12 <- length(unique(cluster12))
#         dfc2 <- (M2 / (M2 - 1)) * ((N - 1) / (N - K))
#         dfc12 <- (M12 / (M12 - 1)) * ((N - 1) / (N - K))
#         u2j <- apply(estfun(fit), 2, function(x) tapply(x, cluster2, sum))
#         u12j <- apply(estfun(fit), 2, function(x) tapply(x, cluster12, sum))
#         vc2 <- dfc2 * sandwich(fit, meat=crossprod(u2j) / N)
#         vc12 <- dfc12 * sandwich(fit, meat=crossprod(u12j) / N)
#         out <- vc1 + vc2 - vc12
#     }
#     return(out)
# }
# NULLtoNA <- function(x){
#     if(is.null(x)) return(NA)
#     x
# }

