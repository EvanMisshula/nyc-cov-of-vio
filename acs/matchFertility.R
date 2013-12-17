matchFertility <- function(dbf,fertility)
    {
        youthMatch <- data.frame(dbf['BoroCT2010'],
                                 dbf['BoroCode'],
                                 dbf['CT2010'],
                                 wbirth12=rep(0,nrow(dbf)),
                                 birth12nm=rep(0,nrow(dbf)),
                                 wNB23mo=rep(0,nrow(dbf)),
                                 pctFert=rep(0,nrow(dbf)),
                                 pctUMFert=rep(0,nrow(dbf)),
                                 pctUMBirth=rep(0,nrow(dbf))
                                 )

        fertility$acs_tract_s <- levels(fertility$acs_tract)[fertility$acs_tract]

        fertility$BoroCode=-1
        fertility$BoroCode[fertility$acs_county==5] <- 2
        fertility$BoroCode[fertility$acs_county==47] <- 3
        fertility$BoroCode[fertility$acs_county==61] <- 1
        fertility$BoroCode[fertility$acs_county==81] <- 4
        fertility$BoroCode[fertility$acs_county==85] <- 5

        match=FALSE;
        k=0
        unmatchedTracts=matrix(-1,nrow = 10,ncol=3)
        for(i in 1:nrow(dbf)){
                for(j in 1:nrow(fertility)){
                    if( (as.numeric(youthMatch[['BoroCode']][i])==fertility[['BoroCode']][j]) && (youthMatch[['CT2010']][i]==fertility[['acs_tract_s']][j])){
                        match=TRUE
                        break
                    }
                }
                if(match==FALSE) {
                    cat("gone through all of fertility
no matching tract\ni=",i," j=",j,sep="")
                    unmatchedTracts[k+1,1] <- dbf$BoroCode[i]
                    unmatchedTracts[k+1,2] <- dbf$CT2010[i]
                    unmatchedTracts[k+1,3] <- i
                } else {
                    youthMatch$rowInYE[i]=j
                    youthMatch$wbirth12[i]=fertility$Women.who.had.a.birth.in.the.past.12.months..[j]
                    youthMatch$birth12nm[i]=fertility$Women.who.had.a.birth.in.the.past.12.months..Unmarried..never.married..widowed.and.divorced...[j]
                    youthMatch$wNB23mo[i]=fertility$Women.who.did.not.have.a.birth.in.the.past.12.months..[j]
                    youthMatch$pctFert[i]=fertility$pct.fertility[j]
                    youthMatch$pctUMFert[i]=fertility$pct.unmarried.fertility[j]
                    youthMatch$pctUMBirth[i]=fertility$pct.umarried.births[j]
                }
            }
    dbf3 <- data.frame(dbf[,1:10],
                       youthMatch$wbirth12,
                       youthMatch$birth12nm,
                       youthMatch$wNB23mo,
                       youthMatch$pctFert,
                       youthMatch$pctUMFert,
                       youthMatch$pctUMBirth,
                       dbf[,20:21])

        write.dbf(dataframe = dbf3,file = './fertShp/ct2010fs.dbf')
                       
      
