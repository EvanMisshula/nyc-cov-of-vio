matchPoverty <- function(dbf,poverty)
    {
        youthMatch <- data.frame(dbf['BoroCT2010'],
                                 dbf['BoroCode'],
                                 dbf['CT2010'],
                                 ttlPpl=rep(0,nrow(dbf)),
                                 pplPov=rep(0,nrow(dbf)),
                                 pctPov=rep(0,nrow(dbf))
                                 )

        poverty$acs_tract_s <- levels(poverty$acs_TRACT)[poverty$acs_TRACT]

        poverty$BoroCode=-1
        poverty$BoroCode[poverty$acs_county==5] <- 2
        poverty$BoroCode[poverty$acs_county==47] <- 3
        poverty$BoroCode[poverty$acs_county==61] <- 1
        poverty$BoroCode[poverty$acs_county==81] <- 4
        poverty$BoroCode[poverty$acs_county==85]g <- 5

        match=FALSE;
        k=0
        unmatchedTracts=matrix(-1,nrow = 10,ncol=3)
        for(i in 1:nrow(dbf)){
                for(j in 1:nrow(poverty)){
                    if( (as.numeric(youthMatch[['BoroCode']][i])==poverty[['BoroCode']][j]) && (youthMatch[['CT2010']][i]==poverty[['acs_tract_s']][j])){
                        match=TRUE
                        break
                    }
                }
                if(match==FALSE) {
                    cat("gone through all of poverty
no matching tract\ni=",i," j=",j,sep="")
                    unmatchedTracts[k+1,1] <- dbf$BoroCode[i]
                    unmatchedTracts[k+1,2] <- dbf$CT2010[i]
                    unmatchedTracts[k+1,3] <- i
                } else {
                    youthMatch$rowInYE[i]=j
                    youthMatch$ttlPpl[i]=poverty$Total.People[j]
                    youthMatch$pplPov[i]=poverty$People.in.poverty[j]
                    youthMatch$pctPov[i]=poverty$pct.in.poverty[j]
                }
            }
    dbf2 <- data.frame(dbf[,1:10],youthMatch$ttlPpl,
                       youthMatch$pplPov,
                       youthMatch$pctPov,dbf[,20:21])

        write.dbf(dataframe = dbf2,file = './povShp/ct2010fs.dbf')
                       
      
