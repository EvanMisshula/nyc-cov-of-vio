matchYouthEmp <- function(dbf,youthEmployment)
    {
        youthMatch <- data.frame(dbf['BoroCT2010'],
                                 dbf['BoroCode'],
                                 dbf['CT2010'],
                                 rowInYE=rep(0,nrow(dbf)),
                                 pctIdl=rep(0,nrow(dbf)),
                                 ttlMYCt=rep(0,nrow(dbf)),
                                 ttlMYPct=rep(0,nrow(dbf)),
                                 tlUnPct=rep(0,nrow(dbf)),
                                 tOOLFPct=rep(0,nrow(dbf))
                                 )
        match=FALSE;
        k=0
        unmatchedTracts=matrix(-1,nrow = 10,ncol=3)
        for(i in 1:nrow(dbf)){
                for(j in 1:nrow(youthEmployment)){
                    if( (as.numeric(youthMatch[['BoroCode']][i])==youthEmployment[['BoroCode']][j]) && (youthMatch[['CT2010']][i]==youthEmployment[['acs_tract_s']][j])){
                        match=TRUE
                        break
                    }
                }
                if(match==FALSE) {
                    cat("gone through all of youthEmployment
no matching tract\ni=",i," j=",j,sep="")
                    unmatchedTracts[k+1,1] <- dbf$BoroCode[i]
                    unmatchedTracts[k+1,2] <- dbf$CT2010[i]
                    unmatchedTracts[k+1,3] <- i
                } else {
                    youthMatch$rowInYE[i]=j
                    youthMatch$pctIdl[i]=youthEmployment$youth.idle.pct[j]
                    youthMatch$ttlMYCt[i]=youthEmployment$Total.Male.Youth.Ct[j]
                    youthMatch$ttlMYPct[i]=youthEmployment$Total.Male.Youth.Pct[j]
                    youthMatch$tlUnPct[i]=youthEmployment$youth.unemployed.pct[j]
                    youthMatch$tOOLFPct[i]=youthEmployment$youth.Not.in.labor.force.pct[j]
                }
            }
