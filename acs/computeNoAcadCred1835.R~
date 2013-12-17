computeAcadCred <- function() {
            # make the geometry ...counties
    bronx=geo.make(state="NY",county="Bronx",tract = "*")
    queens=geo.make(state="NY",county="Queens",tract = "*")
    manhattan=geo.make(state="NY",county="New York",tract = "*")
    si=geo.make(state="NY",county="Richmond",tract = "*")
    bk=geo.make(state="NY",county="Kings",tract = "*")
    #make nyc
    nyc=bronx+queens+manhattan+si+bk
    #get gendered education -- pretty names or you're lost
    edu <- acs.fetch(geography = nyc,table.number = "B15001",col.names = "pretty")
    acs_county=unmarriedBirths@geography$county
    acs_tract=unmarriedBirths@geography$tract
    #select the columns we need
    relcolumns <- c(1, #ttl over 18
                    2, #ttl over 18 men
                    3, #ttl over 18 to 24 men
                    4, #ttl over 18 to 24 men: Less than 9th grade
                    5, #ttl over 18 to 24 men: 9th to 12th grade, no diploma
                    6, #ttl over 18 to 24 men : High school graduate, GED, or alternative
                    7 #ttl over 18 to 24 men: Some college, no degree
                    )

    shortNames <- c("acs_county",
                    "acs_tract",
                    "MFTot",
                    "A18OMTot",
                    "A1824MTot",
                    "A1824Mlt9thG",
                    "A1824M9to12NoDip",
                    "A1824MHSGrad",
                    "A1824MSColND"
                   )

    noEdCred <- data.frame(acs_county,acs_tract,edu@estimate[,relcolumns])

        # save the full names
    longNames <- colnames(noEdCred)
    # colnames

    colnames(noEdCred) <- shortNames
    longCountyTract <- row.names(noEdCred)
                                        #get rid of the row names
    noEdCred <- data.frame(noEdCred,row.names = NULL)

    noEdCred$total.noEdCred <- noEdCred$A1824Mlt9thG+
                   noEdCred$A1824M9to12NoDip+
                   noEdCred$A1824MHSGrad+
                   noEdCred$A1824MSColND

    noEdCred$pct.male.youth <-100*(noEdCred$A18OMTot/noEdCred$MFTot)
    
    noEdCred$pct.noEdCred.male <- 100*(noEdCred$total.noEdCred/noEdCred$A18OMTot)

    noEdCred$pct.noEdCred.male.youth <- 100*(noEdCred$total.noEdCred/noEdCred$A1824MTot)
}
