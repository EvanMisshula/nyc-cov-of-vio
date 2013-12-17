computePctUnmarriedMothersRct <- function(){
        # make the geometry ...counties
    bronx=geo.make(state="NY",county="Bronx",tract = "*")
    queens=geo.make(state="NY",county="Queens",tract = "*")
    manhattan=geo.make(state="NY",county="New York",tract = "*")
    si=geo.make(state="NY",county="Richmond",tract = "*")
    bk=geo.make(state="NY",county="Kings",tract = "*")
    #make nyc
    nyc=bronx+queens+manhattan+si+bk
    #get gendered employment -- pretty names or you're lost
    unmarriedBirths <- acs.fetch(geography = nyc,table.number = "B13014",col.names = "pretty")
    acs_county=unmarriedBirths@geography$county
    acs_tract=unmarriedBirths@geography$tract
    #select the columns we need
    relcolumns <- c(1, #ttl women 15 to 50
                    2, #ttl women 15 to 50 who gave birth in lst 12mo
                    9, #ttl women 15 to 50 who gave birth in lst 12mo unmarried
                    15 #ttl women 15 to 50 who did not give birth in lst 12mo
                    )
    unmarriedFertility <- data.frame(acs_county,acs_tract,unmarriedBirths@estimate[,relcolumns])

    # save the full names
    longNames <- colnames(unmarriedFertility)
    #kill the prefix
    shortNames <- gsub("WOMEN.15.TO.50.YEARS.WHO.HAD.A.BIRTH.IN.THE.PAST.12.MONTHS.BY.MARITAL.STATUS.AND.EDUCATIONAL.ATTAINMENT...","",longNames)
    colnames(unmarriedFertility) <- shortNames
    #save the long row names (geography == Bct2010)
    longCountyTract <- row.names(unmarriedFertility)
                                        #get rid of the row names
    unmarriedFertility <- data.frame(unmarriedFertility,row.names = NULL)
    unmarriedFertility$pct.fertility <- 100*(unmarriedFertility$'Women.who.had.a.birth.in.the.past.12.months..'/
                                             unmarriedFertility$'Total..')
    unmarriedFertility$pct.unmarried.fertility <- 100*(
                                             unmarriedFertility$'Women.who.had.a.birth.in.the.past.12.months..Unmarried..never.married..widowed.and.divorced...'/
                                             unmarriedFertility$'Total..')
    unmarriedFertility$pct.umarried.births <- 100*(
                                             unmarriedFertility$'Women.who.had.a.birth.in.the.past.12.months..Unmarried..never.married..widowed.and.divorced...'/
                                             unmarriedFertility$'Women.who.had.a.birth.in.the.past.12.months..')

    

}



