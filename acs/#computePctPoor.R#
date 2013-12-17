computePctPoor <- function() {
    # make the geometry ...counties
    bronx=geo.make(state="NY",county="Bronx",tract = "*")
    queens=geo.make(state="NY",county="Queens",tract = "*")
    manhattan=geo.make(state="NY",county="New York",tract = "*")
    si=geo.make(state="NY",county="Richmond",tract = "*")
    bk=geo.make(state="NY",county="Kings",tract = "*")
    #make nyc
    nyc=bronx+queens+manhattan+si+bk
    #get gendered employment -- pretty names or you're lost
    poor_education_gender <- acs.fetch(geography = nyc,table.number = "B17003",col.names = "pretty")
    acs_county=poor_education_gender@geography$county
    #select the columns we need
    relcolumns <- c(1,2)
    acs_tract=poor_education_gender@geography$tract
    
    # create a regular data frame from the estimates
    poverty <- data.frame(acs_county,acs_tract,poor_education_gender@estimate[,relcolumns])

    shortNames <- c("acs_county",
                   "acs_TRACT",
                   "Total.People",
                   "People.in.poverty"
                   )
        # save the full names
    longNames <- colnames(poverty)
    colnames(poverty) <- shortNames
    #save the long row names (geography == Bct2010)
    longCountyTract <- row.names(poverty)
                                        #get rid of the row names
    poverty <- data.frame(poverty,row.names = NULL)

    poverty$pct.in.poverty <- 100*(poverty$People.in.poverty/
                                   poverty$Total.people)

    poverty$pct.in.poverty[is.nan(poverty$pct.in.poverty)] <- 0
    
}
