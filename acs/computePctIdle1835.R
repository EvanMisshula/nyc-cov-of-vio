computePctIdle1835 <- function() {
    # make the geometry ...counties
    bronx=geo.make(state="NY",county="Bronx",tract = "*")
    queens=geo.make(state="NY",county="Queens",tract = "*")
    manhattan=geo.make(state="NY",county="New York",tract = "*")
    si=geo.make(state="NY",county="Richmond",tract = "*")
    bk=geo.make(state="NY",county="Kings",tract = "*")
    #make nyc
    nyc=bronx+queens+manhattan+si+bk
    #get gendered employment -- pretty names or you're lost
    age_sex_gt_16 <- acs.fetch(geography = nyc,table.number = "B23001",col.names = "pretty")
    acs_county=age_sex_gt_16@geography$county
    #select the columns we need
    relcolumns <- c(1,#total 16+
                    2,#and male
                    3,# only 16-19
                    4,# 16-19 in labor force
                    6,# 16-19 in labor force civilian
                    8,# 16-19 in labor force civilian, unemployed
                    9,# 16-19 in labor force civilian, out of labor force
                    10, #only 20-21 male                                            
                    11, #only 20-21 male in labor force                             
                    13, #only 20-21 male in labor force civilian                    
                    15, #only 20-21 male in labor force civilian, unemployed        
                    16, #only 20-21 male in labor force civilian, out of labor force
                    17, #only 22-24 male                                            
                    18, #only 22-24 male in labor force                             
                    20, #only 22-24 male in labor force civilian                    
                    22, #only 22-24 male in labor force civilian, unemployed        
                    23, #only 22-24 male in labor force civilian, out of labor force
                    24, #only 25-29 male                                            
                    25, #only 25-29 male in labor force                             
                    27, #only 25-29 male in labor force civilian                    
                    29, #only 25-29 male in labor force civilian, unemployed        
                    30, #only 25-29 male in labor force civilian, out of labor force
                    31, #only 30-34 male                                            
                    32, #only 30-34 male in labor force                             
                    34, #only 30-34 male in labor force civilian                    
                    36, #only 30-34 male in labor force civilian, unemployed        
                    37  #only 30-34 male in labor force civilian, out of labor force
                    )
    acs_tract=age_sex_gt_16@geography$tract
    
    # create a regular data frame from the estimates
    youthEmployment <- data.frame(acs_county,acs_tract,age_sex_gt_16@estimate[,relcolumns])

    # save the full names
    longNames <- colnames(youthEmployment)
    #now that we know them shorten as much as possible
    shortNames <- gsub("Sex.by.Age.by.Employment.Status.for.the.Population.16.Years.and.over...","",longNames)
    colnames(youthEmployment) <- shortNames
    #save the long row names (geography == Bct2010)
    longCountyTract <- row.names(youthEmployment)
                                        #get rid of the row names
    youthEmployment <- data.frame(youthEmployment,row.names = NULL)
                                        #at risk youth count
    #compute the total at-risk youth count
    youthEmployment$Total.Male.Youth.Ct <- apply(youthEmployment[,c(5,10,15,20,25)],1,sum)

    #male youth as a pct of labor force
    youthEmployment$Total.Male.Youth.Pct <- 100*youthEmployment$Total.Male.Youth.Ct/youthEmployment$Total..

    #youth unemployment
    youthEmployment$youth.unemployed.pct <- 100*(youthEmployment$Male..16.to.19.years..In.labor.force..Civilian..Unemployed.
                                                 +youthEmployment$Male..20.and.21.years..In.labor.force..Civilian..Unemployed.
                                                 +youthEmployment$Male..22.to.24.years..In.labor.force..Civilian..Unemployed.
                                                 +youthEmployment$Male..25.to.29.years..In.labor.force..Civilian..Unemployed.
                                                 +youthEmployment$Male..30.to.34.years..In.labor.force..Civilian..Unemployed.)/
                                                     (youthEmployment$Male..16.to.19.years..+
                                                      youthEmployment$Male..20.and.21.years..+
                                                      youthEmployment$Male..22.to.24.years..+
                                                      youthEmployment$Male..25.to.29.years..+
                                                      youthEmployment$Male..30.to.34.years..
                                                      )

    #youth out of labor force
    youthEmployment$youth.Not.in.labor.force.pct <- 100*(youthEmployment$Male..16.to.19.years..Not.in.labor.force.
                                                 +youthEmployment$Male..20.and.21.years..Not.in.labor.force.
                                                 +youthEmployment$Male..22.to.24.years..Not.in.labor.force.
                                                 +youthEmployment$Male..25.to.29.years..Not.in.labor.force.
                                                 +youthEmployment$Male..30.to.34.years..Not.in.labor.force.

                                                         )/
                                        (youthEmployment$Male..16.to.19.years..+
                                        youthEmployment$Male..20.and.21.years..+
                                        youthEmployment$Male..22.to.24.years..+
                                         youthEmployment$Male..25.to.29.years..+
                                         youthEmployment$Male..30.to.34.years..)

    youthEmployment$youth.idle.pct <-
        youthEmployment$youth.unemployed.pct+
            youthEmployment$youth.Not.in.labor.force.pct 

}


