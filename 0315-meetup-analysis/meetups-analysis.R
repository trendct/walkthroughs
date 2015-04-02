library(plyr)
library(dplyr)

ctmeet <- read.csv("meetup_groups.csv", quote = "", 
                   row.names = NULL, 
                   stringsAsFactors = FALSE)

# Adding column names
colnames(ctmeet) <- c("Area","Group","Created","Neighborhood","State","Category","Members","Who")

# Converting the Date group was created into a recognized format
ctmeet$Created_Date <- as.POSIXct(ctmeet$Created/1000, origin="1970-01-01")

# Extracting the year
ctmeet$Year <- as.numeric(format(ctmeet$Created_Date,"%Y"))

# Total number of members by Area
ctmeet %>% group_by(Area) %>% summarise(Total.Members=sum(Members))


# Analyzing number of groups per city by category and building a dataframe

cat1 <- ctmeet %>% filter(Area=="Bridgeport") %>% group_by(Category) %>% summarise(Bridgeport=length(Category))

cat2 <- ctmeet %>% filter(Area=="Hartford") %>% group_by(Category) %>% summarise(Hartford=length(Category))

cat <- join(cat2, cat1, by="Category")

cat3 <- ctmeet %>% filter(Area=="New Haven") %>% group_by(Category) %>% summarise(New.Haven=length(Category))

cat <- join(cat, cat3, by="Category")

cat4 <- ctmeet %>% filter(Area=="Stamford") %>% group_by(Category) %>% summarise(Stamford=length(Category))

cat <- join(cat, cat4, by="Category")

cat5 <- ctmeet %>% filter(Area=="Waterbury") %>% group_by(Category) %>% summarise(Waterbury=length(Category))

cat <- join(cat, cat5, by="Category")


# Turn the figures into percent and add them to the dataframe

cat$Hartford.Perc <- round(((cat$Hartford/sum(cat$Hartford, na.rm=TRUE))*100), digits=2)

cat$Bridgeport.Perc <- round(((cat$Bridgeport/sum(cat$Bridgeport, na.rm=TRUE))*100), digits=2)

cat$New.Haven.Perc <- round(((cat$New.Haven/sum(cat$New.Haven, na.rm=TRUE))*100), digits=2)

cat$Stamford.Perc <- round(((cat$Stamford/sum(cat$Stamford, na.rm=TRUE))*100), digits=2)

cat$Waterbury.Perc <- round(((cat$Waterbury/sum(cat$Waterbury, na.rm=TRUE))*100), digits=2)

# Exporting the dataframe into a CSV

write.csv(cat,"ct-categories.csv")


# Ok, time to look at group membership count

members1 <- ctmeet %>% filter(Area=="Bridgeport") %>% group_by(Category) %>% summarise(Bridgeport=sum(Members))

members2 <- ctmeet %>% filter(Area=="Hartford") %>% group_by(Category) %>% summarise(Hartford=sum(Members))

members <- join(members2, members1, by="Category")

members3 <- ctmeet %>% filter(Area=="New Haven") %>% group_by(Category) %>% summarise(New.Haven=sum(Members))

members <- join(members, members3, by="Category")

members4 <- ctmeet %>% filter(Area=="Stamford") %>% group_by(Category) %>% summarise(Stamford=sum(Members))

members <- join(members, members4, by="Category")

members5 <- ctmeet %>% filter(Area=="Waterbury") %>% group_by(Category) %>% summarise(Waterbury=sum(Members))

members <- join(members, members5, by="Category")
 

# How's it look as a percentage? 

members$Hartford.Perc <- round(((members$Hartford/sum(members$Hartford, na.rm=TRUE))*100), digits=2)

members$Bridgeport.Perc <- round(((members$Bridgeport/sum(members$Bridgeport, na.rm=TRUE))*100), digits=2)

members$New.Haven.Perc <- round(((members$New.Haven/sum(members$New.Haven, na.rm=TRUE))*100), digits=2)

members$Stamford.Perc <- round(((members$Stamford/sum(members$Stamford, na.rm=TRUE))*100), digits=2)

members$Waterbury.Perc <- round(((members$Waterbury/sum(members$Waterbury, na.rm=TRUE))*100), digits=2)

write.csv(members,"ct-members.csv")
  

# Hartford groups by year

HartfordTotal <- ctmeet %>% filter(Area=="Hartford") %>% group_by(Category) %>% summarise(Total=length(Category))

Hartford2003 <- ctmeet %>% filter(Area=="Hartford" & Year==2003) %>% group_by(Category) %>% summarise(y2003=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2003, by="Category")

Hartford2004 <- ctmeet %>% filter(Area=="Hartford" & Year==2004) %>% group_by(Category) %>% summarise(y2004=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2004, by="Category")

Hartford2005 <- ctmeet %>% filter(Area=="Hartford" & Year==2005) %>% group_by(Category) %>% summarise(y2005=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2005, by="Category")

Hartford2006 <- ctmeet %>% filter(Area=="Hartford" & Year==2006) %>% group_by(Category) %>% summarise(y2006=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2006, by="Category")

Hartford2007 <- ctmeet %>% filter(Area=="Hartford" & Year==2007) %>% group_by(Category) %>% summarise(y2007=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2007, by="Category")

Hartford2008 <- ctmeet %>% filter(Area=="Hartford" & Year==2008) %>% group_by(Category) %>% summarise(y2008=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2008, by="Category")

Hartford2009 <- ctmeet %>% filter(Area=="Hartford" & Year==2009) %>% group_by(Category) %>% summarise(y2009=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2009, by="Category")

Hartford2010 <- ctmeet %>% filter(Area=="Hartford" & Year==2010) %>% group_by(Category) %>% summarise(y2010=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2010, by="Category")

Hartford2011 <- ctmeet %>% filter(Area=="Hartford" & Year==2011) %>% group_by(Category) %>% summarise(y2011=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2011, by="Category")

Hartford2012 <- ctmeet %>% filter(Area=="Hartford" & Year==2012) %>% group_by(Category) %>% summarise(y2012=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2012, by="Category")

Hartford2013 <- ctmeet %>% filter(Area=="Hartford" & Year==2013) %>% group_by(Category) %>% summarise(y2013=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2013, by="Category")

Hartford2014 <- ctmeet %>% filter(Area=="Hartford" & Year==2014) %>% group_by(Category) %>% summarise(y2014=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2014, by="Category")

Hartford2015 <- ctmeet %>% filter(Area=="Hartford" & Year==2015) %>% group_by(Category) %>% summarise(y2015=length(Category))
HartfordTotal <- join(HartfordTotal, Hartford2015, by="Category")

write.csv(HartfordTotal,"Hartford_years.csv")


# New Haven groups by year

NewHavenTotal <- ctmeet %>% filter(Area=="New Haven") %>% group_by(Category) %>% summarise(Total=length(Category))

NewHaven2003 <- ctmeet %>% filter(Area=="New Haven" & Year==2003) %>% group_by(Category) %>% summarise(y2003=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2003, by="Category")

NewHaven2004 <- ctmeet %>% filter(Area=="New Haven" & Year==2004) %>% group_by(Category) %>% summarise(y2004=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2004, by="Category")

NewHaven2005 <- ctmeet %>% filter(Area=="New Haven" & Year==2005) %>% group_by(Category) %>% summarise(y2005=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2005, by="Category")

NewHaven2006 <- ctmeet %>% filter(Area=="New Haven" & Year==2006) %>% group_by(Category) %>% summarise(y2006=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2006, by="Category")

NewHaven2007 <- ctmeet %>% filter(Area=="New Haven" & Year==2007) %>% group_by(Category) %>% summarise(y2007=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2007, by="Category")

NewHaven2008 <- ctmeet %>% filter(Area=="New Haven" & Year==2008) %>% group_by(Category) %>% summarise(y2008=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2008, by="Category")

NewHaven2009 <- ctmeet %>% filter(Area=="New Haven" & Year==2009) %>% group_by(Category) %>% summarise(y2009=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2009, by="Category")

NewHaven2010 <- ctmeet %>% filter(Area=="New Haven" & Year==2010) %>% group_by(Category) %>% summarise(y2010=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2010, by="Category")

NewHaven2011 <- ctmeet %>% filter(Area=="New Haven" & Year==2011) %>% group_by(Category) %>% summarise(y2011=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2011, by="Category")

NewHaven2012 <- ctmeet %>% filter(Area=="New Haven" & Year==2012) %>% group_by(Category) %>% summarise(y2012=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2012, by="Category")

NewHaven2013 <- ctmeet %>% filter(Area=="New Haven" & Year==2013) %>% group_by(Category) %>% summarise(y2013=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2013, by="Category")

NewHaven2014 <- ctmeet %>% filter(Area=="New Haven" & Year==2014) %>% group_by(Category) %>% summarise(y2014=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2014, by="Category")

NewHaven2015 <- ctmeet %>% filter(Area=="New Haven" & Year==2015) %>% group_by(Category) %>% summarise(y2015=length(Category))
NewHavenTotal <- join(NewHavenTotal, NewHaven2015, by="Category")

write.csv(NewHavenTotal,"NewHaven_years.csv")



# Bridgeport groups by year

BridgeportTotal <- ctmeet %>% filter(Area=="Bridgeport") %>% group_by(Category) %>% summarise(Total=length(Category))

Bridgeport2003 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2003) %>% group_by(Category) %>% summarise(y2003=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2003, by="Category")

Bridgeport2004 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2004) %>% group_by(Category) %>% summarise(y2004=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2004, by="Category")

Bridgeport2005 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2005) %>% group_by(Category) %>% summarise(y2005=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2005, by="Category")

Bridgeport2006 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2006) %>% group_by(Category) %>% summarise(y2006=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2006, by="Category")

Bridgeport2007 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2007) %>% group_by(Category) %>% summarise(y2007=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2007, by="Category")

Bridgeport2008 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2008) %>% group_by(Category) %>% summarise(y2008=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2008, by="Category")

Bridgeport2009 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2009) %>% group_by(Category) %>% summarise(y2009=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2009, by="Category")

Bridgeport2010 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2010) %>% group_by(Category) %>% summarise(y2010=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2010, by="Category")

Bridgeport2011 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2011) %>% group_by(Category) %>% summarise(y2011=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2011, by="Category")

Bridgeport2012 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2012) %>% group_by(Category) %>% summarise(y2012=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2012, by="Category")

Bridgeport2013 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2013) %>% group_by(Category) %>% summarise(y2013=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2013, by="Category")

Bridgeport2014 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2014) %>% group_by(Category) %>% summarise(y2014=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2014, by="Category")

Bridgeport2015 <- ctmeet %>% filter(Area=="Bridgeport" & Year==2015) %>% group_by(Category) %>% summarise(y2015=length(Category))
BridgeportTotal <- join(BridgeportTotal, Bridgeport2015, by="Category")

write.csv(BridgeportTotal,"Bridgeport_years.csv")


# Stamford groups by year

StamfordTotal <- ctmeet %>% filter(Area=="Stamford") %>% group_by(Category) %>% summarise(Total=length(Category))

Stamford2003 <- ctmeet %>% filter(Area=="Stamford" & Year==2003) %>% group_by(Category) %>% summarise(y2003=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2003, by="Category")

Stamford2004 <- ctmeet %>% filter(Area=="Stamford" & Year==2004) %>% group_by(Category) %>% summarise(y2004=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2004, by="Category")

Stamford2005 <- ctmeet %>% filter(Area=="Stamford" & Year==2005) %>% group_by(Category) %>% summarise(y2005=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2005, by="Category")

Stamford2006 <- ctmeet %>% filter(Area=="Stamford" & Year==2006) %>% group_by(Category) %>% summarise(y2006=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2006, by="Category")

Stamford2007 <- ctmeet %>% filter(Area=="Stamford" & Year==2007) %>% group_by(Category) %>% summarise(y2007=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2007, by="Category")

Stamford2008 <- ctmeet %>% filter(Area=="Stamford" & Year==2008) %>% group_by(Category) %>% summarise(y2008=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2008, by="Category")

Stamford2009 <- ctmeet %>% filter(Area=="Stamford" & Year==2009) %>% group_by(Category) %>% summarise(y2009=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2009, by="Category")

Stamford2010 <- ctmeet %>% filter(Area=="Stamford" & Year==2010) %>% group_by(Category) %>% summarise(y2010=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2010, by="Category")

Stamford2011 <- ctmeet %>% filter(Area=="Stamford" & Year==2011) %>% group_by(Category) %>% summarise(y2011=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2011, by="Category")

Stamford2012 <- ctmeet %>% filter(Area=="Stamford" & Year==2012) %>% group_by(Category) %>% summarise(y2012=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2012, by="Category")

Stamford2013 <- ctmeet %>% filter(Area=="Stamford" & Year==2013) %>% group_by(Category) %>% summarise(y2013=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2013, by="Category")

Stamford2014 <- ctmeet %>% filter(Area=="Stamford" & Year==2014) %>% group_by(Category) %>% summarise(y2014=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2014, by="Category")

Stamford2015 <- ctmeet %>% filter(Area=="Stamford" & Year==2015) %>% group_by(Category) %>% summarise(y2015=length(Category))
StamfordTotal <- join(StamfordTotal, Stamford2015, by="Category")

write.csv(StamfordTotal,"Stamford_years.csv")


# Waterbury groups by year

WaterburyTotal <- ctmeet %>% filter(Area=="Waterbury") %>% group_by(Category) %>% summarise(Total=length(Category))

Waterbury2003 <- ctmeet %>% filter(Area=="Waterbury" & Year==2003) %>% group_by(Category) %>% summarise(y2003=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2003, by="Category")

Waterbury2004 <- ctmeet %>% filter(Area=="Waterbury" & Year==2004) %>% group_by(Category) %>% summarise(y2004=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2004, by="Category")

Waterbury2005 <- ctmeet %>% filter(Area=="Waterbury" & Year==2005) %>% group_by(Category) %>% summarise(y2005=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2005, by="Category")

Waterbury2006 <- ctmeet %>% filter(Area=="Waterbury" & Year==2006) %>% group_by(Category) %>% summarise(y2006=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2006, by="Category")

Waterbury2007 <- ctmeet %>% filter(Area=="Waterbury" & Year==2007) %>% group_by(Category) %>% summarise(y2007=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2007, by="Category")

Waterbury2008 <- ctmeet %>% filter(Area=="Waterbury" & Year==2008) %>% group_by(Category) %>% summarise(y2008=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2008, by="Category")

Waterbury2009 <- ctmeet %>% filter(Area=="Waterbury" & Year==2009) %>% group_by(Category) %>% summarise(y2009=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2009, by="Category")

Waterbury2010 <- ctmeet %>% filter(Area=="Waterbury" & Year==2010) %>% group_by(Category) %>% summarise(y2010=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2010, by="Category")

Waterbury2011 <- ctmeet %>% filter(Area=="Waterbury" & Year==2011) %>% group_by(Category) %>% summarise(y2011=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2011, by="Category")

Waterbury2012 <- ctmeet %>% filter(Area=="Waterbury" & Year==2012) %>% group_by(Category) %>% summarise(y2012=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2012, by="Category")

Waterbury2013 <- ctmeet %>% filter(Area=="Waterbury" & Year==2013) %>% group_by(Category) %>% summarise(y2013=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2013, by="Category")

Waterbury2014 <- ctmeet %>% filter(Area=="Waterbury" & Year==2014) %>% group_by(Category) %>% summarise(y2014=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2014, by="Category")

Waterbury2015 <- ctmeet %>% filter(Area=="Waterbury" & Year==2015) %>% group_by(Category) %>% summarise(y2015=length(Category))
WaterburyTotal <- join(WaterburyTotal, Waterbury2015, by="Category")

write.csv(WaterburyTotal,"Waterbury_years.csv")

