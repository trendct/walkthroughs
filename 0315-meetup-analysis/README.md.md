#Gathering and analyzing Meetup.com data by city
This tutorial will walkthrough the process in which I queried Meetup.com's API with Python and analyzed the data using R to write [the story]: The most popular meet-up groups in Connecticut cities.

What you'll need to reproduce this:
- A Meetup.com [API key]
- A way to run Python scripts. (Mac? Use terminal. PC? [Something a little more complicated])
- A way to run R scripts. ([RStudio] is highly recommended and works on any system)
- A program to open the generated CSV files (OK, this is a little too specific)
- A way to visualize the data, like [datawrapper.de] or [plot.ly] (I used a proprietary TrendCT tool)

I realize it'd make more sense to stay within one environment with this dataset (Pull and analyze the data within Python or R instead of switching from one to the other). But I say when a deadline is looming, you go with what you're most comfortable with. And this process, while not the cleanest it could be, is mine. That being said, I am totally open to refining it if any one reading this would have any suggestions. 

We are at all different levels of skill here and I hope to learn as much as I look forward to teaching.

##The Question
**How do the meet-up groups in the top five most-populated cities in Connecticut compare in membership totals, category types, and change in time? **

Meetup.com has a great API with excellent [documentation]. 
Here's the [meetup-pages-names-dates.py] script I used.

```sh
from __future__ import unicode_literals

import requests
import json
import time
import codecs
import sys
UTF8Writer = codecs.getwriter('utf8')
sys.stdout = UTF8Writer(sys.stdout)

def main():
        # If you want to change the cities to search, do so below
        cities =[("Bridgeport","CT"),("New Haven","CT"),("Hartford","CT"),("Stamford","CT"),("Waterbury","CT")]
        api_key= "YOUR_KEY_GOES_HERE"
        # Get your key here https://secure.meetup.com/meetup_api/key/
        for (city, state) in cities:
            per_page = 200
            results_we_got = per_page
            offset = 0
            while (results_we_got == per_page):
                # Meetup.com documentation here: http://www.meetup.com/meetup_api/docs/2/groups/
                # You can change search perimeter around each city by changing the "radius" parameter
                response=get_results({"sign":"true","country":"US", "city":city, "state":state, "radius": 10, "key":api_key, "page":per_page, "offset":offset })
                time.sleep(1)
                offset += 1
                results_we_got = response['meta']['count']
                for group in response['results']:
                    category = ""
                    if "category" in group:
                        category = group['category']['name']
                    print "," .join(map(unicode, [city, group['name'].replace(","," "), group['created'], group['city'],group.get('state',""),category,group['members'], group.get('who',"").replace(","," ")]))

            time.sleep(1)

def get_results(params):

	request = requests.get("http://api.meetup.com/2/groups",params=params)
        data = request.json()
	
	return data

if __name__=="__main__":
        main()
```

I ran the script and piped the results into a CSV by typing this in the terminal:

```python meetup-pages-names-dates.py > meetup_groups.csv```

##Analyzing the data

I used the dplyr library in R to organize and run calculations on the data. This is the script: [meetups-analysis.R]

This is the gist of what I was trying to do:
- Find the total number of groups by town per category
- Figure out the percentage of categories per town in case the population difference skewed the data
- Repeat the process above but for membership totals by category per city and group growth by year

```sh
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
```

##What next
You now have seven datasets to play with:
- One for categories compared city to city
- One for membership compared city to city  
- and five for all cities' groups over time

Go dig through the results and see what points you'd like to visualize or write about.
Have fun!

[API key]:https://secure.meetup.com/meetup_api/key/
[Something a little more complicated]:http://docs.python-guide.org/en/latest/starting/install/win/
[RSTudio]:http://www.rstudio.com
[The story]:http://trendct.org/2015/03/30/the-most-popular-meet-up-groups-in-connecticut-cities/
[datawrapper.de]:http://www.datawrapper.de
[plot.ly]:http://plot.ly
[documentation]:http://www.meetup.com/meetup_api/docs/2/groups/
[meetup-pages-names-dates.py]:https://github.com/trendct/walkthroughs/blob/master/0315-meetup-analysis/meetup-pages-names-dates.py
[meetups-analysis.R]:https://github.com/trendct/walkthroughs/blob/master/0315-meetup-analysis/meetups-analysis.R
