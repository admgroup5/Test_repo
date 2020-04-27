haha$TimeID <- 0
setDT(haha)[time, TimeID := i.TimeID, on = .("reg month" = Month, "reg year" = Year)]
view(haha)

setDT(haha)[time, time$TimeID := i.TimeID, on = .(haha$`reg month` = time$MonthName, haha$`reg year` = time$Year)]
view(haha)

haha <- haha %>% 
  mutate(TimeID=ifelse((haha$`reg month`%in%time$MonthName)&(haha$`reg year`%in%time$Year),time$TimeID,NA))


# sqldf("SELECT time.TimeID, haha.`Authority code`, haha.`Authority name`, haha.`new births`
# FROM haha
# INNER JOIN time
# ON haha.`reg month`=time.MonthName;")


# change a column from excel date codes into usable dates
haha<- x4
view(haha)
haha$date <- as.Date(haha$date, origin = "1899-12-30")
haha$date <- as.Date(haha$date, format = "%m/%d/%y")



myprettydate <- format(25-05-2018, format = "%b %d, %Y")
###########################################################################
###########################################################################
# *************example codes*****************

# Add a new column called "new" in the dataset called "haha"and populate it with the value 6 in every row 
# haha$new <- 6


# exp <- separate(cleanhouse,'Date', c('month', 'year'), sep = '-')
# Below is working, but only one iteration at a time. I will have to run it manually hundreds of times
# for it to find all the local authorities and replace them with their matching local authority codes
# as1$`Local Authority`[match(la$`Local authority`, as1$`Local Authority`)] <- la$`Local authority code`
# counts <- table(as1$`Local Authority`, useNA ="ifany")
# view(counts)
# for(i in junk$alpha) if(i %in% "B") junk$alpha <- "b"

# as1$Values[match(as2$ID, as1$ID)] <- as2$Values
# as1 <- data.frame(ID = c(1,2,3,4,5,6),
#                   pID = c(21,22,23,24,25,26),
#                   Values = c(435,33,45,NA, NA,12))
# as2 <- data.frame(ID = c(4,5),
#                   pid = c(24,25),
#                   Values = c(544, 676))

# df[df$height == 20, "height"] <- NA
# xtry <- x2 %>% mutate(`Local Authority` = replace(`Local Authority`, `Local Authority` %in% tst2, tstval))
# for(i in as1$`Local Authority`) if(i %in% la$`Local authority`) as1$la2[match(la$`Local authority`, as1$`Local Authority`)] <- la$`Local authority code`
# counts <- table(as1$`Local Authority`, useNA ="ifany")
# view(counts)

# for(i in as1$la2) if(i == "Bolton" ) as1$la2<- 'E08000001'
# Below works but once it has replaced all of bury, it cannot work again because the code is still
# looking for bury and cannot replace a null
# for(i in as1$`Local Authority`) if(i %in% la$`Local authority`) as1$`Local Authority`[match(la$`Local authority`, as1$`Local Authority`)] <- la$`Local authority code`

# *************example codes*****************
###########################################################################
###########################################################################