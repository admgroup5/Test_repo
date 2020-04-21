
###########################################################################
###########################################################################
# *************example codes*****************

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