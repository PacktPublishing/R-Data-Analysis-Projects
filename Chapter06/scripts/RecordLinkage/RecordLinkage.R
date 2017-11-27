###############################################################################
#   
#   R Data Analysis Projects
#
#   Chapter X
#
#   Record Linkage
#   Stochastic and Machine Learning Approaches 

#
#   Gopi Subramanian
###############################################################################

library(RecordLinkage, quietly = TRUE)


###### Quick look at our data #########
data(RLdata500)
str(RLdata500)
head(RLdata500)


#### Feature generation #############

rec.pairs <- compare.dedup(RLdata500
                       ,blockfld = list(1, 5:7)
                       ,strcmp =   c(2,3,4)
                       ,strcmpfun = levenshteinSim)

summary(rec.pairs)

matches <- rec.pairs$pairs
matches[c(1:3, 1203:1204), ]

RLdata500[1,]
RLdata500[174,]

# String features
rec.pairs.matches <- compare.dedup(RLdata500
                                   ,blockfld = list(1, 5:7)
                                   ,strcmp =   c(2,3,4)
                                  ,strcmpfun = levenshteinSim)

# Not specifying the fields for string comparision
rec.pairs.matches <- compare.dedup(RLdata500
                                   ,blockfld = list(1, 5:7)
                                   ,strcmp =   TRUE
                                   ,strcmpfun = levenshteinSim)

head(rec.pairs.matches$pairs)

# String comparision before blocking
rec.pairs.matches <- compare.dedup(RLdata500
                                   ,blockfld = list(1, 5:7)
                                   ,strcmp =   c(1,2,3,4)
                                   ,strcmpfun = levenshteinSim)
head(rec.pairs.matches$pairs)


# Phoenotic features
rec.pairs.matches <- compare.dedup(RLdata500
                                   ,blockfld = list(1, 5:7)
                                   ,phonetic =   c(2,3,4)
                                   ,phonfun  = pho_h)

head(rec.pairs.matches$pairs)

RLdata500[2,]
RLdata500[43,]




