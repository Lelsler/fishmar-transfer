

k = 2000
m = "ARG"


table1 <- data[data$t == k, ]
#edgeList1 <- table1 %>% select(iso3, imp.iso3)

nextYear <- k + 1
# create appropriate second table, and first edgelist
table2 <- data[data$t == nextYear, ]
#edgeList2 <- table2 %>% select(iso3, imp.iso3)

#edgeList2$Exporter = as.character(edgeList2$Exporter)
#edgeList1$Exporter = as.character(edgeList1$Exporter)
#edgeList2$Importer == as.character(edgeList2$Importer)
#edgeList1$Importer == as.character(edgeList1$Importer)

c2 <- table2[table2$iso3 == m, ]
c1 <- table1[table1$imp.iso3 == m, ]

comp <- c2$imp.iso3 %in% c1$imp.iso3 # New Imports

df = c2[!(c2$imp.iso3 %in% c1$imp.iso3), ]


