rm(list = ls())

library(RODBC)
(con <- odbcConnect('rtest', case = 'tolower'))
(sqlTables(con))
(sqlQuery(con, 'describe who'))
who.from.MySQL <- sqlQuery(con, 'select * from who')
head(who.from.MySQL[, 1 : 3])
(odbcClose(con))

rm(list = ls())
load('capital.punishment.rda')
library(RODBC)
con <- odbcConnect('rtest')
(sqlTables(con))
sqlSave(con, capital.punishment,
  tablename = 'capital_punishment')
sqlTables(con)
odbcClose(con)
