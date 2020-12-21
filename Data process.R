library(readxl)
library(dplyr)
library(zoo)
library(writexl)

# ---------

Spot = read_excel('RWTCm.xls')
Futures = read_excel('RCLC4m.xls')
read_excel('MONTHLY DATA.xlsx')

Spot$Date = as.Date(as.yearmon(Spot$Date))
Futures$Date = as.Date(as.yearmon(Futures$Date))
# ---------


terr = read_excel('globalterrorismdb_0919dist.xlsx')
terr$date <- as.Date(with(terr, paste(iyear, imonth, iday,sep="-")), "%Y-%m-%d")
terr1 = terr %>% 
        select(date, region_txt) %>%
        filter(region_txt == 'Middle East & North Africa') %>%
        group_by(date) %>%
        summarise(n =n())
plot(terr1$date, terr1$n, type = 'l')
terr1$date <- format(as.Date(terr1$date ), "%Y-%m")

terr1 = terr1 %>%
        group_by(date) %>%
        summarise(n =sum(n))
terr1$date = as.Date(as.yearmon(terr1$date))
plot(terr1$date, terr1$n, type = 'l')
write_xlsx(terr1, 'terrorism in mena.xlsx')



# ---------
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
        # I prefer straight data.frames
        # but if you like tidyverse tibbles (the default with read_excel)
        # then just pass tibble = TRUE
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
}

all_sheet = read_excel_allsheets('MONTHLY DATA.xlsx')
names(all_sheet)
a = all_sheet[[1]]
s = all_sheet[[2]]
d = all_sheet[[3]]
f = all_sheet[[4]]
g = all_sheet[[5]]
h = all_sheet[[6]]
j = all_sheet[[7]]
k = all_sheet[[8]]
l = all_sheet[[9]]
z = all_sheet[[10]]
x = all_sheet[[11]]
p = all_sheet[[12]]




a$Date = as.Date(as.yearmon(a$Date))
s$Date = as.Date(as.yearmon(s$Date))
d$Date = as.Date(as.yearmon(d$Date))
f$Date = as.Date(as.yearmon(f$Date))
g$Date = as.Date(as.yearmon(g$Date))
h$Date = as.Date(as.yearmon(h$Date))
j$Date = as.Date(as.yearmon(j$Date))
k$Date = as.Date(as.yearmon(k$Date))
l$Date = as.Date(as.yearmon(l$Date))
z$Date = as.Date(as.yearmon(z$Date))
x$Date = as.Date(as.yearmon(x$Date))
p$Date = as.Date(as.yearmon(p$Date))

database = z[37:264,]
database = left_join(database, a)
database = left_join(database, s)
database = left_join(database, d)
database = left_join(database, f)
database = left_join(database, g)
database = left_join(database, h)

database = left_join(database, k)
database = left_join(database, l)
database = left_join(database, x)
database = left_join(database, p)

database = left_join(database, Spot)
database = left_join(database, Futures)
database$basis = database$`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)` - database$`Cushing, OK Crude Oil Future Contract 4 (Dollars per Barrel)`
plot(x = database$Date,database$basis, type = 'l')
plot(x = database$Date,database$`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, type = 'l')
lines(x = database$Date,database$`Cushing, OK Crude Oil Future Contract 4 (Dollars per Barrel)`, type = 'l', col = 'blue')



database$`GDP GROWTH RATE` = c(na.approx(database$`GDP GROWTH RATE`), 4.2, 4.2)
colnames(terr1)[1] = 'Date'
database = left_join(database, terr1)
database = database[, c(1:12,16,15,13,14)]
colnames(database)[13] = 'Terrorism_attack'
write_xlsx(database, 'database.xlsx')












