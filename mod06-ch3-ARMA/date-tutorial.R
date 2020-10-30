# From string to date object
# Using the standard ISO date format
# https://en.wikipedia.org/wiki/ISO_8601
dt1 <- as.Date("2012-07-22")
dt1
class(dt1)
typeof(dt1)

# Same, string to object, but using the specific format
dt2 <- as.Date("04/20/2011", format = "%m/%d/%Y")
dt2

dt3 <- as.Date("October 6, 2010", format = "%B %d, %Y")
dt3

dt1 - dt2
# Data object knows how to subtract
# and it prints it in days by default

difftime(dt1, dt2, units = "weeks")
# Same difference, but in specific units

dt2 + 10
# The standard unit is a day. 
# So it reads an integer as days

dt2 - 10

three.dates <- as.Date(c("2010-07-22", "2011-04-20", "2012-10-06"))
three.dates
# Vector of dates

diff(three.dates)
## Time differences in days

six.weeks <- seq(dt1, length = 6, by = "week")
six.weeks
# Six dates differing by a week

six.fortnight <- seq(dt1, length = 6, by = 14)
six.fortnight
# Six dates differing by 14 days

six.fortnight <- seq(dt1, length = 6, by = "2 weeks")
six.fortnight

unclass(as.Date("1970-01-01"))
# Starting unix time
unclass(dt1)
dt1 - as.Date("1970-01-01")
# I subtracted zero days
## Time difference of 15543 days

# https://en.wikipedia.org/wiki/POSIX
# Document of unix standard for user management, time and other things
# that make operating system unix-like
# https://en.wikipedia.org/wiki/Unix_time

tm1 <- as.POSIXct("2013-07-24 23:55:26")
tm1
typeof(tm1)
class(tm1)
# The posix object contains time unlike date ojbect

datetime1 <- as.Date("2013-07-24 23:55:26")
datetime1
# The time was cut off
# And the basic representation is a number of days

# Again posix, but with non-standard time format
tm2 <- as.POSIXct("25072013 08:32:07", format = "%d%m%Y %H:%M:%S")
tm2

tm3 <- as.POSIXct("2010-12-01 11:42:03", tz = "GMT")
tm3
# This is with explicit time zone specification
# Otherwise it assumes UTC
# https://en.wikipedia.org/wiki/Coordinated_Universal_Time

tm2 > tm1
# Posix object could be compared
as.Date(tm2) > as.Date(tm1)
# and date objects too
# But the following fails due to type difference
as.Date(tm2) > tm1

# Again difference
as.POSIXct("2013-03-10 08:32:07") - as.POSIXct("2013-03-09 23:55:26")

st = Sys.time()
st
class(st)
unclass(st)
unclass(tm1)
# One has attributes, and the other does not, 
# because the other has the standard time zone. 
# ----- stopped here ------------

difftime(tm1, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "secs")

# POSIXlt is a third class 
# that allows to easily extract fields from a list
tm1.lt <- as.POSIXlt("2013-07-24 23:55:26")
tm1.lt

pisixlt_unclassed = unclass(tm1.lt)
pisixlt_unclassed
class(pisixlt_unclassed)

tm1.lt[1]
tm1.lt[2]
tm1.lt[[1]]
# tm1.lt[[[1]]] # Triple indexation is too much


pisixlt_unlisted = unlist(tm1.lt)
pisixlt_unlisted
class(pisixlt_unlisted)
tm1.lt$sec

tm1.lt$wday

tm1.lt_min_truncated = trunc(tm1.lt, "days")
tm1.lt_min_truncated
tm1.lt_min_truncated$minutes

trunc(tm1.lt, "mins")

# regression
x <- 1:5
d <- as.Date(c("2020-08-28", "2020-08-29", "2020-08-30", "2020-08-31", "2020-09-01"))
lm(x ~ d)

# https://www.unixtimestamp.com/index.php
# Repeat with pure numbers
d1 = unclass(as.Date("2020-08-28"))
d1
d_vector = c(d1, d1+1, d1+2, d1+3, d1+4)
# can also do
d_vector = d1 : (d1+4)
d_vector

lm(x ~ d_vector)
# coincides

p <- as.POSIXct(c("2020-08-28", "2020-08-29", "2020-08-30", "2020-08-31", "2020-09-01"))
lm(x ~ p)


ts_freq_1 <- ts(1:5, start = 2020, frequency = 1)
time(ts_freq_1)
lm(ts_freq_1 ~ time(ts_freq_1))

ts_freq_12 <- ts(1:5, start = 2020, frequency = 12)
time(ts_freq_12)
lm(ts_freq_12 ~ time(ts_freq_12))
