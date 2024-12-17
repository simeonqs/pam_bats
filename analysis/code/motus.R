library(motus) # install from https://github.com/MotusWTS/motus
library(dplyr)
library(lubridate)

# 648 is our project number, it will ask you to login with your user
# new = TRUE means you download, FALSE means you load something you already
# downloaded
sql_motus <- tagme(648, dir = "./data/", new = TRUE) 
tbl_alltags <- tbl(sql_motus, "alltags")
df_alltags <- tbl_alltags %>%
  collect() %>%
  mutate(time = as_datetime(ts))

# look at single tag
tag_dat = df_alltags[df_alltags$motusTagID == '78818',]

# look at subset of pings based on longitude
sub = tag_dat[tag_dat$recvDeployLon > 5,]
plot(sub$recvDeployLat, sub$recvDeployLon)

# look at when pings were made across different locations
plot(sub$time, col = as.integer(as.factor(sub$recvDeployName)))

# get all ping locations
unique(sub$recvDeployName)

# look at single ping location, if the differences are mainly around 2.9 
# seconds, you should bre able to trust it's not just noised that pinged
t = sub[sub$recvDeployName == '03_Sönke-Nissen-Koog',]$time |> sort() |> diff()
plot(as.numeric(t[t<10]))
length(which(t < 4 & t > 2))
