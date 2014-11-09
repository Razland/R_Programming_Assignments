

df <-read.csv("outcome-of-care-measures.csv")[, c(2,7,11,17,23)]
names(df) <-gsub("[.]", "", names(df)) ## cuts "." from names
## changes names on data columns to more manageable strings
names(df) <- c("HospitalName", "State" , "HeartAttackRate", "HeartFailureRate", "PneumoniaRate")
## gets HeartAttackRate data order for state over the entire data frame A-Z
orderVector <- order(df[df$State==state, 3], decreasing = FALSE, na.last=TRUE)
##  writes hospitalname, state, heartattackrate
(df[df$State==state, 1:3])[orderVector,]
## used to get alphabetical list of states
stateVector <- sort(unique(df[df$State,2]))
##prints (accesses) all the states
for (i in 1:length(levels(stateVector))) { print(levels(stateVector)[i])}


# order of heart attack for state == {code}
orderVector <- order(df[df$State==state, 3], decreasing = FALSE, na.last=TRUE)
# add vector to list for statevector position[] == 2 state == AL
orderList[2] <- list(orderVector)
#list all heartattackrate with hospitalname and state for state==AL
(df[df$State==state,1:3])[orderList[[2]],]

# for each state, get the order for heartattackrate, store in numbered
# list
for (i in 1:length(levels(stateVector))) { (levels(stateVector)[i]) -> state
orderVector <- order(df[df$State==state, 3], decreasing = FALSE, na.last=TRUE)
orderList[i] <- list(orderVector)}


#sets some null text values to NA
df[(df$HeartAttackRate == "Not Available"), 3 ] <- NA
df[df$HeartFailureRate=="Not Available", 4] <- NA
df[df$PneumoniaRate=="Not Available", 5] <- NA
#sets text data values to numeric
df$HeartAttackRate <- as.numeric(df$HeartAttackRate)
df$HeartFailureRate <- as.numeric(df$HeartFailureRate)
df$PneumoniaRate <- as.numeric(df$PneumoniaRate)




order((na.omit(df[df$State=="TX", c(1,3)]))[2]) -> HeartAttackOrder
head((na.omit(df[df$State=="TX", c(1,3)]))[HeartAttackOrder,])
(((na.omit(df[df$State=="TX", c(1,3)]))[HeartAttackOrder,])[2])
unique(((na.omit(df[df$State=="TX", c(1,3)]))[HeartAttackOrder,])[2])
