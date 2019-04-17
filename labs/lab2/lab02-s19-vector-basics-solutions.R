# Title: "Lab 2: Vectors"
# Subtitle: "Stat 133"
# Author: "Gaston Sanchez"


# remove existing objects in your current workspace
rm(list = ls())

# download data to working directory
rdata <- "https://github.com/ucb-stat133/stat133-labs/raw/master/data/nba2018-salary-points.RData"
download.file(url = rdata, destfile = 'nba2018-salary-points.RData')

# load data in your R session
load('nba2018-salary-points.RData')

# list the available objects with ls()
ls()

# examined the class of the objects
class(player)
class(team)
class(position)
class(salary)
class(scored)

# data type of each object
typeof(player)
typeof(position)
typeof(scored)

# ===============================================
# subsetting examples using a numeric vector inside the brackets:
# ===============================================

# fifth element of 'player'
player[5]

# numeric range
player[2:8]

# numeric vector
player[c(1, 3, 5, 7)]

# different order
player[c(20, 9, 10, 50)]

# third element (four times)
player[rep(3, 4)]

# Subset the first four players:
four <- player[1:4]


# ===============================================
# What happens if you specify: 
# ===============================================
# an index of zero: four[0]?
four[0]

# a negative index: four[-1]?
four[-1]

# various negative indices: four[-c(1,2,3)]?
four[-c(1,2,3)]

# an index greater than the length of the vector: four[5]?
four[5]

# repeated indices: four[c(1,2,2,3,3,3)]?
four[c(1,2,2,3,3,3)]


# ===============================================
# Figure out how to use `seq()`, `rep()` to extract:
# ===============================================
# all the even elements in player
player[seq(from = 2, to = length(player), by = 2)]

# all the odd elements in salary
salary[seq(from = 1, to = length(salary), by = 2)]

# all multiples of 5 (e.g. 5, 10, 15, etc) of team
team[seq(from = 5, to = length(team), by = 5)]

# elements in positions 10, 20, 30, 40, etc of scored points
scored[seq(from = 10, to = length(scored), by = 10)]

# all the even elements in team but this time in reverse order
rev(team[seq(from = 2, to = length(team), by = 2)])


# ===============================================
# Logical subsetting
# ===============================================

# dummy vector
a <- c(5, 6, 7, 8)

# logical subsetting
a[c(TRUE, FALSE, TRUE, FALSE)]

# your turn
four[c(TRUE, TRUE, TRUE, TRUE)]
four[c(TRUE, TRUE, FALSE, FALSE)]
four[c(FALSE, FALSE, TRUE, TRUE)]
four[c(TRUE, FALSE, TRUE, FALSE)]
four[c(FALSE, FALSE, FALSE, FALSE)]

# recycling
four[TRUE]
four[c(TRUE, FALSE)]

# elements greater than 6
a > 6

# elements different from 6
a != 6

scored_four <- points[1:4]

# elements greater than 100
scored_four[scored_four > 100]

# elements less than 100
scored_four[scored_four < 100]

# elements less than or equal to 10
scored_four[scored_four <= 10]

# elements different from 10
scored_four[scored_four != 10]


# Logical operators allow you to combine several comparisons:
# players of Golden State (GSW)
player[team == 'GSW']

# name of players with salaries greater than 20 million dollars
player[salary > 20000000]

# name of players with scored points between 1000 and 1200 (exclusive)
player[scored > 1000 & scored < 1200]



# ===============================================
# Use bracket notation, to answer the following questions
# ===============================================

# players in position Center, of Warriors (GSW)
player[position == 'C' & team == 'GSW']

# players of both GSW (warriors) and LAL (lakers)
player[team == 'GSW' | team == 'LAL']
player[team %in% c('GSW', 'LAL')]

# players in positions Shooting Guard and Point Guards, of Lakers (LAL) 
player[team == 'LAL' & (position == 'SG' | position == 'PG')]
player[team == 'LAL' & (position %in% c('SG', 'PG'))]

# subset Small Forwards of GSW and LAL
player[position == 'SF' & (team == 'GSW' | team == 'LAL')]
player[position == 'SF' & (team %in% c('GSW', 'LAL'))]

# name of the player with largest salary
player[which.max(salary)]

# name of the player with smallest salary
player[which.min(salary)]

# name of the player with largest number of scored points
player[which.max(scored)]

# salary of the player with largest number of scored points
salary[which.max(scored)]

# largest salary of all Centers
max(salary[position == 'C'])

# team of the player with the largest number of scored points
team[which.max(scored)]

# name of the player with the largest number of 3-pointers
player[which.max(points3)]



# ===============================================
# Subsetting with Character Vectors
# ===============================================

# create warriors_player
warriors_player <- player[team == 'GSW']

# create warriors_salary
warriors_salary <- salary[team == 'GSW']

# create warriors_points
warriors_points <- points[team == 'GSW']

# give names to warriors_salary
names(warriors_salary) <- warriors_player

# subsetting with namesd elements
warriors_salary["Andre Iguodala"]
warriors_salary[c("Stephen Curry", "Kevin Durant")]


# ===============================================
# Some plotting
# ===============================================

#scatterplot
plot(scored, salary)

# log transformations
log_scored <- log(scored)
log_salary <- log(salary)

# another scatterplot
plot(log_scored, log_salary)

# another plot
plot(log_scored, log_salary)
text(log_scored, log_salary, labels = player)

# one more plot
plot(log_scored, log_salary)
text(log_scored, log_salary, labels = abbreviate(player))


# ===============================================
# Factors
# ===============================================

is.factor(team)

position_fac <- factor(position)

table(position_fac)

position_fac[1:5]


# positions of Warriors
position_fac[team == 'GSW']

# positions of players with salaries > 15 millions
position_fac[salary > 15000000]

# frequencies (counts) of positions with salaries > 15 millions
table(position_fac[salary > 15000000])

# relative frequencies (proportions) of 'SG' (Shooting Guards) in each team
prop.table(table(team[position_fac == 'SG']))

table(team, position_fac)

