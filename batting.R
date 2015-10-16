awards <- read.csv("data/lahman-csv_2015-01-24/AwardsPlayers.csv", stringsAsFactors = F)
mvp = subset(awards, awardID == "Most Valuable Player")
years = unique(mvp$yearID)
batting = read.csv("data/lahman-csv_2015-01-24/Batting.csv", stringsAsFactors = F)
batting$AVG = batting$H/batting$AB
batting$PA = rowSums(batting[,c("AB", "BB", "HBP", "SF")], na.rm = T)
batting$EBH = rowSums(batting[,c("X2B", "X3B", "HR")], na.rm = T)
batting$X1B = batting$H - batting$EBH
batting$SLG = (batting$X1B + (batting$X2B*2) + (batting$X3B*3) + (batting$HR*4))/batting$AB
batting$OB = rowSums(batting[,c("H", "BB", "HBP")], na.rm = T)
batting$OBP = batting$OB/batting$PA
batting$OPS = batting$SLG + batting$OBP

for(i in years) {
  print(i)
  ## AL
  y_AL = subset(batting, yearID == i & PA > 500 & lgID == "AL")
  
  y_AL$ZPA = ZScore(y_AL$PA)
  y_AL$ZR = ZScore(y_AL$R)
  y_AL$ZAVG = ZScore(y_AL$AVG)
  y_AL$ZHR = ZScore(y_AL$HR)
  y_AL$ZSLG = ZScore(y_AL$SLG)
  y_AL$ZOBP = ZScore(y_AL$OBP)
  y_AL$ZOPS = ZScore(y_AL$OPS)
  y_AL$ZSB = ZScore(y_AL$SB)
  
  almvp = subset(mvp, yearID == i & lgID == "AL")
  print(subset(y_AL, playerID == almvp$playerID))
  
  
  ## NL
  y_NL = subset(batting, yearID == i & PA > 500 & lgID == "NL")
  
  y_NL$ZPA = ZScore(y_NL$PA)
  y_NL$ZR = ZScore(y_NL$R)
  y_NL$ZAVG = ZScore(y_NL$AVG)
  y_NL$ZHR = ZScore(y_NL$HR)
  y_NL$ZSLG = ZScore(y_NL$SLG)
  y_NL$ZOBP = ZScore(y_NL$OBP)
  y_NL$ZOPS = ZScore(y_NL$OPS)
  y_NL$ZSB = ZScore(y_NL$SB)
  
  nlmvp = subset(mvp, yearID == i & lgID == "NL")
  print(subset(y_NL, playerID == nlmvp$playerID))
}

