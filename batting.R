# awards <- read.csv("data/lahman-csv_2015-01-24/AwardsPlayers.csv", stringsAsFactors = F)
# mvp = subset(awards, awardID == "Most Valuable Player")
# years = unique(mvp$yearID)
batting = read.csv("data/lahman-csv_2015-01-24/Batting.csv", stringsAsFactors = F)
teams = read.csv("data/lahman-csv_2015-01-24/Teams.csv", stringsAsFactors = F)
fielding = read.csv("data/lahman-csv_2015-01-24/Fielding.csv", stringsAsFactors = F)
fieldign_group = sqldf('select yearID, playerID, sum(E) as E, max(POS) as POS from fielding group by yearID, playerID')
batting = sqldf('select t.W, t.G as T_G, t.L, t.DivWin, t.WCWin, t.LgWin, t.WSWin, f.POS, f.E, b.* from teams t 
                  inner join batting b on b.teamID = t.teamID and t.yearID = b.yearID
                  inner join fieldign_group f on b.yearID = f.yearID and b.playerID = f.playerID')
                  
batting$AVG = batting$H/batting$AB
batting$PA = rowSums(batting[,c("AB", "BB", "HBP", "SF")], na.rm = T)
batting$EBH = rowSums(batting[,c("X2B", "X3B", "HR")], na.rm = T)
batting$X1B = batting$H - batting$EBH
batting$SLG = (batting$X1B + (batting$X2B*2) + (batting$X3B*3) + (batting$HR*4))/batting$AB
batting$OB = rowSums(batting[,c("H", "BB", "HBP")], na.rm = T)
batting$OBP = batting$OB/batting$PA
batting$OPS = batting$SLG + batting$OBP

nl_rank = c()
j = 1
for(i in years[years<=1974]) {
  print(i)
#   ## AL
#   y_AL = subset(batting, yearID == i & PA > 500 & lgID == "AL")
#   
#   #y_AL$ZPA = ZScore(y_AL$PA)
#   #y_AL$ZR = ZScore(y_AL$R)
#   #y_AL$ZAVG = ZScore(y_AL$AVG)
#   y_AL$RAVG = rank(-y_AL$AVG)
#   #y_AL$ZHR = ZScore(y_AL$HR)
#   y_AL$RHR = rank(-y_AL$HR)
#   #y_AL$ZSLG = ZScore(y_AL$SLG)
#   y_AL$RSLG = rank(-y_AL$SLG)
#   #y_AL$ZOBP = ZScore(y_AL$OBP)
#   y_AL$ROBP = rank(-y_AL$OBP)
#   #y_AL$ZOPS = ZScore(y_AL$OPS)
#   #y_AL$ZSB = ZScore(y_AL$SB)
#   y_AL$RSB = rank(-y_AL$SB)
#   
#   almvp = subset(mvp, yearID == i & lgID == "AL")
#   this_years_mvp = subset(y_AL[,c("lgID", "yearID",  "playerID", "AVG", "OPS", "HR", "RAVG", "RHR", "RSLG", "ROBP", "RSB")], playerID == almvp$playerID)
#   if(j == 1) {
#     al_result = this_years_mvp
#   } else {
#     al_result = rbind(al_result, this_years_mvp)
#   }
  #print(this_years_mvp)
  #al_result = rbind(al_result, this_years_mvp)
  ab_required = round(max(subset(batting, yearID == i & lgID == "AL")$G, na.rm = T)*3.1)
  y_NL = subset(batting, yearID == i & PA > ab_required & lgID == "AL")
  y_NL$ZPA = ZScore(y_NL$PA)
  y_NL$ZR = ZScore(y_NL$R)
  y_NL$ZAVG = ZScore(y_NL$AVG)
  y_NL$RAVG = rank(-y_NL$AVG)
  y_NL$ZHR = ZScore(y_NL$HR)
  y_NL$RHR = rank(-y_NL$HR)
  y_NL$ZSLG = ZScore(y_NL$SLG)
  y_NL$RSLG = rank(-y_NL$SLG)
  y_NL$ZOBP = ZScore(y_NL$OBP)
  y_NL$ROBP = rank(-y_NL$OBP)
  y_NL$ZOPS = ZScore(y_NL$OPS)
  y_NL$ZSB = ZScore(y_NL$SB)
  y_NL$RSB = rank(-y_NL$SB)
  y_NL$ZE = ZScore(-y_NL$E)
  y_NL$RE = rank(y_NL$E)
  
  nlmvp = subset(mvp, yearID == i & lgID == "AL")
  this_years_mvp = subset(y_NL[,c("lgID", "yearID",  "playerID", "W", "L", "AVG", "OPS", "HR", "RAVG", "RHR", "RSLG", "ROBP", "RSB", "ZAVG", "ZHR", "ZSLG", "ZOBP", "ZSB", "ZE")], playerID == nlmvp$playerID)
  m_zs = sqldf('select yearID, max(ZSLG) as ZSLG, max(ZAVG) as ZAVG, max(ZHR) as ZHR, max(ZOBP) as ZOBP, max(ZSB) as ZSB, max(ZE) as ZE from y_NL where W > L')
  if(j == 1) {
    nl_result = this_years_mvp
    max_zscores = m_zs
  } else {
    nl_result = rbind(nl_result, this_years_mvp)
    max_zscores = rbind(max_zscores, m_zs)
  }

  if(j > 5) {
    # Let's figure out who we think should win the MVP
    ## Remove SB number if it is Z
    y_NL[(y_NL[,"ZSB"]<0),]$ZSB = 0
    wgt = sqldf(paste('select avg(z.ZSLG)/avg(m.ZSLG) as WSLG, avg(z.ZAVG)/avg(m.ZAVG) as WAVG, avg(z.ZHR)/avg(m.ZHR) as WHR, avg(z.ZOBP)/avg(m.ZOBP) as WOBP, avg(z.ZSB)/avg(m.ZSB) as WSB
           from max_zscores m inner join nl_result z on m.yearID = z.yearID and z.yearID >= ', i-5, 'and z.yearID <', i))
    
    #candidates = sqldf('select playerID, ZSLG + ZAVG*.75 + ZHR*.5 + ZOBP * .5 + ZSB * .5 + ZE * 0 as SCORE, W, L from y_NL where W > L order by SCORE desc')
    if(is.numeric(wgt$WSLG)) {
      candidates = sqldf(paste('select playerID, ZSLG *', wgt$WSLG, '+ ZAVG*', wgt$WAVG, ' + ZHR*', wgt$WHR, ' + ZOBP * ', wgt$WOBP, ' + ZSB * ', wgt$WSB, ' + ZE * 0 as SCORE, W, L from y_NL where W > L order by SCORE desc'))
    } else {
      candidates = sqldf('select playerID, ZSLG + ZAVG*.75 + ZHR*.5 + ZOBP * .5 + ZSB * .5 + ZE * 0 as SCORE, W, L from y_NL where W > L order by SCORE desc')
    }
    candidates$RANK = rank(-candidates$SCORE)
    print(wgt)
    print(head(candidates))
    print(this_years_mvp)
    player_rank = subset(candidates, playerID == nlmvp$playerID[1])
    if(NROW(player_rank) > 0) {
      nl_rank = c(nl_rank, player_rank$RANK)
    }
  }

  j = j + 1
  #if(i >= 1979) stop("Keep back some data from testing")
}
summary(nl_rank)
plot(nl_rank)
# > # > summary(nl_rank)
#   > # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   > # 1.000   1.000   1.000   3.026   4.000  20.000 
#   
#   > summary(nl_rank)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   4.256   4.750  30.000   
#nl_result

# plot(x = nl_result$yearID, y=nl_result$ZAVG)
# points(x = nl_result$yearID, y=nl_result$ZHR, col="blue")
# points(x = nl_result$yearID, y=nl_result$ZSLG, col="green")
# points(x = nl_result$yearID, y=nl_result$ZOBP, col="red")
# points(x = nl_result$yearID, y=nl_result$ZSB, col="orange")

# plot(x = al_result$yearID, y=al_result$ZAVG)
# points(x = al_result$yearID, y=al_result$ZHR, col="blue")
# points(x = al_result$yearID, y=al_result$ZSLG, col="green")
# points(x = al_result$yearID, y=al_result$ZOBP, col="red")
# points(x = al_result$yearID, y=al_result$ZSB, col="orange")

