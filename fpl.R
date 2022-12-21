### Expected Points Program for FPL
library(jsonlite);library(dplyr)


#Read in H2H data
h2h.results = NULL
for(p in 1:10) h2h.results = rbind(h2h.results,fromJSON(paste0("https://fantasy.premierleague.com/api/leagues-h2h-matches/league/54440/?page=",p))$results)
last.gw = max(h2h.results$event[h2h.results$entry_1_points>0])
for(gw in 1:last.gw) gw.h2h.results = select(subset(h2h.results,event==gw),entry_1_player_name,entry_1_points,entry_2_player_name,entry_2_points)

## Collect all scores and weekly ranks
scores = ranks = opps = matrix(NA,26,last.gw+1)
for(gw in 1:last.gw){
  gw.h2h.results = select(subset(h2h.results,event==gw),entry_1_player_name,entry_1_points,entry_2_player_name,entry_2_points)
  scr = rbind(as.matrix(gw.h2h.results[,1:2]),as.matrix(gw.h2h.results[,3:4]))
  if(gw==1) scores[,1] = ranks[,1] = opps[,1] = scr[order(scr[,1]),1]
  scores[,gw+1] = cbind(as.numeric(scr[order(scr[,1]),2]))
  ranks[,gw+1] = rank(-as.numeric(scores[,gw+1]),ties.method="min")  
  opps[match(gw.h2h.results$entry_1_player_name,scores[,1]),gw+1] = gw.h2h.results$entry_2_player_name
  opps[match(gw.h2h.results$entry_2_player_name,scores[,1]),gw+1] = gw.h2h.results$entry_1_player_name
}

#Points per GW
pts = sapply(1:last.gw,function(gw) sapply(1:nrow(scores),function(p) mean(3*(as.numeric(scores[p,gw+1])>as.numeric(scores[match(opps[p,gw+1],scores[,1]),gw+1]))+1*(as.numeric(scores[p,gw+1])==as.numeric(scores[match(opps[p,gw+1],scores[,1]),gw+1])))))
#Expected Points Per Gameweek
xpts = sapply(1:last.gw,function(gw) sapply(1:nrow(scores),function(p) mean(3*(as.numeric(scores[p,gw+1])>as.numeric(scores[-p,gw+1]))+1*(as.numeric(scores[p,gw+1])==as.numeric(scores[-p,gw+1])))))
#Delta xPt
rowSums(pts-xpts)

#Opponents Rank and Expected Points
opp.ranks = sapply(1:last.gw,function(gw) sapply(1:nrow(ranks),function(p) as.numeric(ranks[match(opps[p,gw+1],ranks[,1]),gw+1])))
rowMeans(opp.ranks)
opp.xpts = sapply(1:last.gw,function(gw) sapply(1:nrow(ranks),function(p) xpts[match(opps[p,gw+1],ranks[,1]),gw]))
rowSums(opp.xpts)

#Add average opponent's current rank (played top of the league)
tot.pts = rowSums(pts)
tot.score = apply(scores[,-1],1,function(x) sum(as.numeric(x)))
curr.rank = order(tot.pts,tot.score,decreasing = TRUE)
opp.curr.ranks = sapply(1:last.gw,function(gw) sapply(1:nrow(ranks),function(p) as.numeric(curr.rank[match(opps[p,gw+1],ranks[,1])])))
rowMeans(opp.curr.ranks)


#SoS (average opponent score rank)
tot.score.rank = order(tot.score,decreasing = TRUE)
sos = rowMeans(sapply(1:last.gw,function(gw) sapply(1:nrow(ranks),function(p) as.numeric(tot.score.rank[match(opps[p,gw+1],ranks[,1])])))) - mean(1:length(tot.score))

#Opponent Delta (i.e. actual performance relative to expected performance measured by xpts)
opp.actual.xpts = sapply(1:last.gw,function(gw) sapply(1:nrow(ranks),function(p) xpts[match(opps[p,gw+1],ranks[,1]),gw]))
opp.rank.xpts = sapply(1:length(tot.score),function(p) mean(3*(tot.score[p]>tot.score[-p])+1*(tot.score[p]==tot.score[-p])))
opp.xpt.delta = rowMeans(opp.actual.xpts - opp.rank.xpts)

## Compile
#Construct results table
res = cbind(1:nrow(ranks),tot.pts[curr.rank],tot.score[curr.rank],rank(-rowSums(xpts))[curr.rank],round(rowSums(xpts)[curr.rank],1),round(rowSums(pts-xpts)[curr.rank],1),
            round(opp.xpt.delta[curr.rank],1),round(sos[curr.rank],1))
rownames(res) = ranks[curr.rank,1]
colnames(res) = c("Rank","Pts","Score","xRank","xPts","Your Delta","Ave Opponent Delta","SoS")
res
write.table(res,sep=";",quote=F)





####Old code using points league data

#Read in league json
league.data = fromJSON("https://fantasy.premierleague.com/api/leagues-h2h/54440/standings/")
people = league.data$standings$results[,"player_name"]
teams = league.data$standings$results[,"entry_name"]
ids = league.data$standings$results[,"entry"]
pt = league.data$standings$results[,"total"]
team.average = which(people=="AVERAGE")
people = people[-team.average]; teams = teams[-team.average]; ids = ids[-team.average]; pt = pt[-team.average];
#Should probably work in AVERAGE appropriately

#Read in gameweek scores for each team id and calculate actual score that week
gw.scores = NULL
for(i in 1:length(people)){
  team.data = fromJSON(paste("https://fantasy.premierleague.com/api/entry/",ids[i],"/history/",sep=""))
  bench.boost = rep(0,nrow(team.data$current))
  if(!is.null(dim(team.data$chips))) if(any(team.data$chips[,"name"] == "bboost")) bench.boost[team.data$chips[which(team.data$chips[,"name"] == "bboost"),"event"]] = 1
  foo = team.data$current[,"points"] - team.data$current[,"event_transfers_cost"] + bench.boost*team.data$current[,"points_on_bench"]
  gw.scores = rbind(gw.scores,foo)
}
rownames(gw.scores) = people; colnames(gw.scores) = 1:ncol(gw.scores)


#Calculate expected points per week
xpt = round(cbind(rowSums(sapply(1:ncol(gw.scores),function(g) sapply(1:length(people),function(i) mean(3*(gw.scores[i,g]>gw.scores[-i,g])+1*(gw.scores[i,g]==gw.scores[-i,g])))))),1)

#Construct results table
res = cbind(1:length(people),rank(-xpt)[order(pt,decreasing=TRUE)],sort(pt,decreasing=TRUE),xpt[order(pt,decreasing=TRUE)],round(sort(pt,decreasing=TRUE)-xpt[order(pt,decreasing=TRUE)],1))
rownames(res) = paste(teams[order(pt,decreasing=TRUE)]," (",people[order(pt,decreasing=TRUE)],")",sep="")
colnames(res) = c("Rank","xRank","Pts","xPts","Luck (Pts-xPts)")
res
write.table(res,sep=";",quote=F)

#HTML Table
library("tableHTML")
tableHTML(res) %>% add_css_column(css = list('text-align', 'center'), columns = 1:6) 
#Could email #gm_create_draft(xx)

#Write out gw.scores somewhere
tot = rowSums(gw.scores)
rev(sort(tot))
