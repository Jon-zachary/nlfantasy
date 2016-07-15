#Lahman Database preparation
library(Lahman)
library(dplyr)
#-Merge listings of players on multiple teams within the same year-
Batting %>% group_by(playerID,yearID) %>% 
summarise(teamID=first(teamID),lgID=first(lgID),G=sum(G),AB=sum(AB),H=sum(H),R=sum(R),RBI=sum(RBI),SB=sum(SB),CS=sum(CS),X2B=sum(X2B),X3B=sum(X3B),HR=sum(HR),BB=sum(BB),IBB=sum(IBB),SH=sum(SH),SF=sum(SF),HBP=sum(HBP),GIDP=sum(GIDP))->bat
#-Set NA's to zero-
bat[is.na(bat)]<-0
#-Add a singles column-
bat %>% mutate(X1B=H-X2B-X3B-HR)->bat
#-Prepare the fielding df for merge-
Fielding %>% group_by(playerID,yearID) %>% summarise(E=sum(E),POS=first(POS))->field
field[is.na(field)]<-0
#-merge bat and field, the new df has errors and position-
right_join(bat,field)->bat
#-Add NL fantasy points-
bat %>% mutate(points=X1B+R+RBI+BB+SB+(2*X2B)+(4*X3B)+(4*X3B)+(4*HR)-E)->bat
#-remove pitchers-
bat %>% filter(!(POS=='P'))->bat
#-prepare the master to merge with batting for full names and birthday-
Master %>% select(playerID,nameFirst,nameLast,birthDate)->mast
mast %>% mutate(Name=paste(nameFirst,nameLast)) %>% select(-nameFirst,-nameLast)->mast
mast$birthDate<-as.Date(mast$birthDate)
#-Remove NA's only .025% and they're all super old timey so nbd-
mast<-na.omit(mast)
#-merge master and batters-
merge(bat,mast)->bat


