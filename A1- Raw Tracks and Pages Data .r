
# Loading A1-1_pages.csv 
pages_dataset<-read.csv("~/Desktop/Brandless/brandless_take_home_exercise_data/A1-1_pages.csv",header =TRUE)

# Top 10 Visited pages
top_10_visited_pages<-sqldf("SELECT count(path) as Count,Path from pages_dataset group by 2 order by 1 desc limit 10")

# Re-Ordering
top_10_visited_pages$path<-factor(top_10_visited_pages$path,levels = c("/","/category/food","/shop_all","/category/home-and-office","/category/beauty","/category/personal-care","/category/household-supplies","/category/health","/about","/checkout/email"))

# Top 10 pages visited in a ggplot
ggplot(top_10_visited_pages,aes(x=path,y=Count,label=Count))+labs(title="Top 10 pages visited",x="Path",y="# of times visited")+theme(axis.text.x = element_text(angle =45,vjust = 0.5))+geom_bar(stat="identity", width = 0.5, aes(fill=path))+geom_text(hjust=0.09,angle=45)+theme(text=element_text(size=10, family="Comic Sans MS"))

# Loading A1-1_tracks.csv 
tracks_dataset<-read.csv("~/Desktop/Brandless/brandless_take_home_exercise_data/A1-1_tracks.csv",header =TRUE)

# Joining Tracks and Pages
tracks_and_pages_joined<-sqldf("SELECT count(a.event) as count, a.event as event, b.path FROM tracks_dataset a INNER JOIN top_10_visited_pages b ON a.context_page_path=b.path GROUP BY 2,3 order by count desc,path")

# Grouping and Ranking
tracks_and_pages_Rank<-tracks_and_pages_joined %>% group_by(path) %>% mutate(ranks=order(count,decreasing = TRUE))

# Top 5 tracks from Top 10 most visited pages
top_5_tracks<-filter(tracks_and_pages_Rank,ranks<6)

# Re-Ordering
top_5_tracks<-top_5_tracks %>% group_by(path) %>% arrange(desc(count),.by_group = TRUE)

# Visualizing Top 5 Events in Top 10 pages Visited 
ggplot(top_5_tracks,aes(x=path,y=count,fill=event))+geom_bar(stat = "identity",position="fill")+theme(axis.text.x = element_text(angle =45,vjust = 0.5))+theme(text=element_text(size=10, family="Comic Sans MS"))+scale_fill_brewer(palette="Spectral")+labs(title="Top 5 Events Tracked",x="Path",y="# of times events Clicked")

# Visualizing Top 5 Events in Top 10 pages Visited  by Percentage
ggplot(top_5_tracks,aes(x=path,y=count,fill=event))+geom_bar(stat = "identity",position="fill")+theme(axis.text.x = element_text(angle =45,vjust = 0.5))+theme(text=element_text(size=10, family="Comic Sans MS"))+scale_fill_brewer(palette="Spectral")+labs(title="Top 5 Events Tracked by Percentage",x="Path",y="# of times events Clicked by Percentage")+scale_y_continuous(labels = percent_format())

#Scaling to  events to 100% and then retrieving individual event percentages
top_5_tracks<-sqldf("SELECT b.sum as total_per_path,a.count as track_count,a.event,a.path as path,a.ranks as ranks FROM (Select count as count, event as event,path as path, ranks as ranks from top_5_tracks) as a, (Select Sum(count) as Sum,path from top_5_tracks   group by path ) as b where a.path=b.path ")

top_5_tracks$percentage <-paste(round((top_5_tracks$track_count/top_5_tracks$total_per_path)*100,1),"%")