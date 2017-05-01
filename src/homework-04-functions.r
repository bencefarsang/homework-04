  #
#                    Házi feladat 4     
#                    Programozás I.     
#                  2016/17. II. félév 
#                     Farsang Bence      
#                      2017.04.25.
#
#                      FÜGGVÉNYEK
#
#------------------------------------------------------
#                       II.feladat
#------------------------------------------------------

popular_tweets <- function(name= "Hillary Clinton" , number=3) {
  
  # error message, if the input is wrong
  if( name != "Donald Trump" & name != "Hillary Clinton" ) { 
    
    # error message
    stop('Az elnökjelölt neve hibásan lett megadva.')
    
   } else {
  
     # sum of retweets and favorites
     for (i in 1: dim(tweets)[1]){
       tweets$likes[i] <- tweets$retweet_count[i] + tweets$favorite_count[i]
     }
     
    # if Clinton
  
  if( name == "Hillary Clinton" ) {
    
    # new Clinton only dataset
         tweets2 <- tweets[!(tweets$handle == "realDonaldTrump"),]
    # sorting largest to smallest
    index_sorted <- sort(tweets2$likes,  decreasing = TRUE, index.return = TRUE)$ix
    
     # if Trump 
  } else {
    
    # new Trump only dataset
         tweets2 <- tweets[!(tweets$handle == "HillaryClinton"),]
    # sorting largest to smallest
    index_sorted <- sort(tweets2$likes,  decreasing = TRUE, index.return = TRUE)$ix
    
  } 
     # if the asked number of tweets is out of range
     capture.output(ifelse(number > length(index_sorted), number <- length(index_sorted), number), file= 'NUL')
     
     # print
      sprintf("%s %i legnépszerűbb tweetje:", name, number)
      print(tweets2$text[index_sorted[1:number]])
   }}

#------------------------------------------------------
# EXTRA FUNCTION for exercise III/1.
#------------------------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}