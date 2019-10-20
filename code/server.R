########################
# uGeoTagger           #
# by Alessio Benedetti #
# server.R file        #
########################

# load libraries
library(ggmap)
library(googleVis)
library(jsonlite)
library(leaflet.extras)
library(mongolite)
library(rio)
library(shiny)
library(shinyjs)
library(V8)

# set database connections parameters
mongoDBConnection <- fromJSON('www/doc/ATLASmongodb.json')
dbName <- mongoDBConnection$dbName
dbCollection <- mongoDBConnection$dbCollection
dbUrl <- mongoDBConnection$dbUrl

# set the mandatory fields on input window
fieldsMandatory <- c("slug")

# set badge labels
badges <- c('gold','silver','bronze','proven','rated','unrated','N/A')

# load countries and cities data from base xslx file
locations <- "www/doc/world_cities.xlsx" %>% import %>% toJSON %>% fromJSON
countries <- sort(unique(locations$country_name),decreasing = FALSE)
regions <- sort(unique(locations$region),decreasing = FALSE)

# load custom marker icons (under dev)
#uTestIcon <- makeIcon(
#  iconUrl = "www/utest.jpg",
#  iconWidth = 38, iconHeight = 38,
#  iconAnchorX = 22, iconAnchorY = 94#,
#  #shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
#  #shadowWidth = 50, shadowHeight = 64,
#  #shadowAnchorX = 4, shadowAnchorY = 62
#)

# define reverse geocoding function
nearest_coord = function(inputCoords) {
  
  reverse_geocoding <- revgeocode(inputCoords, output = 'more')
  
  country <- reverse_geocoding$country
  
  if (country=="Russia") {country <- "Russian Federation"} 
  
  if (!is.null(reverse_geocoding$administrative_area_level_3)) {
    city <- reverse_geocoding$administrative_area_level_3
  } else if (!is.null(reverse_geocoding$locality)) {
    city <- reverse_geocoding$locality
  } else {city <- "N/A"}
  
  region <- unique(locations[(locations$country_name == country),c(5)]) 
  
  country_code <- tolower(unique(locations[(locations$country_name == country),c(3)]))
  
  # [1] value guarantees that only one record is returned.
  # eg. multiples region records are returned when no city is found for the Russian Federation
  return(list(region[1], country[1], city[1], country_code[1]))
  
}

# define hmtl popup function
uTesterHTML <- function(userData, country_code, timestamp) {
  
  #debug user data
  #userData <- fromJSON('https://www.utest.com/api/v1/users/aspegnol')
  #print(paste0('http://www.crwflags.com/fotw/images/',substr(country_code, 1, 1),'/',country_code,'.gif'))
  
    content <- paste0("<link href='http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.css' rel='stylesheet'>",
                      "<main style=' width:25em;'>",
                      "<h3><a href='https://www.utest.com/profile/",userData$slug,"/stats' target='_blank'>",userData$name,"</a></h3>",
                      "<i>@",userData$slug,"</i>",
                      "<br><i style = 'font-size: 0.85em;'> Created on ",timestamp,"</i>",
                      "<br><i style = 'font-size: 0.85em;'> Updated on ",format(Sys.time(), "%Y-%m-%d %X %Z %z"),"</i>",
                      if (nchar(paste0('http://www.crwflags.com/fotw/images/',substr(country_code, 1, 1),'/',country_code,'.gif'))==44) {
                       paste0("<p><img style = 'border:1px solid black;' src='http://www.crwflags.com/fotw/images/",substr(country_code, 1, 1),"/",country_code,".gif' alt='flag' title='Flag of ",unique(locations[locations$country_code == toupper(country_code),c(4)]),"' width=40>&nbsp;&nbsp;&nbsp;")
                      })
  
  if (is.null(userData$social_network_urls) | !is.null(userData$website)) { 
    
    if (!is.null(userData$social_network_urls$twitter)) {
        content <- paste0(content, "<a href='https://twitter.com/",userData$social_network_urls$twitter,"' target='_blank' alt='twitter' title='Twitter'><i class='fa fa-twitter' aria-hidden='true'></i></a>&nbsp;&nbsp;")
       }
    
    if (!is.null(userData$social_network_urls$linkedin)) {
        content <- paste0(content, "<a href='https://www.linkedin.com/in/", strsplit(userData$social_network_urls$linkedin,"/")[[1]][length(strsplit(userData$social_network_urls$linkedin,"/")[[1]])],"' target='_blank' alt='linkedin' title='Linked In'><i class='fa fa-linkedin' aria-hidden='true'></i></a>&nbsp;&nbsp;")
       }
    
    if (!is.null(userData$website) & userData$website != "") {
        content <- paste0(content, "<a href='",userData$website,"' target='_blank' alt='website' title='Website'><i class='fa fa-globe' aria-hidden='true'></i></a>")
       }
    
  }
                             
    content <- paste0(content,"</p>",
                      if (userData$is_utest_employee) {"<p style = 'font-size: 1em; font-weight: bold; color: white; background: #00a8cc'>uTest Employee</p>"}
                      else if (userData$user_type == "moderator") {"<p style = 'font-size: 0.95em; font-weight: bold; color: black; background: #fdb900'>Moderator</p>"}
                      else if (!is.null(userData$platform_roles$ttl)) {
                       if (userData$platform_roles$ttl) {"<p style = 'font-size: 0.95em; font-weight: bold; color: white; background: #fa8513'>Test Team Lead</p>"}
                       },
                      "<input style='display: none;' id='tab1' type='radio' name='tabs' checked><label for='tab1' style='display: inline-block; text-align: center; margin: 0 0 -1px; padding: 10px 25px;}'><i class='fa fa-user' aria-hidden='true'></i> Bio</label>",
                      if (!is.null(userData$badges)) {
                       "<input style='display: none;' id='tab2' type='radio' name='tabs'><label for='tab2' style='display: inline-block; text-align: center; margin: 0 0 -1px; padding: 10px 25px;}'><i class='fa fa-certificate' aria-hidden='true'></i> Badges</label>"
                      },
                      "<input style='display: none;' id='tab3' type='radio' name='tabs'><label for='tab3' style='display: inline-block; text-align: center; margin: 0 0 -1px; padding: 10px 25px;}'><i class='fa fa-database' aria-hidden='true'></i> Stats</label>",
                      "<section id='content1'>",
                      "<img src = '",userData$avatar_url,"' style=' display:block; width:15em; margin:auto; height:auto; border:3px solid black;'>",
                      "<hr>",
                       "<table style='font-size: 1.2em; margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                           "<td><b>uPoints:</b></td>",
                           "<td style = 'padding-left: 30px;'>",
                           if (is.null(userData$badges$badge_type)) {userData$points}
                           else {sum(userData$badges[2])},
                         "</td>")
   
                      
  if (!is.null(userData$rank)) { 
      
    content <- paste0(content,                    
                         "<tr>",
                           "<td><b>Rank:</b></td>",
                           "<td style = 'padding-left: 30px;'>",userData$rank," / ",userData$global_lowest_rank,"</td>",
                         "</tr>"
                     )
  }
    
    content <- paste0(content,
                         "<tr>",
                           "<td><b>Followers:</b></td>",
                           "<td style = 'padding-left: 30px;'>",userData$follow_stats$follower_count,"</td>",
                         "</tr>",  
                         "<tr>",  
                           "<td><b>Following:</b></td>",    
                           "<td style = 'padding-left: 30px;'>",userData$follow_stats$following_count,"</td>",
                         "</tr>",
                       "</table>"
                     )
                    
  
  if (!is.null(userData$platform_badges)) {
                      
    content <- paste0(content,
                      "<hr>", 
                      "<table>",
                         "<tr>",
                           "<td></td>",
                           "<td><img style = 'margin-left: 30px;' src = 'https://www.utest.com/assets/images/badges/",
                           if (userData$platform_badges$localization == "gold") {"goldBadge.svg"}
                           else if (userData$platform_badges$localization == "silver") {"silverBadge.svg"}
                           else if (userData$platform_badges$localization == "bronze") {"bronzeBadge.svg"}
                           else if (userData$platform_badges$localization == "proven") {"provenBadge.svg"}
                           else if (userData$platform_badges$localization == "rated") {"ratedBadge.svg"}
                           else "unratedBadge.svg",
                           "' height=30 width=30></td>",
                           "<td><img style = 'margin-left: 30px;' src = 'https://www.utest.com/assets/images/badges/",
                           if (userData$platform_badges$usability == "gold") {"goldBadge.svg"}
                           else if (userData$platform_badges$usability == "silver") {"silverBadge.svg"}
                           else if (userData$platform_badges$usability == "bronze") {"bronzeBadge.svg"}
                           else if (userData$platform_badges$usability == "proven") {"provenBadge.svg"}
                           else if (userData$platform_badges$usability == "rated") {"ratedBadge.svg"}
                           else "unratedBadge.svg",
                           "' height=30 width=30></td>",
                           "<td><img style = 'margin-left: 30px;' src = 'https://www.utest.com/assets/images/badges/",
                           if (userData$platform_badges$security == "gold") {"goldBadge.svg"}
                           else if (userData$platform_badges$security == "silver") {"silverBadge.svg"}
                           else if (userData$platform_badges$security == "bronze") {"bronzeBadge.svg"}
                           else if (userData$platform_badges$security == "proven") {"provenBadge.svg"}
                           else if (userData$platform_badges$security == "rated") {"ratedBadge.svg"}
                           else "unratedBadge.svg",
                           "' height=30 width=30></td>",
                           "<td><img style = 'margin-left: 30px;' src = 'https://www.utest.com/assets/images/badges/",
                           if (userData$platform_badges$load == "gold") {"goldBadge.svg"}
                           else if (userData$platform_badges$load == "silver") {"silverBadge.svg"}
                           else if (userData$platform_badges$load == "bronze") {"bronzeBadge.svg"}
                           else if (userData$platform_badges$load == "proven") {"provenBadge.svg"}
                           else if (userData$platform_badges$load == "rated") {"ratedBadge.svg"}
                           else "unratedBadge.svg",
                           "' height=30 width=30></td>",
                           "<td><img style = 'margin-left: 30px;' src = 'https://www.utest.com/assets/images/badges/",
                           if (userData$platform_badges$functional == "gold") {"goldBadge.svg"}
                           else if (userData$platform_badges$functional == "silver") {"silverBadge.svg"}
                           else if (userData$platform_badges$functional == "bronze") {"bronzeBadge.svg"}
                           else if (userData$platform_badges$functional == "proven") {"provenBadge.svg"}
                           else if (userData$platform_badges$functional == "rated") {"ratedBadge.svg"}
                           else "unratedBadge.svg",
                           "' height=30 width=30></td>",
                           "<td></td>",
                         "</tr>",
                         "<tr>",
                           "<td></td>",    
                           "<td><img src = 'https://www.utest.com/assets/images/badges/localizationLarge.svg' height=50 width=50></td>",
                           "<td><img src = 'https://www.utest.com/assets/images/badges/usabilityLarge.svg' height=50 width=50></td>",
                           "<td><img src = 'https://www.utest.com/assets/images/badges/securityLarge.svg' height=50 width=50></td>",
                           "<td><img src = 'https://www.utest.com/assets/images/badges/loadLarge.svg' height=50 width=50></td>",
                           "<td><img src = 'https://www.utest.com/assets/images/badges/functionalLarge.svg' height=50 width=50></td>",
                           "<td></td>",
                         "</tr>",
                       "</table>"
                     )
  }
  
    content <- paste0(content,"<hr></section>")               
                      
  if (!is.null(userData$badges)) {
  
    content <- paste0(content,
                      "<section id='content2'>",
                        if ('at_mention_in_status_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/shoutOut.svg' alt='shoutout' title='Use an @mention in a status' height=50 width=50>"},
                        if ('first_follow_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/1stFollow.svg' alt='1stFollow' title='You followed your first tester' height=50 width=50>"},
                        if ('follow_ten_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/makingFriends.svg' alt='makingFriends' title='You followed 10 testers' height=50 width=50>"},
                        if ('follow_twenty_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/theSubscriber.svg' alt='theSubscriber' title='You followed 20 testers' height=50 width=50>"},
                        if ('follow_thirty_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/feedFrenzy.svg' alt='feedFrenzy' title='You followed 30 testers' height=50 width=50>"},
                        if ('first_follower_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/1stFollower.svg' alt='1stFollower' title='You received your 1st follower' height=50 width=50>"},
                        if ('ten_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/gainingPopularity.svg' alt='gainingPopularity' title='You have 10 followers' height=50 width=50>"},
                        if ('twenty_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/soPopular.svg' alt='soPopular' title='You have 20 followers' height=50 width=50>"},
                        if ('fifty_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/rockStar.svg' alt='rockStar' title='You have 50 followers' height=50 width=50>"},
                        if ('hundred_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/stardom.svg' alt='stardom' title='You have 100 followers' height=50 width=50>"},
                        if ('five_hundred_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/superStardom.svg' alt='superStardom' title='You have 500 followers' height=50 width=50>"},
                        if ('thousand_followers_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/megaStardom.svg' alt='megaStardom' title='You have 1000 followers' height=50 width=50>"},
                        if ('first_response_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/1stResponse.svg' alt='1stResponse' title='Comment gets 1 response' height=50 width=50>"},
                        if ('five_responses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/riseAndShine.svg' alt='riseAndShine' title='Comment gets 5 responses' height=50 width=50>"},
                        if ('six_responses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/gettingWarmer.svg' alt='gettingWarmer' title='Comment gets 6 responses' height=50 width=50>"},
                        if ('twelve_responses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/heatingUp.svg' alt='heatingUp' title='Comment gets 12 responses' height=50 width=50>"},
                        if ('fifty_responses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/onFire.svg' alt='onFire' title='Comment gets 50 responses' height=50 width=50>"},
                        if ('five_likes_three_days_in_a_row_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/respected.svg' alt='respected' title='Leave a comment with at least 5 likes 3 days in a row' height=50 width=50>"},
                        if ('five_status_replies_in_a_row_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/attentive.svg' alt='attentive' title='Reply to a status 5 days in a row' height=50 width=50>"},
                        if ('five_statuses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/shoutItLoud.svg' alt='shoutItLoud' title='Write 5 status updates' height=50 width=50>"},
                        if ('five_statuses_in_a_row_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/outgoing.svg' alt='outgoing' title='Write a status update five days in a row' height=50 width=50>"},
                        if ('ice_breaker_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/iceBreaker.svg' alt='iceBreaker' title='Create a forums topic with at least 5 likes twice in one week' height=50 width=50>"},
                        if ('one_tool_rating_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/1stToolRating.svg' alt='1stToolRating' title='Rate at least 1 tool' height=50 width=50>"},
                        if ('ten_forum_comments_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/intrigued.svg' alt='intrigued' title='Authored 10 forum posts' height=50 width=50>"},
                        if ('ten_forum_responses_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/intrigued.svg' alt='intrigued' title='Replied to 10 forum topics' height=50 width=50>"},
                        if ('ten_likes_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/writingContest.svg' alt='writingContest' title='Have your authored content liked 10 times' height=50 width=50>"},
                        if ('fifty_likes_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/bestOfNet.svg' alt='bestOfNet' title='Have your authored content liked 50 times' height=50 width=50>"},
                        if ('two_hundred_fifty_likes_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/pulitzerPrize.svg' alt='pulitzerPrize' title='Have your authored content liked 250 times' height=50 width=50>"},
                        if ('thousand_likes_badge' %in% userData$badges$badge_type)
                          {"<img src = 'https://www.utest.com/assets/images/badges/nobelPrize.svg' alt='nobelPrize' title='Have your authored content liked 1000 times' height=50 width=50>"},
                        "<hr>",
                      "</section>"
                     )
  }
    
    
  if (!is.null(userData$site_stats)) {
      
    content <- paste0(content,
                      "<section id='content3'>",
                        "<table style='font-size: 1.2em; margin-left:auto; margin-right:auto;'>",
                          "<tr>",
                            "<td><b>Joined at:</b></td>",
                            "<td style = 'padding-left: 30px;'>",
                              paste0(substr(userData$site_stats$joined_at,9,10)," ",month.abb[as.numeric(substr(userData$site_stats$joined_at,6,7))]," ",substr(userData$site_stats$joined_at,1,4)),
                            "</td>",
                          "</tr>",
                          "<tr>",
                            "<td><b>Last login:</b></td>",
                            "<td style = 'padding-left: 30px;'>",
                              paste0(substr(userData$site_stats$last_login_at,9,10)," ",month.abb[as.numeric(substr(userData$site_stats$last_login_at,6,7))]," ",substr(userData$site_stats$last_login_at,1,4)),
                            "</td>",
                          "</tr>",
                          "<tr>",
                            "<td><b>Last comment:</b></td>",
                            "<td style = 'padding-left: 30px;'>",
                              paste0(substr(userData$site_stats$last_commented_at,9,10)," ",month.abb[as.numeric(substr(userData$site_stats$last_commented_at,6,7))]," ",substr(userData$site_stats$last_commented_at,1,4)),
                          "</td>",
                          "</tr>",
                        "</table>",
                        "<hr>",
                        "<table style='font-size: 1.2em; margin-left:auto; margin-right:auto;'>",
                          "<tr>",
                            "<td><b>Posts per month:</b></td>",
                            "<td style = 'padding-left: 30px;'>",userData$site_stats$posts_per_month,"</td>",
                          "</tr>",
                          "<tr>",
                            "<td><b>Forum topics:</b></td>",
                            "<td style = 'padding-left: 30px;'>",userData$site_stats$topics,"</td>",
                          "</tr>",
                          "<tr>",
                            "<td><b>Test cycles:</b></td>",
                            "<td style = 'padding-left: 30px;'>",userData$site_stats$test_cycles,"</td>",
                          "</tr>",
                        "</table>",
                        "<hr>",
                        "<table style='font-size: 1.2em; margin-left:auto; margin-right:auto;'>",
                          "<tr>",
                            "<td><b>Id:</b></td>",
                            "<td style = 'padding-left: 30px;'>",userData$id,"</td>",
                          "</tr>",
                          "<tr>",
                            "<td><b>Platform Id:</b></td>",
                            "<td style = 'padding-left: 30px;'>",userData$platform_id,"</td>",
                          "</tr>",
                        "</table>",
                      "<hr>",
                      "</section>"
      )
  }  
  
  
  #debug HTML content
  #fileConn<-file(paste0(userData$slug,".html"))
  #writeLines(content, fileConn)
  #close(fileConn)
    
  return(content)
  
} # END uTesterHTML


# DEFINE SERVER LOGIC
shinyServer(function(input, output) {

 #######################
 ## SUPPORT FUNCTIONS ##
 #######################
  
 options(warn=2, shiny.error=recover)  
  
 # function that refreshes map and stats data  
 refreshData = function() {
 
  # open/close mongodb connection in order to save data in a dataframe
  dbConnection <- mongo(db = dbName, collection = dbCollection, url = dbUrl)
  geoData <- dbConnection$find()
  rm(dbConnection)
  gc()
  
  # load the map
  #if (!is.null(geoData$slug)) { #--> this chunk to be enabled whenever the DB collection is fully erased
    
   output$map <- renderLeaflet({
    leaflet(data=geoData) %>% addTiles(group = "Open Street Map (default)", options = providerTileOptions(noWrap = TRUE)) %>%
                              addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
                              addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
                              addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map", options = providerTileOptions(noWrap = TRUE)) %>%
                              addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
                              addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
                              addFullscreenControl() %>%
                              addMarkers(~long, ~lat, popup = ~html, clusterOptions = markerClusterOptions(freezeAtZoom = 5)) %>%
                              addLayersControl(
                               baseGroups = c("Open Street Map (default)", "Stamen Watercolor", "Nasa Earth at Night","Open Topo Map","Stamen Terrain Background","Esri World Imagery"),
                               position = c("topleft"),
                               options = layersControlOptions(collapsed = FALSE)
                              )
   })
  
  # load the charts
  
  # chart1: dynamic bar chart count by region, country or city (top 5)
   chart1 <- reactive(
    if (input$chart1Select == 'Regions')
     {
      chart1Temp <- aggregate(geoData[,c("region")], by=list(geoData$region), FUN=length)
      colnames(chart1Temp)[1] <- "Region"
      colnames(chart1Temp)[2] <- "Count"
      chart1Temp <- head(chart1Temp[order(-chart1Temp[,2],chart1Temp[,1]),])
     }
    else if (input$chart1Select == 'Countries')
     {
      chart1Temp <- aggregate(geoData[,c("country")], by=list(geoData$country), FUN=length)
      colnames(chart1Temp)[1] <- "Country"
      colnames(chart1Temp)[2] <- "Count"
      chart1Temp <- head(chart1Temp[order(-chart1Temp[,2],chart1Temp[,1]),])
     }
    else if (input$chart1Select == 'Cities')
     {
      chart1Temp <- aggregate(geoData[,c("city")], by=list(geoData$city), FUN=length)
      colnames(chart1Temp)[1] <- "City"
      colnames(chart1Temp)[2] <- "Count"
      chart1Temp <- head(chart1Temp[order(-chart1Temp[,2],chart1Temp[,1]),])
     }
   )
  
   barColor <- reactive(
    if (input$chart1Select == 'Regions') {barColorTemp <- "green"}
    else if (input$chart1Select == 'Countries') {barColorTemp <- "blue"}
    else if (input$chart1Select == 'Cities') {barColorTemp <- "red"}
   )
   
   output$chart1Chart <- renderGvis({
    gvisBarChart(chart1(), options=list(title="uTester's count by geography", legend="none", series=paste0("[{targetAxisIndex:0, color:'",barColor(),"'}]"), height=400))
   })
  
   
   # chart2: rating badges count
   loc_no <- c(nrow(geoData[geoData$localization=='gold',]),
               nrow(geoData[geoData$localization=='silver',]),
               nrow(geoData[geoData$localization=='bronze',]),
               nrow(geoData[geoData$localization=='proven',]),
               nrow(geoData[geoData$localization=='rated',]),
               nrow(geoData[geoData$localization=='unrated',]),
               nrow(geoData[geoData$localization=='N/A',]))
    
   usa_no <- c(nrow(geoData[geoData$usability=='gold',]),
               nrow(geoData[geoData$usability=='silver',]),
               nrow(geoData[geoData$usability=='bronze',]),
               nrow(geoData[geoData$usability=='proven',]),
               nrow(geoData[geoData$usability=='rated',]),
               nrow(geoData[geoData$usability=='unrated',]),
               nrow(geoData[geoData$usability=='N/A',]))

   sec_no <- c(nrow(geoData[geoData$security=='gold',]),
               nrow(geoData[geoData$security=='silver',]),
               nrow(geoData[geoData$security=='bronze',]),
               nrow(geoData[geoData$security=='proven',]),
               nrow(geoData[geoData$security=='rated',]),
               nrow(geoData[geoData$security=='unrated',]),
               nrow(geoData[geoData$security=='N/A',]))
   
   loa_no <- c(nrow(geoData[geoData$load=='gold',]),
               nrow(geoData[geoData$load=='silver',]),
               nrow(geoData[geoData$load=='bronze',]),
               nrow(geoData[geoData$load=='proven',]),
               nrow(geoData[geoData$load=='rated',]),
               nrow(geoData[geoData$load=='unrated',]),
               nrow(geoData[geoData$load=='N/A',]))
   
   fun_no <- c(nrow(geoData[geoData$functional=='gold',]),
               nrow(geoData[geoData$functional=='silver',]),
               nrow(geoData[geoData$functional=='bronze',]),
               nrow(geoData[geoData$functional=='proven',]),
               nrow(geoData[geoData$functional=='rated',]),
               nrow(geoData[geoData$functional=='unrated',]),
               nrow(geoData[geoData$functional=='N/A',]))
   
   chart2Temp <- data.frame(badges, loc_no, usa_no, sec_no, loa_no, fun_no)
  
   chart2 <- reactive({
   
    if (input$chart2Select == 'Localization') {chart2Temp <- chart2Temp[,c(1,2)]}
    else if (input$chart2Select == 'Usability') {chart2Temp <- chart2Temp[,c(1,3)]}
    else if (input$chart2Select == 'Security') {chart2Temp <- chart2Temp[,c(1,4)]}
    else if (input$chart2Select == 'Load') {chart2Temp <- chart2Temp[,c(1,5)]}
    else if (input$chart2Select == 'Functional') {chart2Temp <- chart2Temp[,c(1,6)]}
   
   })
  
  
   output$chart2Chart <- renderGvis({
    
    gvisPieChart(chart2(), options=list(title="Rating badges per specialization",
                                                 slices= "{
                                                           0: {color: '#d4af37'},
                                                           1: {color: '#c0c0c0'},
                                                           2: {color: '#cd7f32'},
                                                           3: {color: '#c8a2c8'},
                                                           4: {color: '#009900'},
                                                           5: {color: '#4d4dff'},
                                                           6: {color: '#4c4c4c'}
                                                          }",
                                                 is3D =TRUE, height=400)
                                                )
  
   })

  
   # chart3: tagged uTesters count by day
   # fix 1.1: corrected the chart timeline
   chart3 <- aggregate(geoData[,c("slug")], by=list(as.Date(geoData$timestamp)), FUN=length)
   colnames(chart3)[1] <- "Date"
   colnames(chart3)[2] <- "Count"
   chart3 <- cbind(chart3,format(as.Date(chart3$Date),"%d %b %Y"))
   colnames(chart3)[3] <- "Formatted Date"
  
   output$chart3Chart <- renderGvis({
    gvisLineChart(chart3[,c(3,2)], options=list(title=paste0("Registered uTesters (",length(geoData$slug)," currently tagged)"), legend="none", height=400))
   })
 
  #} #--> this chunk to be enabled whenever the DB collection is fully erased
  
 }
 
  # function that fills the "Country" combobox of the map tab
 output$countriesMap <- renderUI({
   
  selectInput("countryMap","Country:", countries)
   
 })
 
 
 # function that fills the "City" combobox of the map tab once a country is selected
 observeEvent(input$countryMap, {
   
  output$citiesMap <- renderUI({ 
   
   selectInput("cityMap","City:", sort(unique(locations[locations$country_name == input$countryMap, c(2)], decreasing = FALSE)))
    
  })
   
 })
 

 # event that checks if all mandatory fields have a value 
 observe({
  mandatoryFilled <-
   vapply(fieldsMandatory,
   function(x) {
    !is.null(input[[x]]) && input[[x]] != ""
   },
   logical(1))
   mandatoryFilled <- all(mandatoryFilled)
    
   # enable/disable the submit button
   shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
   
 })
 
 
 # event that shows/hides location/coordinates fields and checks latitude and longitude values
 observeEvent(input$position, {
   
  if (input$position == "loc") {
   show(id = "location", anim = TRUE)
   hide(id = "coordinates", anim = TRUE)
   hide(id = "checkLong", anim = TRUE)
   hide(id = "checkLat", anim = TRUE)
  } else {
   hide(id = "location", anim = TRUE)
   show(id = "coordinates", anim = TRUE)
    
    # event that checks if latitude is between -90 and 90
    observeEvent(input$lat, {
    
     if (input$lat > 90 | input$lat < -90 | is.na(input$lat) | input$lat == '') {
      show(id = "checkLat", anim = TRUE)
     } else {
      hide(id = "checkLat", anim = TRUE)
     }
      
    })
    
    # event that checks if longitude is between -180 and 180
    observeEvent(input$long, {
       
     if (input$long > 180 | input$long < -180 | is.na(input$long) | input$long == '') {
      show(id = "checkLong", anim = TRUE)
     } else {
      hide(id = "checkLong", anim = TRUE)
     }
       
    })
    
  }
  
 })

 
 # event that reset input elements in the flying board
 observeEvent(input$reset, {
   
  reset("slug")
  reset("lat")
  reset("long")
  reset("countriesMap")
  reset("citiesMap")
   
 })
 
 
 ########## 
 ## CORE ##
 ##########
 
 # load the data while entering the app
 refreshData()

 # event that manages the "Submit" button action
 observeEvent(
  input$submit,
  {
    
   tryCatch(
    
    {
   
     # load preliminary data (uTester JSON, coordinates if a city is entered and build the upsert statement)  
     
     cleanSlug <- tolower(trimws(input$slug))
      
     userJSON <- fromJSON(paste0('https://www.utest.com/api/v1/users/',cleanSlug))
     
     # open mongodb connection in order retrieve the creation timestamp & execute the upsert statement
     dbConnection <- mongo(db = dbName, collection = dbCollection, url = dbUrl)
     # fix 1.3: the chart timeline now refers to the creation timestamp
     
     if (nrow(dbConnection$find(paste0('{"slug":"',cleanSlug,'"}'),'{"timestamp": true}')) == 0) {
       creation_timestamp <- format(Sys.time(), "%Y-%m-%d %X %Z %z")
     }
     else {
       creation_timestamp <- dbConnection$find(paste0('{"slug":"',cleanSlug,'"}'),'{"timestamp": true}')[,c(2)]  
     }
     
     str <- paste0('{',
                   if (input$position == "loc") {
                     
                     coords <- geocode(paste(input$countryMap, input$cityMap))
                     paste0(
                      '"region":"', unique(locations[(locations$country_name == input$countryMap) & (locations$city_name == input$cityMap),c(5)]),
                      '","country":"', input$countryMap,
                      '","city":"', input$cityMap,
                      '","lat":', coords$lat,
                      ',"long":', coords$lon,
                      ',"position":"location"',
                      ',"html":"', uTesterHTML(userJSON, tolower(unique(locations[(locations$country_name == input$countryMap),c(3)])),creation_timestamp),'"'
                     )
                     
                   } else {
                     
                     find_nearest_coord <- nearest_coord(c(input$long, input$lat))
                     paste0(
                      '"region":"', find_nearest_coord[[1]],'",
                      "country":"',find_nearest_coord[[2]],'",
                      "city":"',find_nearest_coord[[3]],'",
                      "lat":', input$lat,',
                      "long":', input$long,
                      ',"position":"coordinates"',
                      ',"html":"', uTesterHTML(userJSON,find_nearest_coord[[4]],creation_timestamp),'"'
                     )
                     
                   }
                  )
     
     str <- paste0(str, 
                   if (!is.null(userJSON$platform_badges)) {
                     
                     paste0(
                      ',"localization":"', userJSON$platform_badges$localization,
                      '","usability":"', userJSON$platform_badges$usability,
                      '","security":"', userJSON$platform_badges$security,
                      '","load":"', userJSON$platform_badges$load,
                      '","functional":"', userJSON$platform_badges$functional, '"}'
                     )
                     
                   } else {
                     
                     paste0(
                       ',"localization":"N/A',
                       '","usability":"N/A',
                       '","security":"N/A',
                       '","load":"N/A',
                       '","functional":"N/A"}'
                     )
                     
                   }
                  )

     # debug upsert string
     #print(str)
     # or
     #fileConn<-file(paste0(cleanSlug,".txt"))
     #writeLines(str, fileConn)
     #close(fileConn)
     
     # timestamp is not updated but only inserted
     dbConnection$update(paste0('{"slug":"',cleanSlug,'"}'), paste0('{"$setOnInsert":{"timestamp": "',format(Sys.time(), "%Y-%m-%d %X %Z %z"),'"},','"$set":',str,'}'), upsert = TRUE)
     
     # close mongodb connection
     rm(dbConnection)
     gc()
     
     # information popup for the user
     shinyjs::js$swal("Success!", paste("User", cleanSlug," has been tagged on the map.\n\n Should you need to edit the assigned location, simply submit again a new city (or coordinate)."), "success")
     #print("Success! Row appended.")
     
     # refresh the data after the upsert
     refreshData()
    
    },
   
   error = function(err) 
   
    {
    
     # information popup for the user
     shinyjs::js$swal("Error!", paste("User", cleanSlug,"has not been tagged on the map.\n\n Here's the logged error:", "\n",err), "error")
     #print(paste0("Attention! Row not appended. Here's the error:", err))
    
    }
  
   )
  }
 )#END observeEvent
  
})