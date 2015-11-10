#' Scrapes Batting or Pitching Leaders from Baseball-Reference
#'
#' Takes the url for a Baseball-References leaderboard and scrapes the data over
#' a specified timeframe
#' @param yrs numeric. Vector of the years to scrape the data from
#' @param type Whether to scrape batting or pitching data.  Defaults to batting
#' @param bref_base_url character containing the url with the correct
#'   Baseball-Reference leaderboard.  Defaults to Japanese leaders.
#' @details Pastes \code{yrs} to the end of \code{brefBaseURL} to create a
#'   vectors of urls.  Then scrapes the first table at each of these urls using
#'   \code{\link{readHTMLTable}}.  Then binds the table for each year using
#'   \code{\link{rbind_all}} and returns these stacked tables as a
#'   \code{\link{tbl_df}} object.
#' @return \code{tbl_df} of data scraped from Baseball-Reference.
#' @examples
#' # batters
#' x <- scrape_bref(2005:2014)
#' # pitchers
#' y <- scrape_bref(2005:2014, type = "pit")
scrape_bref <- function(yrs, type = c("bat", "pit"), brefBaseURL = "http://www.baseball-reference.com/japan/leader.cgi?type=bat&year=") {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "pit") brefBaseURL <- stringr::str_replace_all(brefBaseURL, "bat", "pit")
  urls <- paste0(brefBaseURL, yrs)
  downloads <- lapply(urls, function(link) {
    z <- XML::readHTMLTable(link, which = 1, header = T, stringsAsFactors = F)
    z$Year <- substr_right(link, 4)
    z
  })
  downloads[sapply(downloads, is.data.frame)] %>%
    rbind_all %>%
    tbl_df() %>%
    mutate_each(funs(x = stringr::str_replace_all(., ",", "-")), Tm:Year)
}

#' Clean Japanese Leaders Downloaded from Baseball-Reference
#'
#' Takes the Japanese data scraped from Baseball-Reference using
#' \code{\link{scrape_bref}} and cleans it so that it can be combined with
#' the data downloaded from PIA.  Only used to process data for the apps.
#' @param dat data.frame.  Data scraped from Baseball-Reference
#' @param j_ids data.frame of japanese ids for players who have played in both
#'   Japan and America.  The j_ids are custom, used only in this app
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return tbl_df of japanese statistics that have been cleaned.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' dat <- read.csv("0-downloads/bat_japan.csv", header = T, stringsAsFactors = F)
#' jids <- read.csv("manual-info/japan_ids.csv", header = T, stringsAsFactors = F)
#' y <- clean_japan(dat, jids, "bat")
#' setwd(curr_wd)
clean_japan <- function(dat, j_ids, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  clean1 <- dat %>%
    mutate(display_name = j_display_name(Name),
           lastf = getLastF(display_name)) %>%
    group_by(display_name, lastf) %>%
    mutate(id_num = Name %>% as.factor() %>% as.numeric(),
           j_id = paste0(lastf, id_num)) %>%
    ungroup() %>%
    mutate(ML.Org = Tm,
           Level = Lg,
           K = SO)

  # Add players without MLBIDs to biographical files with dummy heights, weights so that they show up

#   if (type == "bat") {
#     bio <- read.csv("N:/Apps/simScoresApp/data/manual-info/bio_bat.csv", header = T);
#     positions <- read.csv("N:/Apps/simScoresApp/data/manual-info/positions.csv", header = T);
#
#     for (i in 1:nrow(clean1)) {
#       if (clean1[i, "j_id"] %in% bio$MLBID) {
#         # do nothing
#         print(paste0(clean1[i, "j_id"], " exists in bio_bat"));
#       }
#       else {
#         #rbind(bio, data.frame(MLBID = clean2[i, 34], Name = clean2[i, 2], Height = 0, Weight = 0, Bats = "n/a", Throws = "n/a", Date.of.Birth = "n/a", ROO = "n/a", Start.Year = 1900));
#         new.df <- data.frame(MLBID = clean1[i, 35], Name = clean1[i, "Name"], Height = 100, Weight = 100, Bats = "Both", Throws = "Both", Date.of.Birth = "1/1/1990", ROO = "Asia", Start.Year = 2006)
#         names(new.df) <- c("MLBID", "Name", "Height", "Weight", "Bats", "Throws", "Date.of.Birth", "ROO", "Start.Year");
#         bio <- rbind(bio, new.df);
#         #print(paste0(clean1[i, "j_id"], " added to bio_bat"));
#       }
#
#     }
#     bio <- dplyr::select(bio, MLBID, Name, Height, Weight, Bats, Throws, Date.of.Birth, ROO, Start.Year)
#     write.csv(bio, "N:/Apps/simScoresApp/data/manual-info/bio_bat.csv", row.names = FALSE);
#   }
#
#   # If pitcher
#   else {
#     print(names(bio))
#     print(names(clean1))
#     bio <- read.csv("N:/Apps/simScoresApp/data/manual-info/bio_pit.csv", header = T);
#     positions <- read.csv("N:/Apps/simScoresApp/data/manual-info/positions.csv", header = T);
#     for (i in 1:nrow(clean1)) {
#
#       #
#       # First add bio info if necessary
#       #
#
#       if (clean1[i, "j_id"] %in% bio$MLBID) {
#         # do nothing
#         print(paste0(clean1[i, "j_id"], " exists in bio_pit"));
#       }
#       else {
#         new.df <- data.frame(MLBID = clean1[i, 41], Name = clean1[i, "Name"], Height = 100, Weight = 100, Throws = "Both", Date.of.Birth = "1/1/1990", ROO = "Asia", Start.Year = 2006)
#         names(new.df) <- c("MLBID", "Name", "Height", "Weight", "Throws", "Date.of.Birth", "ROO", "Start.Year");
#         bio <- rbind(bio, new.df);
#         print(paste0(clean1[i, "j_id"], " added to bio_pit"));
#       }
#
#       #
#       # Add position info if necessary
#       #
#       #print(names(clean2))
#       #if (clean2[i, "j_id"] %in% positions$MLBID) {
#       #positions$MLBID <- positions$MLBID %>% as.character
#       #clean2$j_id <- clean2$j_id %>% as.character
#       #if (positions %>% filter(MLBID == clean2[i, "j_id"], Year == clean2[i, "Year"]) %>% nrow() > 0) {
#       # do nothing
#       #print(paste0(clean2[i, "j_id"], " in year ", clean2[i, "Year"], " exists in positions (pitcher) -"));
#       #}
#       #else {
#       #rbind(bio, data.frame(MLBID = clean2[i, 39], Name = clean2[i, 2], Height = 0, Weight = 0, Bats = "n/a", Throws = "n/a", Date.of.Birth = "n/a", ROO = "n/a", Start.Year = 1900));
#       #new.df <- data.frame(MLBID = clean2[i, 39], "Year" = clean2[i, "Year"], Position = "SP")
#       #names(new.df) <- c("MLBID", "Year", "Position");
#       #positions <- rbind(positions, new.df);
#       #print(paste0(clean2[i, "j_id"], " in year ", clean2[i, "Year"], " added to positions (pitcher) :"));
#       #}
#
#     }
#
#     bio <- dplyr::select(bio, MLBID, Name, Height, Weight, Throws, Date.of.Birth, ROO, Start.Year)
#     #positions <- dplyr::select(positions, MLBID, Year, Position)
#     write.csv(bio, "N:/Apps/simScoresApp/data/manual-info/bio_pit.csv", row.names = FALSE);
#     #write.csv(positions, "N:/Apps/simScoresApp/data/manual-info/positions.csv", row.names = FALSE);
#   }

  if(type == "bat") {
    clean2 <- clean1 %>%
      mutate(X1B = H - X2B - X3B - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, PA, AB, X1B, X2B, X3B, HR, BB, HBP, K)
  } else  {
    clean2 <- clean1 %>%
      mutate(TBF = BF,
             SDT = H - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, IP, TBF, SDT, HR, BB, HBP, K)
  }
  clean2 %>%
    left_join(j_ids) %>%
    mutate(MLBID = ifelse(is.na(MLBID),
                          j_id,
                          as.character(MLBID))) %>%
    dplyr::select(MLBID, Year:K)
}
