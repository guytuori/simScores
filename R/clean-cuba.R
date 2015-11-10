scrape_cuban_bref <- function(ids = c("47638f90", "d31aed96", "cb73c393", "de145c19", "8bacd609", "7c28d664", "41e5bc89", "ee6b6c39", "a956bbc8"), years = c(2005:2013), type = c("bat", "pit"), brefBaseURL = "http://www.baseball-reference.com/minors/leader.cgi?type=bat&id=") {
    type <- match.arg(type, c("bat", "pit"))

    temp <- data.frame(ids, years)
    concat.urls <- paste0(temp$ids, temp$years)

    if(type == "pit") brefBaseURL <- stringr::str_replace_all(brefBaseURL, "bat", "pitch")
    urls <- paste0(brefBaseURL, concat.urls)
    downloads <- lapply(urls, function(link) {
        z <- XML::readHTMLTable(substr(link, 0, nchar(link)-4), which = 1, header = T, stringsAsFactors = F)
        z$Year <- substr_right(link, 4)
        z
    })
    downloads[sapply(downloads, is.data.frame)] %>%
        rbind_all %>%
        tbl_df() %>%
        mutate_each(funs(x = stringr::str_replace_all(., ",", "-")), Tm:Year)
}


clean_cuba <- function(dat, j_ids, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  clean1 <- dat %>%
    mutate(display_name = j_display_name(Name),
           lastf = getLastF(display_name)) %>%
    group_by(display_name, lastf) %>%
    mutate(id_num = Name %>% as.factor() %>% as.numeric(),
           j_id = paste0(lastf, id_num)) %>%
    ungroup();
  dups_only <- dat %>% mutate(display_name = j_display_name(Name),
                              lastf = getLastF(display_name)) %>%
    group_by(display_name, lastf) %>%
    mutate(id_num = display_name %>% factor %>% as.numeric(),
           j_id = paste0(lastf, id_num)) %>%
    ungroup() %>% group_by(lastf) %>% distinct(display_name) %>% filter(n() > 1) %>% arrange(lastf) %>% ungroup();

  if (type == "bat") {
    clean2 <- dups_only %>% assignCID %>% replaceID(clean1) %>%
      mutate(ML.Org = Tm,
             Level = "CNS",
             K = SO,
             H = H %>% as.character %>% as.integer,
             X2B = X2B %>% as.character %>% as.integer,
             X3B = X3B %>% as.character %>% as.integer,
             HR = HR %>% as.character %>% as.integer)
  }

  else {
    clean2 <- dups_only %>% assignCID %>% replaceID(clean1) %>%
      mutate(ML.Org = Tm,
             Level = "CNS",
             BF = BF %>% as.character %>% as.integer,
             H = H %>% as.character %>% as.integer,
             HR = HR %>% as.character %>% as.integer,
             K = SO %>% as.character %>% as.integer);

  }

  # Add players without MLBIDs to biographical files with dummy heights, weights so that they show up

#   if (type == "bat") {
#     bio <- read.csv("N:/Apps/simScoresApp/data/manual-info/bio_bat.csv", header = T);
#     positions <- read.csv("N:/Apps/simScoresApp/data/manual-info/positions.csv", header = T);
#     for (i in 1:nrow(clean2)) {
#       if (clean2[i, "j_id"] %in% bio$MLBID) {
#         # do nothing
#         #print(paste0(clean2[i, "j_id"], " exists in bio_bat"));
#       }
#       else {
#         #rbind(bio, data.frame(MLBID = clean2[i, 34], Name = clean2[i, 2], Height = 0, Weight = 0, Bats = "n/a", Throws = "n/a", Date.of.Birth = "n/a", ROO = "n/a", Start.Year = 1900));
#         new.df <- data.frame(MLBID = clean2[i, 34], Name = clean2[i, 2], Height = 100, Weight = 100, Bats = "Both", Throws = "Both", Date.of.Birth = "1/1/1990", ROO = "Caribbean", Start.Year = 2006)
#         names(new.df) <- c("MLBID", "Name", "Height", "Weight", "Bats", "Throws", "Date.of.Birth", "ROO", "Start.Year");
#         bio <- rbind(bio, new.df);
#         #print(paste0(clean2[i, "j_id"], " added to bio_bat"));
#       }
#
#     }
#     bio <- dplyr::select(bio, MLBID, Name, Height, Weight, Bats, Throws, Date.of.Birth, ROO, Start.Year)
#     write.csv(bio, "N:/Apps/simScoresApp/data/manual-info/bio_bat.csv", row.names = FALSE);
#   }
#
#   # If pitcher
#   else {
#     bio <- read.csv("N:/Apps/simScoresApp/data/manual-info/bio_pit.csv", header = T);
#     positions <- read.csv("N:/Apps/simScoresApp/data/manual-info/positions.csv", header = T);
#     for (i in 1:nrow(clean2)) {
#
#       #
#       # First add bio info if necessary
#       #
#
#       if (clean2[i, "j_id"] %in% bio$MLBID) {
#         # do nothing
#         #print(paste0(clean2[i, "j_id"], " exists in bio_pit"));
#       }
#       else {
#         new.df <- data.frame(MLBID = clean2[i, 39], Name = clean2[i, 2], Height = 0, Weight = 0, Throws = "n/a", Date.of.Birth = "n/a", ROO = "n/a", Start.Year = 1900)
#         names(new.df) <- c("MLBID", "Name", "Height", "Weight", "Throws", "Date.of.Birth", "ROO", "Start.Year");
#         bio <- rbind(bio, new.df);
#         #print(paste0(clean2[i, "j_id"], " added to bio_pit"));
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
#         # do nothing
#         #print(paste0(clean2[i, "j_id"], " in year ", clean2[i, "Year"], " exists in positions (pitcher) -"));
#       #}
#       #else {
#         #rbind(bio, data.frame(MLBID = clean2[i, 39], Name = clean2[i, 2], Height = 0, Weight = 0, Bats = "n/a", Throws = "n/a", Date.of.Birth = "n/a", ROO = "n/a", Start.Year = 1900));
#         #new.df <- data.frame(MLBID = clean2[i, 39], "Year" = clean2[i, "Year"], Position = "SP")
#         #names(new.df) <- c("MLBID", "Year", "Position");
#         #positions <- rbind(positions, new.df);
#         #print(paste0(clean2[i, "j_id"], " in year ", clean2[i, "Year"], " added to positions (pitcher) :"));
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
    clean3 <- clean2 %>%
      mutate(X1B = H - X2B - X3B - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, PA, AB, X1B, X2B, X3B, HR, BB, HBP, K)
  } else  {
    clean3 <- clean2 %>%
      mutate(TBF = BF,
             SDT = H - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, IP, TBF, SDT, HR, BB, HBP, K)
  }
  clean3 %>%
    left_join(j_ids) %>%
    mutate(MLBID = ifelse(is.na(MLBID),
                          j_id,
                          as.character(MLBID))) %>%
    dplyr::select(MLBID, Year:K)
}

run_cuba_scrape <- function(type = c("bat", "pit")) {
  if (type == "bat") {
    cuba <- scrape_cuban_bref(type = "bat");
  }
  else {
    cuba <- scrape_cuban_bref(type = "pit");
  }
  cuba1 <- cuba[, -1];
  cuba2 <- cleanNames(cuba1);
  cuba2 <- cuba2 %>% as.data.frame;
  if (type == "bat") {
    cuba2 %>% write.csv(paste0("N:/Apps/simScoresApp/data/0-downloads/bat_cuba.csv"));
    cuba2 <- read.csv(paste0("N:/Apps/simScoresApp/data/0-downloads/bat_cuba.csv"), header = T);
  }
  else {
    cuba2 %>% write.csv(paste0("N:/Apps/simScoresApp/data/0-downloads/pit_cuba.csv"));
    cuba2 <- read.csv(paste0("N:/Apps/simScoresApp/data/0-downloads/pit_cuba.csv"), header = T);
  }
  cuba2$Name <- cuba2$Name %>% as.character;


  c_ids=read.csv("N:/Apps/simScoresApp/data/manual-info/cuban_ids.csv", header=T)

  if (type == "bat") {
    cuba.cleaned <- cuba2 %>% clean_cuba(c_ids, type="bat");
  }
  else {
    cuba.cleaned <- cuba2 %>% clean_cuba(c_ids, type="pit");
  }

  return(cuba.cleaned);
}

# Note for below: a c_id is generated based on the players lastf name - that is, their last name and first initial. Should be followed by a number.
# Example: Yoenis Cespedes = CespedesY1

# Function purpose: Properly assigns custom and unique c_ids to Cuban players
# Solves an issue where players with the same last name and first initial were given the same c_id (they shared a lastf by coincidence and the number 1 was being appending to all of them, which doesn't allow for differentiation)
# Params: dat is a list of players (and relevant info) where their lastf form names are the same but their full names are different
# Returns: The same list but with new c_ids that now differentiate between the above players
assignCID <- function(dat) {
  # Turn the lastf form names into factors as a means to group players who share a lastf form
  dat1 <- dat %>% mutate(lastfID= factor(dat$lastf) %>% as.numeric)
  # We will need to iterate through each of the lastf names (which is less than the number of players), so find how many there are
  len <- dat1$lastfID %>% max()
  # For storing the results later
  new.dat <- data.frame()

  for (i in 1:len) {
    # Isolate the players with this specific lastf name
    mydat <- filter(dat1, lastfID == i)
    # For each of these players (there are probably only 2-3 of them), assign them each a different number from 1 to n. This number will ultimately be concatenated with the lastf name to get a unique c_id
    mydat$NUM <- seq(1:nrow(mydat))
    # Add it to the results dataframe
    new.dat <- rbind(new.dat, mydat)
  }
  return(new.dat)
}

# This takes the newly created c_ids for players with duplicated ones before and replaces them inside the original list
# updated is the returned dataframe from assignCID()
# dat is the original list of player data with incorrect c_ids
# Returns a list in the same form as dat but with the new c_ids merged in over the incorrect ones.
replaceID <- function(updated, dat) {
  for (i in 1:nrow(updated)) {
    for (j in 1:nrow(dat)) {
      if (dat[j,"display_name"] == updated[i, "display_name"]) {
        dat[j, "j_id"] <- paste0(updated[i, "lastf"], updated[i, "NUM"]);
      }
    }
  }
  return(dat);
}
