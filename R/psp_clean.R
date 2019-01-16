#' Clean Permanent Sampling Plot Data from Alberta. Will not remove treeNumber = 9999
#'
#'@param treeDataRaw individual tree data from Alberta PSP
#'@param plotHeaderDataRaw plot level data inc. location
#'@export
#'
#'@rdname dataPurification_ABMaturePSP
dataPurification_ABMaturePSP <- function(treeDataRaw, plotHeaderDataRaw) {

  setnames(
    treeDataRaw,
    c("Groupnumber", "Plotnumber", "Treenumber"),
    c("GroupNumber", "PlotNumber", "TreeNumber")
  )
  treeDataRaw[, GroupNumber := as.character(GroupNumber)]
  setnames(plotHeaderDataRaw,
           c("PLOT..", "TYPE", "PLOTS", "DEC...LONG","DEC...LAT","Plot.Size.m2","Stand.origin", "Managed."),
           c("GroupNumber", "Type" ,"NofSubplot", "Longitude", "Latitude", "PlotSize", "StandOrigin", "Managed")
  )
  headerData <- plotHeaderDataRaw[, .(GroupNumber, Type, NofSubplot, Longitude, Latitude, Easting,
                                      Northing, Meridian, Elevation, PlotSize, StandOrigin, Managed)]
  headerData[, GroupNumber := as.character(GroupNumber)]
  headerData[Meridian == "UTM 117(NAD83)", Zone := 11]
  headerData[Meridian == "UTM 111(NAD83)", Zone := 12]

  # generate head data for each plot, unmanaged, SA available, location available
  headerData_SA <- treeDataRaw[TreeNumber == 0 & (!is.na(DBHage) | !is.na(Stumpage)), ]
  SADiff <- as.integer(mean(headerData_SA[!is.na(DBHage) &
                                            !is.na(Stumpage)]$Stumpage - headerData_SA[!is.na(DBHage) &
                                                                                         !is.na(Stumpage)]$DBHage))
  headerData_SA <- headerData_SA[!is.na(DBHage) & is.na(Stumpage),
                                 Stumpage := DBHage + SADiff][, .(GroupNumber, PlotNumber, MeasureYear, Stumpage)]
  headerData_SA[, firstMeasureYear := min(MeasureYear),
                by = c("GroupNumber")][, treeAge := Stumpage - MeasureYear + firstMeasureYear]
  headerData_SA <-  headerData_SA[, .(baseYear = mean(firstMeasureYear),
                                      baseSA = round(mean(treeAge))), by = c("GroupNumber")]


  # select PSPs

  # select the plots with locations
  headerData <- headerData[(Longitude != 0 & Latitude != 0) |(Northing != 0 & Easting != 0 & !is.na(Zone)), ]
  # select plots unmanaged
  headerData <- headerData[Managed == "No", ]
  # joining the SA information
  headerData <- setkey(headerData, GroupNumber)[setkey(headerData_SA, GroupNumber),
                                                nomatch  = 0][, ':='(Type = NULL, NofSubplot = NULL,
                                                                     StandOrigin = NULL, Managed = NULL)]
  headerData <- unique(headerData, by = "GroupNumber")

  treeData <- treeDataRaw[TreeNumber != 0, ][
    , .(GroupNumber, PlotNumber, MeasureYear, TreeNumber, Species, DBH,
        Height, Conditioncode1, Conditioncode2, Conditioncode3, Treeplotsize)]
  # remove DBH is not available
  treeData <- treeData[!is.na(DBH) & DBH != 0, ]

  treeData <- treeData[GroupNumber %in% headerData$GroupNumber, ]
  tempPlotID <- unique(treeData[, .(GroupNumber, PlotNumber, MeasureYear)],
                       by = c("GroupNumber", "PlotNumber", "MeasureYear"))
  tempPlotID[, MeasureID := as.numeric(row.names(tempPlotID))]
  tempPlotID <- tempPlotID[, .(MeasureID, GroupNumber, PlotNumber, MeasureYear)]
  setkey(tempPlotID, GroupNumber, PlotNumber, MeasureYear)
  treeData <- tempPlotID[setkey(treeData, GroupNumber, PlotNumber, MeasureYear), nomatch = 0]

  # treeData condition check
  treeData <-  treeData[Conditioncode1 != 25 & Conditioncode1 != 61 &
                          Conditioncode1 != 79 & Conditioncode1 != 80, ]

  treeData <- treeData[is.na(Conditioncode2) |
                         (Conditioncode2 != 25 & Conditioncode2 != 61 & Conditioncode2 != 79 & Conditioncode2 != 80), ]
  treeData <- treeData[is.na(Conditioncode3) |
                         (Conditioncode3 != 25 & Conditioncode3 != 61 & Conditioncode2 != 79 & Conditioncode2 != 80), ]
  treeData[, ':='(Conditioncode1 = NULL, Conditioncode2 = NULL, Conditioncode3 = NULL)]

  # check the plot size
  treeData[, plotsizetime := as.numeric(length(unique(Treeplotsize))), by = c("MeasureID")]

  if (nrow(treeData[plotsizetime == 2, ]) > 0) {
    plotids <- unique(treeData[plotsizetime == 2, ]$MeasureID)
    for (plotid in plotids) {
      groupnumber <- unique(treeData[MeasureID == plotid, ]$GroupNumber)
      plotsize <- as.numeric(headerData[GroupNumber == groupnumber, ]$PlotSize) # obtain plot size from headData
      treeData[MeasureID == plotid, Treeplotsize := plotsize]
    }
  }

  setnames(treeData, "Treeplotsize", "PlotSize")
  measureiddata <- setkey(unique(treeData[, .(MeasureID, GroupNumber, PlotSize, MeasureYear)],
                                 by = "MeasureID"), GroupNumber)
  headerData[, PlotSize := NULL]
  headerData <- measureiddata[setkey(headerData, GroupNumber), nomatch  = 0][, Longitude := -(Longitude)]
  treeData[, ':='(DBH = DBH / 10, PlotSize = NULL, plotsizetime = NULL)]

  headerData[, MeasureID := paste("ABPSPMature_", MeasureID, sep = "")]
  setnames(headerData, "GroupNumber", "OrigPlotID1")


  headerData <- headerData[, .(MeasureID, OrigPlotID1, MeasureYear, Longitude, Latitude, Zone, Easting,
                               Northing = as.numeric(Northing), PlotSize = PlotSize / 10000, baseYear, baseSA)]
  treeData[, MeasureID := paste("ABPSPMature_", MeasureID, sep = "")]
  setnames(treeData, c("GroupNumber", "PlotNumber"), c("OrigPlotID1", "OrigPlotID2"))

  return(list(plotHeaderData = headerData, treeData = treeData))
}


#' Clean Permanent Sampling Plot Data from BC
#'
#'@param treeDataRaw individual tree data from BC PSP
#'@param plotHeaderDataRaw plot level data inc. location
#'@export
#'
#'@rdname dataPurification_BCPSP
dataPurification_BCPSP <- function(treeDataRaw, plotHeaderDataRaw) {
  headerData <- plotHeaderDataRaw[tot_stand_age != -99,][
    ,':='(utmtimes = length(unique(utm_zone)),
          eastingtimes = length(unique(utm_easting)),
          northingtimes = length(unique(utm_northing)),
          SAtimes = length(unique(tot_stand_age)),
          plotsizetimes = length(unique(area_pm)),
          standorigtimes = length(unique(stnd_org)),
          treatmenttimes = length(unique(treatment))),
    by = SAMP_ID]
  # unique(headDataRaw$utmtimes)#1
  # unique(headDataRaw$eastingtimes)#1
  # unique(headDataRaw$northingtimes)# 1
  # unique(headDataRaw$SAtimes) # no NA
  # unique(headDataRaw$plotsizetimes)#1
  # unique(headDataRaw$standorigtimes) # 1


  # select the natural originated and untreated
  headerData <- headerData[stnd_org == "N" | stnd_org == "F",]

  headerData[,treatmenttimes := length(unique(treatment)), by = c("SAMP_ID", "meas_yr")]
  # unique(headDataRaw$treatmenttimes) # 1 2
  headerData <- headerData[treatmenttimes == 1 & treatment == "UNTREATED", .(SAMP_ID, utm_zone, utm_easting,
                                                                             utm_northing, area_pm, tot_stand_age,
                                                                             meas_yr)]


  headerData[, baseYear := min(meas_yr),
             by = SAMP_ID]
  headerData[, baseSA := as.integer(tot_stand_age-(meas_yr-baseYear))]
  # get the plots with locations
  headerData <- headerData[!is.na(utm_zone) & !is.na(utm_easting) & !is.na(utm_northing),]
  # get plots with plot size
  headerData <- headerData[!is.na(area_pm),][,':='(tot_stand_age = NULL, meas_yr = NULL)]
  names(headerData)[1:5] <- c("OrigPlotID1", "Zone", "Easting",
                              "Northing", "PlotSize")
  headerData <- unique(headerData, by = c("OrigPlotID1"))



  # for tree data
  if (is.null(treeDataRaw$OrigPlotID1)) {
    setnames(treeDataRaw, c("SAMP_ID", "plot_no"),
             c("OrigPlotID1", "OrigPlotID2"))
  }
  treeData <- treeDataRaw[OrigPlotID1 %in% unique(headerData$OrigPlotID1),]
  # range(treeData$meas_yr) # 1926 2014
  # unique(treeData$Plotnumber) # 01 02 03
  treeData <- treeData[sub_plot_tree == "N",][, OrigPlotID2 := as.numeric(OrigPlotID2)]
  treeData <- treeData[tree_cls == 1 | tree_cls == 2,.(OrigPlotID1, OrigPlotID2, meas_yr, tree_no, species,
                                                       dbh, height)]
  # unique(treeData$ld)
  names(treeData)[3:7] <- c("MeasureYear", "TreeNumber", "Species", "DBH", "Height")

  measreidtable <- unique(treeData[,.(OrigPlotID1, OrigPlotID2, MeasureYear)], by = c("OrigPlotID1", "OrigPlotID2", "MeasureYear"))
  measreidtable[,MeasureID:= paste("BCPSP_",row.names(measreidtable), sep = "")]
  measreidtable <- measreidtable[,.(MeasureID, OrigPlotID1, OrigPlotID2, MeasureYear)]
  headerData <- setkey(measreidtable, OrigPlotID1)[setkey(headerData, OrigPlotID1), nomatch = 0]
  set(headerData, NULL,"OrigPlotID2", NULL)
  headerData <- headerData[,.(MeasureID, OrigPlotID1, MeasureYear, Longitude = NA,
                              Latitude = NA, Zone, Easting, Northing, PlotSize, baseYear,
                              baseSA)]
  treeData <- setkey(measreidtable, OrigPlotID1, OrigPlotID2, MeasureYear)[setkey(treeData, OrigPlotID1, OrigPlotID2, MeasureYear),
                                                                           nomatch = 0]
  return(list(plotHeaderData = headerData, treeData = treeData))
}

#' Clean Permanent Sampling Plot Data from SK
#'
#'@param SADataRaw tree level data from SK PSP with age information
#'@param plotHeaderRaw plot-measure level data from SK PSP
#'@param measureHeaderRaw plot level data containing location from SK PSP
#'@param treeDataRaw tree level data from SK PSP
#'@export
#'
#'@rdname dataPurification_SKPSP
dataPurification_SKPSP <- function(SADataRaw,
                                   plotHeaderRaw,
                                   measureHeaderRaw,
                                   treeDataRaw) {
  # range(SADataRaw$COUNTED_AGE) # NA NA
  # range(SADataRaw$TOTAL_AGE) # NA NA
  # unique(SADataRaw$TREE_STATUS)
  header_SA <- SADataRaw[!is.na(TOTAL_AGE) & TREE_STATUS == 1, ]
  header_SA[, baseYear := min(YEAR), by = PLOT_ID]
  header_SA[, treeAge := TOTAL_AGE - (YEAR - baseYear)]
  header_SA_Dom <-
    header_SA[CROWN_CLASS == 1, ] # the stand age first determined by dominant trees
  header_SA_Dom[, NofTrees := length(CROWN_CLASS), by  = PLOT_ID]
  # unique(SADataRawDomSA$NofTrees) # 1 2 3 4 5
  # stand age must determined by using at least 2 trees
  header_SA_Dom <- header_SA_Dom[NofTrees != 1, ]
  # SADataRawDomSA[, treeAgeDif:=max(treeAge)-min(treeAge), by = PLOT_ID]
  # range(SADataRawDomSA$treeAgeDif) # 0 44
  # mean(SADataRawDomSA$treeAgeDif) # 7.03
  header_SA_Dom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_Dom <-
    unique(header_SA_Dom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  # for the other plots determine SA using codominant trees
  header_SA_CoDom <- header_SA[CROWN_CLASS == 2, ]

  header_SA_CoDom <-
    header_SA_CoDom[!(PLOT_ID %in% unique(header_SA_Dom$PLOT_ID)), ]
  header_SA_CoDom[, NofTrees := length(CROWN_CLASS), by  = PLOT_ID]
  # unique(SADataRawCodomSA$NofTrees)
  header_SA_CoDom <- header_SA_CoDom[NofTrees != 1, ]
  header_SA_CoDom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_CoDom <-
    unique(header_SA_CoDom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  headData_SA <- rbind(header_SA_Dom, header_SA_CoDom)

  headData_loca <-
    plotHeaderRaw[PLOT_ID %in% unique(headData_SA$PLOT_ID), ][, .(PLOT_ID, Z13nad83_e, Z13nad83_n, Zone = 13)]
  names(headData_loca)[2:3] <- c("Easting", "Northing")
  headData_SALoca <-
    setkey(headData_SA, PLOT_ID)[setkey(headData_loca, PLOT_ID),
                                 nomatch = 0]
  headData_PS <-
    measureHeaderRaw[PLOT_ID %in% unique(headData_SALoca$PLOT_ID), ][, .(PLOT_ID, PLOT_SIZE)][!is.na(PLOT_SIZE), ]
  headData_PS <- unique(headData_PS, by = "PLOT_ID")
  setnames(headData_PS, "PLOT_SIZE", "PlotSize")
  headData <-
    headData_SALoca[setkey(headData_PS, PLOT_ID), nomatch = 0]


  # for tree data
  treeDataRaw <- treeDataRaw[PLOT_ID %in% headData$PLOT_ID, ][, .(
    PLOT_ID,
    TREE_NO,
    YEAR,
    SPECIES,
    DBH,
    HEIGHT,
    TREE_STATUS,
    CONDITION_CODE1,
    CONDITION_CODE2,
    CONDITION_CODE3,
    MORTALITY
  )]

  # check the living trees
  # 1. by tree status codes
  #     1 1 Live
  #     2 2 Declining
  #     3 3 Dead or dying
  #     4 4 Loose bark snag
  #     5 5 Clean snag
  #     6 6 Snag with broken top
  #     7 7 Decomposed snag.
  #     8 8 Down snag
  #     9 9 Stump
  treeData <- treeDataRaw[is.na(TREE_STATUS) | # conservtively
                            TREE_STATUS == 0 |
                            TREE_STATUS == 1 |
                            TREE_STATUS == 2, ]
  # 2. by mortality codes
  #     Null 0
  #     Natural or Undetermined 1
  #     Disease 2
  #     Insect 3
  #     Human 4
  #     Wind 5
  #     Snow 6
  #     Other Trees 7
  #     Hail or Ice Storm 8
  treeData <- treeData[MORTALITY == 0 |
                         is.na(MORTALITY), ]
  # check the trees with both status and mortality are NA
  # unique(treeDataRaw[is.na(TREE_STATUS) & is.na(MORTALITY), ]$CONDITIONCODE1)
  # NULL
  treeData <- treeData[!is.na(DBH) & DBH != 0, ]
  treeData <-
    treeData[, .(PLOT_ID, OrigPlotID2 = NA, YEAR, TREE_NO, SPECIES,  DBH, HEIGHT)]
  names(treeData) <- c(
    "OrigPlotID1",
    "OrigPlotID2",
    "MeasureYear",
    "TreeNumber",
    "Species",
    "DBH",
    "Height"
  )
  setnames(headData, "PLOT_ID", "OrigPlotID1")
  measureidtable <-
    unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  measureidtable[, MeasureID := paste("SKPSP_", row.names(measureidtable), sep = "")]
  measureidtable <-
    measureidtable[, .(MeasureID, OrigPlotID1, MeasureYear)]
  treeData <- setkey(measureidtable, OrigPlotID1, MeasureYear)[setkey(treeData, OrigPlotID1, MeasureYear),nomatch = 0]
  treeData <- treeData[, .(MeasureID,
                           OrigPlotID1,
                           OrigPlotID2,
                           MeasureYear,
                           TreeNumber,
                           Species,
                           DBH,
                           Height)]
  headData <-
    setkey(measureidtable, OrigPlotID1)[setkey(headData, OrigPlotID1),
                                        nomatch = 0]
  headData <- headData[, .(
    MeasureID,
    OrigPlotID1,
    MeasureYear,
    Longitude = NA,
    Latitude = NA,
    Zone,
    Easting,
    Northing,
    PlotSize,
    baseYear,
    baseSA
  )]
  return(list(plotHeaderData = headData, treeData = treeData))
}

#' Clean Temporary Sampling Plot Data from SK Mistik forest management
#'
#'@param compiledPlotData plot-level data from SK TSP Mistic
#'@param compiledTreeData tree-level data from SK TSP Mistic
#'
#'@rdname dataPurification_SKTSP_Mistic
dataPurification_SKTSP_Mistic <- function(compiledPlotData, compiledTreeData) {
  options(scipen = 999) # avoid scientific notation

  headData <- compiledPlotData[, .(ID_FOR, CRZ_ZONE, CRZNORTH, CRZ_EAST, PLOTNUM, YEAR, PSIZE, P_AGECLS)]

  headData <- unique(headData, by = c("ID_FOR", "PLOTNUM"))
  headData[, PlotSize := sum(PSIZE), by = ID_FOR]
  headData <- unique(headData, by = "ID_FOR")[, ':='(PLOTNUM = NULL, PSIZE = NULL)]
  headData[, MeasureID := paste("SKTSP_Mistik_", row.names(headData), sep = "")]
  setnames(headData,c("CRZNORTH", "CRZ_EAST", "CRZ_ZONE", "YEAR", "P_AGECLS"),
           c("Northing", "Easting", "Zone", "MeasureYear", "SA"))

  treeData <- compiledTreeData[, .(ID_FOR, TREENO, SPECIES, DBH, HEIGHT, CONDCOD1, CONDCOD2, CONDCOD3)]
  treeData <- treeData[ID_FOR %in% unique(headData$ID_FOR),]
  # remove dead trees
  treeData <- treeData[CONDCOD1 != "DE", ]
  treeData <- treeData[CONDCOD2 != "DE", ]
  treeData <- treeData[CONDCOD3 != "DE", ]
  set(treeData, NULL, c("CONDCOD1", "CONDCOD2", "CONDCOD3"), NULL)
  treeData <- setkey(headData[, .(MeasureID, ID_FOR, MeasureYear)],
                     ID_FOR)[setkey(treeData, ID_FOR), nomatch = 0]


  treeData <- treeData[, .(MeasureID, OrigPlotID1 = ID_FOR, OrigPlotID2 = NA, MeasureYear,
                           TreeNumber = TREENO, Species = SPECIES, DBH, Height = HEIGHT)]

  headData <- headData[, .(MeasureID, OrigPlotID1 = ID_FOR, MeasureYear, Longitude = NA,
                           Latitude = NA, Zone, Easting = Easting * 10000, Northing = Northing * 10000,
                           PlotSize, baseYear = MeasureYear, baseSA = SA)]

  return(list(plotHeaderData = headData, treeData = treeData))
}

#' Clean Temporary Sampling Plot Data from SK Mistik forest management
#'
#'@param lgptreeRaw tree-level data from NFI PSP
#'@param lgpHeaderRaw plot-level data from NFI PSP
#'@param approxLocation locational data from NFI PSP
#'@rdname dataPurification_NFIPSP
#'
dataPurification_NFIPSP <- function(lgptreeRaw, lgpHeaderRaw, approxLocation) {
  # start from tree data to obtain plot infor
  lgptreeRaw[, year := as.numeric(substr(lgptreeRaw$meas_date, 1, 4))]
  lgpHeaderRaw[, year := as.numeric(substr(lgpHeaderRaw$meas_date, 1, 4))]
  lgpHeader <-
    lgpHeaderRaw[nfi_plot %in% unique(lgptreeRaw$nfi_plot), ][, .(nfi_plot, year, meas_plot_size, site_age)]
  approxLocation <-
    approxLocation[, .(nfi_plot, longitude, latitude, elevation)] %>%
    unique(, by = "nfi_plot")
  lgpHeader <-
    setkey(lgpHeader, nfi_plot)[setkey(approxLocation, nfi_plot), nomatch = 0]
  # remove the plots without SA and location infor
  lgpHeader <- lgpHeader[!is.na(site_age), ][!is.na(longitude), ][!is.na(latitude), ]
  treeData <- lgptreeRaw[, .(nfi_plot, year, tree_num, lgtree_genus, lgtree_species,
                             lgtree_status, dbh, height)][nfi_plot %in% unique(lgpHeader$nfi_plot), ]
  treeData <- treeData[lgtree_status != "DS" & lgtree_status != "M", ][, lgtree_status := NULL]
  setnames(treeData, c("nfi_plot", "year", "tree_num","lgtree_genus", "lgtree_species", "dbh", "height"),
           c("OrigPlotID1", "MeasureYear", "TreeNumber", "Genus", "Species", "DBH", "Height"))

  names(lgpHeader) <- c("OrigPlotID1", "baseYear", "PlotSize", "baseSA", "Longitude", "Latitude", "Elevation")
  lgpHeader <- unique(lgpHeader, by = "OrigPlotID1")
  newheader <- unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  newheader[, MeasureID := paste("NFIPSP_", row.names(newheader), sep = "")]
  lgpHeader <- setkey(lgpHeader, OrigPlotID1)[setkey(newheader, OrigPlotID1), nomatch = 0]
  treeData <- setkey(treeData, OrigPlotID1)[setkey(newheader[, .(OrigPlotID1, MeasureID)], OrigPlotID1), nomatch = 0]
  treeData <- treeData[, .(MeasureID, OrigPlotID1, OrigPlotID2 = NA, MeasureYear,
                           TreeNumber, Genus, Species, DBH, Height)]
  lgpHeader <- lgpHeader[, .(MeasureID, OrigPlotID1, MeasureYear, Longitude, Latitude, Zone = NA,
                             Easting = NA, Northing = NA, PlotSize, baseYear, baseSA)]
  return(list(plotHeaderData = lgpHeader, treeData = treeData))
}
