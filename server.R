library(shiny)
library(rCharts)
shinyServer(function(input, output, session) {
    
    allTeams = c('CSK', 'DD', 'KKR', 'KXIP', 'MI', 'RCB', 'RR', 'SRH')
    iplMatches <- read.csv("ipl2015.csv", header=TRUE, stringsAsFactors=FALSE)
    iplMatches$Date<-as.Date(iplMatches$Date, "%d-%m-%Y")

    getMatchNum = reactive(function(){
        matchNum <- 1
        if(input$radInType == "Match"){
            matchNum <- input$matchNum
        } else {
            matchNum <- 
                max(iplMatches$MN[iplMatches$Date <= as.Date(input$dateNum)])
        }
        return(matchNum)
    })
    
	output$thePlot = renderChart({
        ptsTable <- data.frame()
        for(i in c(1:getMatchNum())){
            ptsTable <- rbind(ptsTable, 
                cbind(team=allTeams, matchNum=i,
                    data.frame(t(sapply(allTeams, teamStat, i, 
                        iplMatches, 'Match')))
                )
            )
        }
        
        nnn <- data.frame(team = as.vector(unlist(ptsTable[1])), 
            matchNum = as.vector(unlist(ptsTable[2])), 
            pts = as.vector(unlist(ptsTable[7])))
                        
        p2 <- nPlot(pts~matchNum, data = nnn, group = "team", type = "lineChart")
        # p2$chart(color = c('#ff0000', '#00ff00', '#0000ff', '#000000', '#00ffff', '#ff00ff', '#ffff00', '#cccccc'))

        # p2$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
        p2$xAxis( axisLabel = 'Match')
        p2$yAxis( axisLabel = 'Points', width=56)
        p2$addParams(dom = 'thePlot')
        return(p2)

	})
    
    output$dtable = renderTable({
        if (input$radInType == "Match"){
            ptsTable <- data.frame(t(
                sapply(allTeams, teamStat, input$matchNum, iplMatches, 'Match')))
        }
        else if (input$radInType == "Date"){
            ptsTable <- data.frame(t(
                sapply(allTeams, teamStat, input$dateNum , iplMatches, 'Date')))
        }
        
        if (input$sortType == "TeamNames"){
            sortedTable <- ptsTable
        } else if (input$sortType == "TeamPoints"){
            sortedTable <- sortTable(ptsTable)
        }
        
        names(sortedTable)=c('Played', 'Won', 'NR', 'Lost', 'Points', 'NRR', 
            'For', 'forOv', 'Against', 'agtOv')
        
        sortedTable$Played <- paste(unlist(sortedTable$Played))
        sortedTable$Won <- paste(unlist(sortedTable$Won))
        sortedTable$NR <- paste(unlist(sortedTable$NR))
        sortedTable$Lost <- paste(unlist(sortedTable$Lost))
        sortedTable$Points <- paste(unlist(sortedTable$Points))
        sortedTable$For <- 
            paste(unlist(sortedTable$For), 
                  unlist(sortedTable$forOv), sep = " / ")
        sortedTable$Against <- 
            paste(unlist(sortedTable$Against), 
                  unlist(sortedTable$agtOv), sep = " / ")
                  
        # Remove the 8th and 10th columns
        sortedTable[,-c(8,10)]
    }, align=c('r','r','r','r','r','r','r','r','r'))
    
    teamStat = function(team, inQualifier, allMatches, matMode){
        if (matMode == 'Match'){
            inMatNum <- inQualifier
            yy <- allMatches[allMatches$MN <= inMatNum, ]
        } else if (matMode == 'Date'){
            # Subset only for the relevant dates
            inDate <- inQualifier
            yy <- allMatches[allMatches$Date <= inDate, ]
        }
        
        homeMatches <- yy[yy$Home == team, ]
        awayMatches <- yy[yy$Away == team, ]
        # Subset only to those where the team has played
        teamMatches <- rbind(homeMatches, awayMatches)
        
        numMatches = dim(teamMatches)[1]
        numWins = length(which(teamMatches$MWin == team))
        numNRs = length(which(teamMatches$MWin == "None"))
        numLosses = numMatches - numWins - numNRs
        numPts = numWins * 2 + numNRs * 1
        
        teamMatches$Ov1stIn =  unlist(lapply(teamMatches$Ov1stIn, 
            function(x) sapply(x, overToDec)))
        teamMatches$Ov2ndIn =  unlist(lapply(teamMatches$Ov2ndIn, 
            function(x) sapply(x, overToDec)))
     
        forSc1st = sum(teamMatches[which(teamMatches$BatFirst == team),]$Sc1stIn)
        forSc2nd = sum(teamMatches[which(teamMatches$BatFirst != team),]$Sc2ndIn)
        forSc = forSc1st + forSc2nd
        
        forOv1st = sum(teamMatches[which(teamMatches$BatFirst == team),]$Ov1stIn)
        forOv2nd = sum(teamMatches[which(teamMatches$BatFirst != team),]$Ov2ndIn)
        forOv = forOv1st + forOv2nd
        
        agtSc1st = sum(teamMatches[which(teamMatches$BatFirst != team),]$Sc1stIn)
        agtSc2nd = sum(teamMatches[which(teamMatches$BatFirst == team),]$Sc2ndIn)
        agtSc = agtSc1st + agtSc2nd
        
        agtOv1st = sum(teamMatches[which(teamMatches$BatFirst != team),]$Ov1stIn)
        agtOv2nd = sum(teamMatches[which(teamMatches$BatFirst == team),]$Ov2ndIn)
        agtOv = agtOv1st + agtOv2nd

        forRR = 0
        agtRR = 0
        if (forOv != 0){
            forRR = forSc / forOv
        }
        if (agtOv != 0){
            agtRR = agtSc / agtOv
        }
        
        nrr = forRR - agtRR
        
        outTbl = list()
        outTbl$numMatches = numMatches
        outTbl$numWins = numWins
        outTbl$numNRs = numNRs
        outTbl$numLosses = numLosses
        outTbl$numPts = numPts
        outTbl$nrr = round(nrr, 3)
        outTbl$forSc = forSc
        outTbl$forOv = decToOver(forOv)
        outTbl$agtSc = agtSc
        outTbl$agtOv = decToOver(agtOv)
        
        return (outTbl)
    }

    overToDec = function(inOver){
        overBalls = as.integer(unlist(strsplit(toString(inOver), ".", fixed=TRUE)))
        overFrac = overBalls[1]
        if (length(overBalls) == 2) {
            overFrac = overFrac + overBalls[2] * (1.0/6)
        }
        return (overFrac)
    }

    decToOver = function(inDec){
        totalBalls = round(as.double(inDec) * 6)
        overs = totalBalls %/% 6
        balls = totalBalls %% 6
        overString = paste0(toString(overs), '.', toString(balls))
        return (overString)
    }

    sortTable = function(ptsTable){
        numMatches <- unlist(ptsTable$numMatches)
        numLosses <- unlist(ptsTable$numLosses)
        numWins <- unlist(ptsTable$numWins)
        numPts <- unlist(ptsTable$numPts)
        nrr <- unlist(ptsTable$nrr)

        ordTable <- ptsTable[order(-numPts, numLosses, -numWins, numMatches, -nrr),]
        
        return(ordTable)
    }

    
})

