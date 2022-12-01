## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(echo=TRUE, collapse=T, comment='#>')
library(rJava)
library(xlsx)

## ----theme--------------------------------------------------------------------

## fonts require a workbook
createFonts <- function(wb) {
  list(
    data = Font(wb, heightInPoints = 11, name='Arial')
    , title = Font(wb, heightInPoints = 16, name='Arial', isBold = TRUE)
    , subtitle = Font(wb, heightInPoints = 13, name='Arial', isBold = FALSE, isItalic = TRUE)
  )
}

## alignment
alignLeft <- Alignment(horizontal='ALIGN_LEFT', vertical='VERTICAL_CENTER', wrapText = TRUE)
alignCenter <- Alignment(horizontal='ALIGN_CENTER', vertical='VERTICAL_CENTER', wrapText=TRUE)

## data formats
dataFormatDate <- DataFormat('m/d/yyyy')
dataFormatNumberD <- DataFormat('0.0')

## fill
fillPrimary <- Fill('#cc0000','#cc0000','SOLID_FOREGROUND')
fillSecondary <- Fill('#ff6666','#ff6666','SOLID_FOREGROUND')


## ----prep_for_report, include=FALSE-------------------------------------------
## todo: fix the xlsx jars being available when generating vignette
#.jaddClassPath(rprojroot::is_r_package$find_file('inst/java/rexcel-0.5.1.jar'))

## ----build_report, results='hide'---------------------------------------------
## The dataset
numbercol <- 9
mydata <- as.data.frame(lapply(1:numbercol,function(x){runif(15, 0,200)}))
mydata <- setNames(mydata,paste0('col',1:numbercol))

## Build report
wb <- createWorkbook()
sh <- createSheet(wb, 'Report')
f <- createFonts(wb)

headerrow <- createRow(sh, 1:2)
headercell <- createCell(headerrow, 1:ncol(mydata))

## title
addMergedRegion(sh,1,1,1,ncol(mydata))
lapply(headercell[1,],function(cell) {
  setCellValue(cell, 'Title of Report')
  setCellStyle(cell, CellStyle(wb) + f$title + alignCenter)
})

## subtitle
addMergedRegion(sh, 2,2, 1,ncol(mydata))
lapply(headercell[2,],function(cell) {
  setCellValue(cell, 'A fantastic report about nothing')
  setCellStyle(cell, CellStyle(wb) + f$subtitle + alignCenter )
})

## cell styles for data
cslist <- lapply(1:ncol(mydata), function(x){CellStyle(wb) + f$data + alignCenter + dataFormatNumberD})
cslist[1:2] <- lapply(cslist[1:2], function(x){x + alignLeft}) ## left align first two columns

## add data
workrow <- 4

addDataFrame(mydata, sh
             , col.names=TRUE
             , row.names = FALSE
             , startRow = workrow
             , startColumn = 1 
             , colStyle = setNames(cslist,1:numbercol)
             , colnamesStyle = CellStyle(wb) + f$subtitle + alignCenter + fillPrimary
             )

workrow <- workrow + nrow(mydata) + 1 ## + 1 for header

## add total row... sorta 
## - (just the first row because I am lazy)
addDataFrame(mydata[1,], sh
             , col.names=FALSE
             , row.names=FALSE
             , startRow = workrow
             , startColumn = 1
             , colStyle = setNames(lapply(cslist,function(x){x + fillSecondary}),1:numbercol)
             )

saveWorkbook(wb, 'excel_report.xlsx')


