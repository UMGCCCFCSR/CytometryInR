#how to put the homeworks in the github
# put it in the homework branch
#first sincronize your fork of CytometryInR on the github on the webpage 
#then pull the data in the local computer on positron
#then we have a branch for homowork
# add the homeworks in that folder of the big branch on CytometryInR
#folder with my user github like ATL0368
#subfolder data, and image if you want
#qmd document 



#Problem 1
#Modify one of the simpler functions (SecondFunction or similar),
#provide your own argument names, and modify the message(),
#paste0 or print() functions to print a text style output. 
#Generate a small vector of values, and iterate through your
#vector using one of the approaches we used.

#Problem 2
#Using the initial framework for the CellConcentration function, 
# retrieve several other keywords that are of interest, 
# and incorporate them into the returned data.frame row.

#Problem 3
#For CellConcentration, we retrieved both start and end time. 
# Look up information on lubridate package, convert these times 
# to a time-style format, and from acquired volume derrive uL/min
# at which each .fcs file was acquired. Did this vary at
# all across days?


#Problem 1
#original function
MySecondFunction <- function(BestFluorophore = "APC"){
    message("Unquestionably, the best fluorophore is ", BestFluorophore)}

SecondFHW <- function(BestMachine = "Aurora"){
  message ("My favourite machine is", BestMachine)}
SecondFHW()
My favourite machine is Aurora

#change parameters
Machines <- c("Aurora", "FACSymphony", "S8", "AriaIII")

#to check
walk(.x=Machines, .f=SecondFHW)
My favourite machine isAurora
My favourite machine isFACSymphony
My favourite machine isS8
My favourite machine isAriaIII

SecondFHW <- function(BestMachine = "Aurora"){
  print(paste0("Today's top cytometry machine is: ", BestMachine))}
> SecondFHW()
[1] "Today's top cytometry machine is: Aurora"

# paste0: joins pieces of text together into a single string, no separator
paste0("Machine: ", "Aurora")
[1] "Machine: Aurora"

#problem 2
#this is the function
CellConcentration <- function(x, subset, dilutionMultiplier){
    Date <- keyword(x)$`$DATE` # Day-Month-Year
    InstrumentSerial <- keyword(x)$`$CYTSN`
    TotalFileEvents <- keyword(x)$`$TOT`
    Specimen <- keyword(x)$GUID
    StartTime <- keyword(x)$`$BTIM`
    EndTime <- keyword(x)$`$ETIM`

    Volume <- keyword(x)$`$VOL`
    Volume <- as.numeric(Volume)

    EventsInTheGate <- gs_pop_get_data(x, subset)
    Cells <- nrow(EventsInTheGate)[[1]] #[[1]] since the output is in a list

    Concentration <- (Cells*1000)/Volume
    Concentration <- Concentration*dilutionMultiplier

    Data <- data.frame(
        Date=Date,
        InstrumentSerial=InstrumentSerial,
        Specimen=Specimen,
        StartTime=StartTime,
        EndTime=EndTime,
        Volume=Volume, 
        Cells=Cells,
        Concentration=Concentration)

    return(Data)}

#first convert cytoset in cytoframe to see and extract keywords
cytoframe_to_flowFrame()
FlowFrame <-cytoframe_to_flowFrame(MyCytoset) # the issue is that cytoframe_to_flowFrame() works on a single cytoframe, not an entire cytoset
#so first have just one sample
cf <- MyCytoset[[1]]
FlowFrame <- cytoframe_to_flowFrame(cf)
# now i have expr, parameters and description inside the S4 class object 
# so now I can extract more keywords
# is from description
cfdescription <- FlowFrame@description
head(cfdescription, 5)
$FCSversionm $`$ENDDATA` etc # also check in the side bar
# for example to add which cytometer $CYT
# operator $OP
# or to extract spillover matrix $spillover
SpilloverMatrix <- keyword(FlowFrame)$`$SPILLOVER`

CellConcentration <- function(x, subset, dilutionMultiplier){
    Date <- keyword(x)$`$DATE` # Day-Month-Year
    InstrumentSerial <- keyword(x)$`$CYTSN`
    TotalFileEvents <- keyword(x)$`$TOT`
    Specimen <- keyword(x)$GUID
    StartTime <- keyword(x)$`$BTIM`
    EndTime <- keyword(x)$`$ETIM`
    Cytometer <-keyword(x)$`$CYT`
    Operator <-keyword(x)$`$OP`

    Volume <- keyword(x)$`$VOL`
    Volume <- as.numeric(Volume)

    EventsInTheGate <- gs_pop_get_data(x, subset)
    Cells <- nrow(EventsInTheGate)[[1]]

    Concentration <- (Cells*1000)/Volume
    Concentration <- Concentration*dilutionMultiplier

    Data <- data.frame(
    Date=Date,
    InstrumentSerial=InstrumentSerial,
    Specimen=Specimen,
    StartTime=StartTime,
    EndTime=EndTime,
    Cytometer=Cytometer,
    Operator=Operator,
    Volume=Volume, 
    Cells=Cells,
    Concentration=Concentration)

  return(Data)}

#to chech one
purrr::map(.x=MyGatingSet[1], .f=CellConcentration, subset="CD19", dilutionMultiplier=100)
  Specimen   StartTime     EndTime Cytometer   Operator Volume Cells Concentration
1             30-Jul-2025            V0333 CellCounts4L_AB_04-INF124-7-00_01.fcs 13:48:10.76 13:48:37.88    Aurora David Rach  30.99   525       1694095


#Problem 3
library("lubridate")
# time formats
#concentration formats
?lubridate
#Users should choose the function whose name models the order in which the year ('y'), month ('m') and day ('d') 
#elements appear the string to be parsed: dmy(), myd(), ymd(), ydm(), dym(), mdy(), ymd_hms()). 
# so now the output look like this
#start time -end time: 13:48:10.76 and 13:48:37.88 
#so maybe not need ultraseconds just 13:48 to 13:48
#also date is 30-jul-2025, maybe change to 20250730

#for sample 1 subset cd19
StartTime <- hms("13:48:10.76")
EndTime <- hms("13:48:37.88")
RunTime <- EndTime - StartTime
[1] "27.12S"
# to convert in seconds
RunTimeSeconds <- as.numeric(RunTime)
[1] 27.12
Volume <- keyword(x)$`$VOL`

uL_per_min <- Volume / (RunTimeSeconds / 60)



#final function
CellConcentration <- function(x, subset, dilutionMultiplier){

    Date <- lubridate::dmy(keyword(x)$`$DATE`)

    InstrumentSerial <- keyword(x)$`$CYTSN`
    TotalFileEvents <- keyword(x)$`$TOT`
    Specimen <- keyword(x)$GUID

    StartTime <- lubridate::hms(keyword(x)$`$BTIM`)
    EndTime <- lubridate::hms(keyword(x)$`$ETIM`)

    Cytometer <- keyword(x)$`$CYT`
    Operator <- keyword(x)$`$OP`

    Volume <- keyword(x)$`$VOL`
    Volume <- as.numeric(Volume)

    EventsInTheGate <- gs_pop_get_data(x, subset)
    Cells <- nrow(EventsInTheGate)[[1]]

    Concentration <- (Cells * 1000) / Volume
    Concentration <- Concentration * dilutionMultiplier

    RunTime <- EndTime - StartTime
    RunTimeSeconds <- as.numeric(RunTime)

    uL_per_min <- Volume / (RunTimeSeconds / 60)

    Data <- data.frame(
        Date = Date,
        InstrumentSerial = InstrumentSerial,
        Specimen = Specimen,
        StartTime = StartTime,
        EndTime = EndTime,
        Cytometer = Cytometer,
        Operator = Operator,
        Volume = Volume,
        Cells = Cells,
        Concentration = Concentration,
        RunTimeSeconds = RunTimeSeconds,
        uL_per_min = uL_per_min
    )

    return(Data)}

#to check 
purrr::map(.x=MyGatingSet[1], .f=CellConcentration, subset="CD19", dilutionMultiplier=100)
        Date InstrumentSerial                              Specimen      StartTime        EndTime Cytometer   Operator Volume Cells Concentration RunTimeSeconds uL_per_min
1 2025-07-30            V0333 CellCounts4L_AB_04-INF124-7-00_01.fcs 13H 48M 10.76S 13H 48M 37.88S    Aurora David Rach  30.99   525       1694095          27.12   68.56195

#so it worked :)

#end HW
git add .
git commit -m "homeworks done"          
git push