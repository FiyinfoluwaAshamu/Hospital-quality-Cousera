# Hospital-quality-Cousera

# Finding the best hospital in a state

Write a function called best that take two arguments:  the 2-character abbreviated name of a state and anoutcome name.  The function reads theoutcome-of-care-measures.csvfile and returns a character vectorwith  the  name  of  the  hospital  that  has  the  best  (i.e.   lowest)  30-day  mortality  for  the  specified  outcomein that state.  The hospital name is the name provided in theHospital.Namevariable.  The outcomes canbe one of “heart attack”, “heart failure”, or “pneumonia”.  Hospitals that do not have data on a particularoutcome should be excluded from the set of hospitals when deciding the rankings.Handling ties.  If there is a tie for the best hospital for a given outcome, then the hospital names shouldbe sorted in alphabetical order and the first hospital in that set should be chosen (i.e.  if hospitals “b”, “c”,and “f” are tied for best, then hospital “b” should be returned).The function should use the following template.best <- function(state, outcome) {## Read outcome data## Check that state and outcome are valid## Return hospital name in that state with lowest 30-day death## rate}The function should check the validity of its arguments.  If an invalidstatevalue is passed tobest,  thefunction should throw an error via thestopfunction with the exact message “invalid state”.  If an invalid outcome value is passed to best, the function should throw an error via thestopfunction with the exactmessage “invalid outcome”

# Ranking hospitals by outcome in a state

Write a function calledrankhospitalthat takes three arguments:  the 2-character abbreviated name of astate (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).The function reads theoutcome-of-care-measures.csvfile and returns a character vector with the nameof the hospital that has the ranking specified by thenumargument.  For example, the callrankhospital("MD", "heart failure", 5)would return a character vector containing the name of the hospital with the 5th lowest 30-day death ratefor heart failure.  The num argument can take values “best”, “worst”,  or an integer indicating the ranking(smaller numbers are better).  If the number given bynumis larger than the number of hospitals in thatstate, then the function should returnNA. Hospitals that do not have data on a particular outcome shouldbe excluded from the set of hospitals when deciding the rankings.Handling ties.  It may occur that multiple hospitals have the same 30-day mortality rate for a given causeof death.  In those cases ties should be broken by using the hospital name.  

# Ranking hospitals in all states

Write a function calledrankallthat takes two arguments: an outcome name (outcome) and a hospital rank-ing (num).  The function reads theoutcome-of-care-measures.csvfile and returns a 2-column data framecontaining the hospital in each state that has the ranking specified innum.  For example the function callrankall("heart attack", "best")would return a data frame containing the names of the hospitals thatare the best in their respective states for 30-day heart attack death rates.  The function should return a valuefor every state (some may beNA). The first column in the data frame is namedhospital, which containsthe hospital name, and the second column is namedstate, which contains the 2-character abbreviation forthe state name.  Hospitals that do not have data on a particular outcome should be excluded from the set ofhospitals when deciding the rankings.Handling ties.  Therankallfunction should handle ties in the 30-day mortality rates in the same waythat therankhospitalfunction handles ties.The function should use the following template.rankall <- function(outcome, num = "best") {## Read outcome data## Check that state and outcome are valid## For each state, find the hospital of the given rank## Return a data frame with the hospital names and the## (abbreviated) state name}NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT calltherankhospitalfunction from the previous section.
