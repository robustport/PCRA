# -*- coding: utf-8 -*-

import pandas as pd, numpy as np, datetime
from scipy import stats


inFile = 'C:/Users/tkpme/OneDrive/Documents/GSoC projects/Textbook/Chapter 18/Volatility and IR of Treasury Nominal yield curve.xlsx'


def main():
    IRdaily = pd.read_excel(inFile, sheet_name='Interest Rates for Pandas')
    IRdaily.replace('NaN', np.nan)
    IRdaily['Date'] = pd.to_datetime(IRdaily['Date'])           #Turn Date into a datetime object with just the date and make it the index
    IRdaily.set_index('Date', inplace=True)

    IRdaily['TwosFives']    = IRdaily['5 yr']  - IRdaily['2 yr'] 
    IRdaily['FivesTwentys'] = IRdaily['20 yr'] - IRdaily['5 yr'] 
    IRdaily['TwosTens']     = IRdaily['10 yr'] - IRdaily['2 yr'] 

    cols = list(IRdaily)                                        #Convert all numbers to floating point
    IRdaily[cols] = IRdaily[cols].astype(float)
    IRweekly  = IRdaily.resample('W').last()                    #Get end of week and end of month snapshots of rates
    IRmonthly = IRdaily.resample('M').last()

    deltaIRdaily   = IRdaily   - IRdaily.shift(1)               #obtain daily , weekly and monthly changes in interest rates
    deltaIRweekly  = IRweekly  - IRweekly.shift(1)
    deltaIRmonthly = IRmonthly - IRmonthly.shift(1)


    filteredDeltaIRdaily = deltaIRdaily[deltaIRdaily['5 yr'].notnull() & deltaIRdaily['TwosTens'].notnull()]

    slope1 = stats.mstats.theilslopes(filteredDeltaIRdaily['5 yr'].values, filteredDeltaIRdaily['TwosTens'].values)[0]
    slope2 = stats.mstats.theilslopes( filteredDeltaIRdaily['TwosTens'].values, filteredDeltaIRdaily['5 yr'].values)[0]
    corr = np.sqrt(slope1 * slope2)
    print(corr)
    print(filteredDeltaIRdaily.corr()['TwosTens'])
  
main()