# -*- coding: utf-8 -*-
import pandas as pd, numpy as np, requests, datetime

outFiles = ['C:/Temp/Treasury Nominal yield curve.xlsx', 'C:/Temp/Treasury Real yield curve.xlsx']

TreasuryUrls = ['https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldAll',
                'https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=realyieldAll']

nRates = [11, 5]    #Number of real and nominal rates
keepRates = [(1,11), (1,5)]  #Range of real and nominal rates to keep - discard short rates and very long rates if needed. 1 denotes the first interest rate
startDates = [datetime.datetime(1990,1,1), datetime.datetime(2003,1,1)]     #start dates for analysis of nominal and real rates
endDates   = [datetime.datetime(2018,12,31), datetime.datetime(2018,12,31)] #end dates for analysis of nominal and real rates

def main():
    for i in range(2):
        rawData = requests.get(TreasuryUrls[i], allow_redirects=True).content   #Get the raw data from the treasury's website

        #get rid of everything before the start of the data table with interest rates
        #replace < and > with ! to make it easier to split, then split it into a list from which we can extract the history of rates
        cleanData = rawData.decode().split('<tr><th scope="col">')[-1].replace('<', '!').replace('>', '!').split('!') 

        processData(cleanData, nRates[i], keepRates[i], startDates[i], endDates[i], outFiles[i])       #process the data and write the results


def processData(cleanData, n, keep, dt1, dt2, outfn):
    dataList = []
    for item in cleanData:
        if len(item)>0:
            if item.upper() == 'DATE':                          #if item = 'Date' or 'DATE' this is the start of the header
                dataList.append('Date')
            elif item[0].isdigit() or ( item[0] == '-' and item[1].isdigit() ): #if the first character is a digit it is an interest rate. Allow for negative real reates
                dataList.append(item)
            elif 'N/A' in item:                                 #missing interest rates are indicatead by NaN
                dataList.append('NaN')

    rows = []                                                   #each row in rows is date + the set of interest rates for that date
    for pos, item in enumerate(dataList):    
        if item[0].isdigit() and '/' in item:                   #Dates start with a number and have the format mm/dd/yyyy 
            rows.append([dataList[pos]]+dataList[pos+keep[0]:pos+keep[1]+1])  #Append a list: first item is the date, following items are the relevant segment of the yield curve for that date

    IRdaily = pd.DataFrame(rows, columns = ['Date'] + dataList[keep[0]:keep[1]+1] ) #create dataframe with a time series of interest rates, column names are pulled from the start
    IRdaily.replace('NaN', np.nan)
    IRdaily['Date'] = pd.to_datetime(IRdaily['Date'])           #Turn Date into a datetime object with just the date and make it the index
    IRdaily.set_index('Date', inplace=True)
    IRdaily = IRdaily[IRdaily.index >= dt1]                     #Drop everthing before the start date and the end date
    IRdaily = IRdaily[IRdaily.index <= dt2]

    cols = list(IRdaily)                                        #Convert all numbers to floating point
    IRdaily[cols] = IRdaily[cols].astype(float)
    IRweekly  = IRdaily.resample('W').last()                    #Get end of week and end of month snapshots of rates
    IRmonthly = IRdaily.resample('M').last()

    deltaIRdaily   = IRdaily   - IRdaily.shift(1)               #obtain daily , weekly and monthly changes in interest rates
    deltaIRweekly  = IRweekly  - IRweekly.shift(1)
    deltaIRmonthly = IRmonthly - IRmonthly.shift(1)

    corrD = deltaIRdaily.corr()                                 #Compute daily, weekly and monthly correlations of interest rate changes
    corrW = deltaIRweekly.corr()
    corrM = deltaIRmonthly.corr()

    covD  = deltaIRdaily.cov()                                  #Compute daily, weekly and monthly covariances of interest rate changes
    covW  = deltaIRweekly.cov()
    covM  = deltaIRmonthly.cov()

    stDevD  = np.sqrt(252 * covD.values.diagonal())             #Compute annualized volatilities of daily, weekly and monthly interest rate changes
    stDevW  = np.sqrt(52  * covW.values.diagonal())
    stDevM  = np.sqrt(12  * covM.values.diagonal())

    stDevdf = pd.DataFrame({'Tenor':cols, 
                            'Annualized Daily Volatility': stDevD, 
                            'Annualized Weekly Volatility': stDevW, 
                            'Annualized Monthly Volatility': stDevM})
    stDevdf = stDevdf.set_index('Tenor')

    eigValCovD, eigVecCovD = np.linalg.eigh(covD)               #PCA for daily, weekly and monthly covariance matrices of interest rate changes
    eigValCovW, eigVecCovW = np.linalg.eigh(covW)               #eigh assumes a Hermetian matrix, order of eigenvalues is reversed!
    eigValCovM, eigVecCovM = np.linalg.eigh(covM)               #Largest eigenvalue is eigVal[-1], smallest eigenvalue is eigVal[0]

    pctRiskCovD = np.array(list( sum(eigValCovD[i:]) / sum(eigValCovD) for i in range(len(eigValCovD)) ) )      #% of risk explained by the largest i eigenvalues of the covariane matrix
    pctRiskCovW = np.array(list( sum(eigValCovW[i:]) / sum(eigValCovW) for i in range(len(eigValCovW)) ) )
    pctRiskCovM = np.array(list( sum(eigValCovM[i:]) / sum(eigValCovM) for i in range(len(eigValCovM)) ) )

    eigValandPctRiskCovD = pd.DataFrame({'Eigenvalues':eigValCovD, 'Cumulative Pct. of Risk': pctRiskCovD})     #create a dataframe with eigenvalues of covariance matrix
    eigValandPctRiskCovW = pd.DataFrame({'Eigenvalues':eigValCovW, 'Cumulative Pct. of Risk': pctRiskCovW})     #and % of risk explained by the largest i eigenvalues for export
    eigValandPctRiskCovM = pd.DataFrame({'Eigenvalues':eigValCovM, 'Cumulative Pct. of Risk': pctRiskCovM})

    outfn = outfn[:-5] + ' from ' + dt1.strftime('%Y-%m-%d') + ' to ' + dt2.strftime('%Y-%m-%d') + outfn[-5:] #Add the start and end date tothe file name
    writer = pd.ExcelWriter(outfn)                              #Create an Excel file in which to write our results

    IRdaily.to_excel(writer, 'Interest Rates')                  #Write the level of interest rates obtained from the Treasury to Excel

    pd.DataFrame(['Daily Changes in Interest Rates']).to_excel(writer,          'Daily Changes', startrow = 0,  startcol=0,  header=False, index=False)    #Write descriptive labels (daily) to Excel
    pd.DataFrame(['Correlation Matrix: Daily changes']).to_excel(writer,        'Daily Changes', startrow = 0,  startcol=15, header=False, index=False)
    pd.DataFrame(['Covariance Matrix: Daily changes']).to_excel(writer,         'Daily Changes', startrow = 15, startcol=15, header=False, index=False)
    pd.DataFrame(['Annualized Volatility: Daily changes']).to_excel(writer,     'Daily Changes', startrow = 30, startcol=15, header=False, index=False)
    pd.DataFrame(['Eigenvalues (Covariance): Daily changes']).to_excel(writer,  'Daily Changes', startrow = 30, startcol=20, header=False, index=False)
    pd.DataFrame(['Eigenvectors (Covariance): Daily changes']).to_excel(writer, 'Daily Changes', startrow = 45, startcol=15, header=False, index=False)

    deltaIRdaily.to_excel(writer,             'Daily Changes', startrow = 1)
    pd.DataFrame(corrD).to_excel(writer,      'Daily Changes', startrow = 1,  startcol=15, header=True, index=True)    #Write daily correlations, covariances, volatilities, eigenvalues and eigenvenctors to Excel
    pd.DataFrame(covD).to_excel(writer,       'Daily Changes', startrow = 16, startcol=15, header=True, index=True)
    stDevdf['Annualized Daily Volatility'].to_excel(writer,  'Daily Changes', startrow = 31, startcol=15, header=True, index=True)
    eigValandPctRiskCovD.to_excel(writer,     'Daily Changes', startrow = 31, startcol=20, header=True, index=True)
    pd.DataFrame(eigVecCovD).to_excel(writer, 'Daily Changes', startrow = 46, startcol=15, header=True, index=True)

    pd.DataFrame(['Weekly Changes in Interest Rates']).to_excel(writer,          'Weekly Changes', startrow = 0,  startcol=0,  header=False, index=False)     #Write descriptive labels (weekly) to Excel
    pd.DataFrame(['Correlation Matrix: Weekly changes']).to_excel(writer,        'Weekly Changes', startrow = 0,  startcol=15, header=False, index=False)
    pd.DataFrame(['Covariance Matrix: Weekly changes']).to_excel(writer,         'Weekly Changes', startrow = 15, startcol=15, header=False, index=False)
    pd.DataFrame(['Annualized Volatility: Weekly changes']).to_excel(writer,     'Weekly Changes', startrow = 30, startcol=15, header=False, index=False)
    pd.DataFrame(['Eigenvalues (Covariance): Weekly changes']).to_excel(writer,  'Weekly Changes', startrow = 30, startcol=20, header=False, index=False)
    pd.DataFrame(['Eigenvectors (Covariance): Weekly changes']).to_excel(writer, 'Weekly Changes', startrow = 45, startcol=15, header=False, index=False)

    deltaIRweekly.to_excel(writer,            'Weekly Changes', startrow = 1)
    pd.DataFrame(corrW).to_excel(writer,      'Weekly Changes', startrow = 1,  startcol=15, header=True, index=True)    #Write weekly correlations, covariances, volatilities, eigenvalues and eigenvenctors to Excel
    pd.DataFrame(covW).to_excel(writer,       'Weekly Changes', startrow = 16, startcol=15, header=True, index=True)
    stDevdf['Annualized Weekly Volatility'].to_excel(writer,  'Weekly Changes', startrow = 31, startcol=15, header=True, index=True)
    eigValandPctRiskCovW.to_excel(writer,     'Weekly Changes', startrow = 31, startcol=20, header=True, index=True)
    pd.DataFrame(eigVecCovW).to_excel(writer, 'Weekly Changes', startrow = 46, startcol=15, header=True, index=True)

    pd.DataFrame(['Monthly Changes in Interest Rates']).to_excel(writer,          'Monthly Changes', startrow = 0,  startcol=0,  header=False, index=False)    #Write descriptive labels (monthly) to Excel
    pd.DataFrame(['Correlation Matrix: Monthly changes']).to_excel(writer,        'Monthly Changes', startrow = 0,  startcol=15, header=False, index=False)
    pd.DataFrame(['Covariance Matrix: Monthly changes']).to_excel(writer,         'Monthly Changes', startrow = 15, startcol=15, header=False, index=False)
    pd.DataFrame(['Annualized Volatility: Monthly changes']).to_excel(writer,     'Monthly Changes', startrow = 30, startcol=15, header=False, index=False)
    pd.DataFrame(['Eigenvalues (Covariance): Monthly changes']).to_excel(writer,  'Monthly Changes', startrow = 30, startcol=20, header=False, index=False)
    pd.DataFrame(['Eigenvectors (Covariance): Monthly changes']).to_excel(writer, 'Monthly Changes', startrow = 45, startcol=15, header=False, index=False)

    deltaIRmonthly.to_excel(writer,           'Monthly Changes', startrow = 1)
    pd.DataFrame(corrM).to_excel(writer,      'Monthly Changes', startrow = 1,  startcol=15, header=True, index=True)    #Write monthly correlations, covariances, volatilities, eigenvalues and eigenvenctors to Excel
    pd.DataFrame(covM).to_excel(writer,       'Monthly Changes', startrow = 16, startcol=15, header=True, index=True)
    stDevdf['Annualized Monthly Volatility'].to_excel(writer,  'Monthly Changes', startrow = 31, startcol=15, header=True, index=True)
    eigValandPctRiskCovM.to_excel(writer,     'Monthly Changes', startrow = 31, startcol=20, header=True, index=True)
    pd.DataFrame(eigVecCovM).to_excel(writer, 'Monthly Changes', startrow = 46, startcol=15, header=True, index=True)

    writer.save()
#___________________________________________________________________________________________________________________________________________________

main()