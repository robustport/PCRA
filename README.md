This PCRA R package is a companion to the book *Robust Portfolio Construction
and Risk Analysis* by R. Douglas Martin, Thomas K. Philips, Stoyan Stoyanov, 
Bernd Scherer and Kirk Li, scheduled for publication by Springer in 2025.

## PCRA Demo Scripts
The PCRA package will eventually have demo scripts for running code that reproduces most or some of the Tables and Figures in each PCRA book chapter.  Assuming that an R package is installed, but not necessarily loaded, you can view a list of the names of all the demo folder R scripts with the following R command

demo(package = “packageName”)

Use the above for the PCRA package, and you will see the following short list of demo scripts:

Demos in package ‘PCRA’:

Ch2_Code        Code that computes most Chapter
                2 figures and tables.
RobustStatisticsForPortfoliosJPM2023
                Reproduces most figures and
                tables in JPM 2023 paper Robust
                Statistics for Portfolios.
demoTest        A few code lines to test "Run
                demo".

which runs the code for the corresponding two Vignettes listed above.

You can view the code for any demo R script with the command “??” (but not with “help” command).  For example, in RStudio, use of the command

?? Ch2_Code

results in a Help tab display with the followng two links:

* *PCRS::Ch2_Code*
* (*Run demo*).

Use the first link, which results in a display of the entire R script in the Help tab.  Then copy/paste the script into your own new R file, and run it in chunks that are of interest to you. Many of the chunks will run quite quickly, e.g., a few seconds, but a few of them may take 2-4 minutes. Doing so for the Ch2_Code.R will help you learn the portfolio methods in the PCRA book Ch 2 Foundations, and apply them to different stocks data in the *stocksCRSP* data set.

NOTE: We do not recommend general use of (*Run demo*). This is because it runs the entire demo script, which will often take much too long. Furthermore, some scripts may fail to execute properly when run this way. That said, the (*Run demo*) link can be handy for running R demo scripts that execute quickly.

## The JPM 2023 Paper and Reproducibility Code
The paper "Robust Statistics for Portfolio Construction and Analysis" by 
R. Douglas Martin, Stoyan V. Stoyanov, Kirk Li and Mahmoud Shammaa, was published
in 2023 by *The Journal of Portfolio Management*, 49  ( 9) 105 - 139.  See the
 [Paper Abstract](https://www.pm-research.com/content/iijpormgmt/49/9/105),
where JPM Subscribers can also download the paper.

The R script file `RobustStatisticsForPortfoliosJPM2023.R` for reproducing most
of the Figures and Tables in the above paper is available in the PCRA package at 
`PCRA/demo`.  Download that R script by going to `PCRA/demo` and double clicking 
on the file, which then allows you to either click on the "Download raw file" 
link or the "Copy raw file" link.  If you have RStudio installed, the first choice 
may open the file in RStudio.  Otherwise you can paste the file into an empty R
script.

If you have any problems with the code, please send emails to both Doug Martin at 
`martinrd3d@gmail.com` and Kirk Li at `cocokecoli@gmail.com`. 
We will post responses in this README file.
