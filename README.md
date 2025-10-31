# WHO HPV PRIME
Papillomavirus Rapid Interface for Modelling and Economics (PRIME) is a static modelling tool designed to estimate the health and economic impact of HPV vaccination in girls before sexual debut, specifically for cervical cancer prevention. 

## Instructions for running the WHO HPV PRIME app (Windows)
The PRIME app runs inside **RStudio**, using the R programming language.
Follow these steps carefully; you only need to set this up once.

### Step 1. Install R and RTools
1.	Go to the R Project website: https://cran.r-project.org/
2.	Choose your operating system (Windows)
3.	Download **R version 4.5.1** *(R-4.5.1 for Windows)* and install it by running the .exe installer
4.	Download **RTools version 4.5** *(Rtools45 installer)* and install it by running the .exe installer
### Step 2. Install RStudio
1.	Go to the RStudio download page: https://posit.co/download/rstudio-desktop/
2.	Download **RStudio Desktop (free version)** for your operating system
3.	Install it by running the .exe installer (this will give you a nicer environment to run R code)
### Step 3. Install JAGS (a statistical engine required for the PRIME model)
1. Go to the SOURCEFORGE website: https://sourceforge.net/projects/mcmc-jags/files/
2. Download the latest version of **JAGS** 
3. Install it by running the .exe installer and follow the defaults
### Step 4. Download the app folder
1.	Download all folders in this Github repository
2.	Save it somewhere convenient on your computer
3. Unzip the folder
### Step 5. Open the app in RStudio
1.	Open **RStudio**
2.	From the top menu, click: **File** → **Open File**
3.	Browse into the **WHO_PRIME_app** folder.
4.	Select the file named **RShiny_PRIME_app.R** and click **Open** (this will open the app script in RStudio’s editor pane)
### Step 6. Run the app
1.	At the top-right of the RStudio editor window, you will see a button: **Run App** (with a small green arrow ▶)
2.	Click **Run App**
3.	RStudio will start the app and a window will open in your default browser showing the PRIME interface

### Notes
- At the first time you run the app, R will automatically download and install the required R packages (this may take a few minutes, depending on your internet connection)
- On later runs, the app will start much faster
