# WHO HPV PRIME
Papillomavirus Rapid Interface for Modelling and Economics (PRIME) is a static modelling tool designed to estimate the health and economic impact of HPV vaccination in girls before sexual debut, specifically for cervical cancer prevention. 

Instructions for running the WHO HPV PRIME app (Windows)
The PRIME app runs inside RStudio, using the R programming language.
Follow these steps carefully; you only need to set this up once.
Step 1. Install R and RTools
1.	Go to the R Project website: https://cran.r-project.org/
2.	Choose your operating system (Windows).
3.	Download and install R version 4.5.1 (R-4.5.1 for Windows).
o	On Windows: run the .exe installer you downloaded.
4.	Download and install RTools version 4.5 (Rtools45 installer) 
o	On Windows: run the .exe installer you downloaded.
Step 2. Install RStudio
1.	Go to the RStudio download page: https://posit.co/download/rstudio-desktop/
2.	Download RStudio Desktop (free version) for your operating system.
3.	Install it (this will give you a nicer environment to run R code).
Step 3. Install JAGS (required for the PRIME model)
The PRIME model uses a statistical engine called JAGS. Please install it before running the app:
•	Windows:
Download the Windows installer from
https://sourceforge.net/projects/mcmc-jags/files/
→ Run the installer and follow the defaults.
Step 4. Download the app folder
1.	Obtain  the folder named WHO_PRIME_app (this contains the Shiny app codes).
2.	Save it somewhere convenient on your computer, e.g. in Documents or Desktop.
→ Make sure the folder name is not changed.
Step 5. Open the app in RStudio
1.	Open RStudio.
2.	From the top menu, click: File → Open File…
3.	Browse into the WHO_PRIME_app folder.
4.	Select the file named RShiny_PRIME_app.R and click Open.
o	This will open the app script in RStudio’s editor pane.
Step 6. Run the app
1.	At the top-right of the RStudio editor window, you will see a button:
Run App (with a small green arrow ▶).
 
2.	Click Run App.
3.	RStudio will start the app. A window will open in your default browser showing the PRIME interface.
 
Notes
•	The first time you run the app, R will automatically download and install the required R packages (this may take a few minutes, depending on your internet connection).
•	On later runs, the app will start much faster.


