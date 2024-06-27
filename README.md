# r_graphics

How to setup cron on windows

Required: R 4.2.2 or later on your local PC. Get it application from the Apps Portal.

Step 1. Download this repo to your local PC. Recommended to save somewhere like "C:\wc" or "C:\Users\YourUser\Documents"

Step 2. Extract the zip. You should have a folder called "r_graphics".

Step 3. Open "MVS_ReservoirStatusGraphics.R" in "r_graphics" with a text editor and make changes to 4 lines of code. 

Step 4. Edit "MVS_ReservoirStatusGraphics.R". 
        Chnage the path at 4 locations at line 64,65,68, and 69 to match where you extracted the "r_graphics" folder. Save and edit text editor. 

Step 5. Right Click on "RunMyRScript.bat" and select "Edit"
        Edit line 4 "C:\Program Files\R\R-4.2.2\bin\Rscript.exe". make sure the path to "Rscript.exe" is correct. Save and exit.

Step 6. Make sure the path to your "wc20200918.ppk" and "id_rsa.ppk" is correct and is working. Save and exit.

Step 7. Doudle click on "RunMyRScript.bat" and test our the script manually. check "output.log" for for details.

Step 8. Setup "Task Scheduler" on your windows computer.
        - Type "Task Scheduler" on the search bar at bottom left (next to windows logo)
        - Click on "Task Scheduler"
        - Click on "Create Task" on the right side under "Actions" > "Task Scheduler Library"

        - Under General
            - Name = "MVS_ReservoirStatusGraphics"
            - leave everything else as default

        - Under Triggers
            - Create a "New' and set to repeat task every hour and duration equal to indefinitely

        - Under Actions
            - Create "New" and browse to "RunMyRScript.bat"

        - Under Condtions/Settings/ and History
            - leave as default

Step 9. Done