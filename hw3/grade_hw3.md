*Zanyu Shi*

### Overall Grade: 97/100

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline?

    Yes. `Mar 1, 2019, 5:29 PM PST`.

-   Is the final report in a human readable format html?

    Yes, `html`.

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report?

    Yes. 

### Correctness and efficiency of solution: 50/50

-   Q1 (22/25)
    -  (-3 pts) \#6. Visualize any other information you are interested in.
-   Q2 (28/25)
	- (+3 pts) \#8. 
    


### Usage of Git: 8/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

    Yes.

-   Are there enough commits? Are commit messages clear? (-2 pts)

    7 commits in develop branch for homework 3. **Make sure** to start version control from the very beginning of a project. Make as many commits as possible during the process. 
    
    
- 	Is the hw3 submission tagged?  
	
	Yes. 

-   Are the folders (`hw1`, `hw2`, ...) created correctly?

    Yes.

-   Do not put a lot auxillary files into version control.

    Yes.

### Reproducibility: 9/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? (-1 pt)

    - Use relative path for reproducibility. For example, use

    ``` r
    saveRDS(LACEP, file = "./LACEP-app/LACEPdat.rds")
    ```

    instead of the path unique to your account on the server:

    ``` r
    saveRDS(LACEP, file = "/home/zyshi/biostat-m280-2019-winter/hw3/LACEP-app/LACEPdat.rds")
    ```

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Yes.

### R code style: 20/20

-   [Rule 3](https://google.github.io/styleguide/Rguide.xml#linelength): Never place more than 80 characters on a line. 

  
-   [Rule 4](https://google.github.io/styleguide/Rguide.xml#indentation): 2 spaces for indenting.

-   [Rule 5](https://google.github.io/styleguide/Rguide.xml#spacing): Place spaces around all binary operators (`=`, `+`, `-`, `<-`, etc.). Exception: Spaces around `=`'s are optional when passing parameters in a function call.

-   [Rule 5](https://google.github.io/styleguide/Rguide.xml#spacing): Do not place a space before a comma, but always place one after a comma.

-   [Rule 5](https://google.github.io/styleguide/Rguide.xml#spacing): Place a space before left parenthesis, except in a function call. Do not place spaces around code in parentheses or square brackets. Exception: Always place a space after a comma.