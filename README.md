# F14 Tomcat

F14 Tomcat is a program for tricking other programs such as Microsoft Teams into thinking you are active at the keyboard.  It does this by simulating a press of the F14 key every few minutes.  To the casual observer of your screen, however, it is supposed to look like a program serving you chirpy messages to keep you feeling positive.


## Installation

No installation of the executable is required.  It will run as-is.


## Usage

The program can be run as-is, but will also accept two parameters: interval (specifying the number of minutes as an integer between simulated key presses) and quotefile (identifying a text file containing alternatives, one quote per line, to the hardcoded quotes within the program).

Example: The following command line will run the program, setting the interval between simulated key-presses to 3 minutes and using the file "my list.txt" from the c:\docs folder to provide the quotations.

```bat
   f14tomcat.exe -interval 3 -quotefile "c:\docs\my list.txt"
```
   

## Build With

Lazarus


## Author

**Hamish A Williams**

- [Email](mailto:f14tomcat@heuristicadvances.co.uk?subject=F14%20Tomcat)


## License

With the exception of the two aircraft images provided, everything in this project is licensed under the Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0).

The original aircraft images are available on wiki media at https://commons.wikimedia.org/wiki/File:F-14_Tomcat_DF-SD-06-03497.jpg and https://commons.wikimedia.org/wiki/File:F-14_Tomcat_on_Carrier.jpeg.  The images have been cut and resized to fit in the image panel.
