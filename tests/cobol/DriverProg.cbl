      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. DriverProg.
AUTHOR.  Michael Coughlan. 
* This program demonstrates the use of the CALL verb
* it calls three external sub-programs that help to demonstrate
* some of the features of the CALL.
* The "MultiplyNums" sub-program takes five parameters.  The first two
* are the numbers to be multiplied, the second two are strings to
* demonstrate that strings can be passed as parameters and the
* last is the returned result of multiplying the two numbers.
* The "Fickle" sub-program demonstrates a program that exhibits 
* State Memory.
* The "Steadfast" sub-program demonstrates how a sub-program that
* uses the IS INITIAL phrase can avoid State Memory.

ENVIRONMENT DIVISION.
DATA DIVISION.

WORKING-STORAGE SECTION.
01 UserNumber         PIC 99.

01 PrnResult          PIC 9(6).
* field declared as COMP cannot be DISPLAYed
* it is necessary to move it to a DISPLAY field.
* DISPLAY is the default value for a field and
* need not be declared.
  

* Parameters must be either 01-level's or elementry
* data-items. 
01 Parameters.
   02 Number1         PIC 9(3).
   02 Number2         PIC 9(3).
   02 FirstString     PIC X(19) VALUE "First parameter  = ".
   02 SecondString    PIC X(19) VALUE "Second parameter = ".
   02 Result          PIC 9(6) COMP.
*  I've made this a COMP field to demonstrate that COMP 
*  items can be passed as parameters but a COMP field cannot
*  be DISPLAYed and so is moved to a DISPLAY field before DISPLAYing it.



PROCEDURE DIVISION.
Begin.
    PERFORM CallMultiplyNums.
    PERFORM CallFickle
    PERFORM CallSteadfast

    PERFORM MakeFickleSteadfast.

    STOP RUN.


CallMultiplyNums.
    DISPLAY "Input 2 numbers (3 digits each)  to be multiplied"
    DISPLAY "First number -  " WITH NO ADVANCING
    ACCEPT Number1
    DISPLAY "Second number - " WITH NO ADVANCING
    ACCEPT Number2.
    DISPLAY "The first string  is " FirstString.
    DISPLAY "The second string is " SecondString.
    DISPLAY ">>>>>>>>> Calling the sub-program now".

    CALL "MultiplyNums"
         USING BY CONTENT Number1, Number2, FirstString,
               BY REFERENCE SecondString, Result.

*   The USING phrase specifies the parameters to be passed to the
*   sub-program. The order of the parameters is important as the
*   sub-program recognizes them by relative location not by name
*
*   Parameters should be passed BY CONTENT when you are not expecting
*   them to get a value from the called program.  We have not passed
*   SecondString by content and you can see that its value is
*   overwritten by the called program.

    DISPLAY "Back in the main program now <<<<<<<<<<<".
    MOVE Result to PrnResult.
    DISPLAY Number1 " multiplied by " Number2 " is = " PrnResult.

    DISPLAY "The first string is  " FirstString.
    DISPLAY "The second string is " SecondString.


CallFickle.
    DISPLAY SPACE
    DISPLAY "------------------- Calling Fickle ---------"
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber.
*   Every time I call Fickle with the same value
*   produces a different result.  This is because
*   it remembers its state from one call to the next.
*   It has "State Memory".


CallSteadFast.
    DISPLAY SPACE
    DISPLAY "------------------- Calling Steadfast ---------"
    MOVE 10 TO UserNumber
    CALL "Steadfast" USING BY CONTENT UserNumber
    MOVE 10 TO UserNumber
    CALL "Steadfast" USING BY CONTENT UserNumber
    MOVE 10 TO UserNumber
    CALL "Steadfast" USING BY CONTENT UserNumber.
*   Every time I call Steadfast with the same value
*   it produces the same result.  We have eliminated
*   State Memory by using the IS INITIAL phrase in
*   Steadfast


MakeFickleSteadfast.
    DISPLAY SPACE
    DISPLAY "----- Making fickle act like Steadfast -------"
    CANCEL "Fickle"
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber

    CANCEL "Fickle"
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber

    CANCEL "Fickle"
    MOVE 10 TO UserNumber
    CALL "Fickle" USING BY CONTENT UserNumber.
*   We can make Fickle act like Steadfast by using
*   the CANCEL verb to set it into its initial state
*   each time we call it
