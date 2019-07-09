      * COBOL PROGRAM CALCULATES AMORITIZATION TABLE AND WRITES IT OUT
      * TO AS A REPORT TO A SEQUENTIAL FILE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  AMORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STDIN ASSIGN TO KEYBOARD.

       DATA DIVISION.
       FILE SECTION.
       FD STDIN.
       01 CHUNK-OF-POST   PIC X(80).
      ******************************************************************
      * NUMERICAL BUFFERS FOR CORE MEMORY PROCESSING
      ******************************************************************
       WORKING-STORAGE SECTION.
           77 WS-COUNTER              PIC Z.
           77 NO-MORE-RECORDS         PIC X(1) VALUE SPACE.
           77 WS-TMP-STR              PIC X(80).
           77 WS-ARGS                 PIC X(50).
           77 WS-TMP                  PIC S9(14)V9(14).
           77 WS-MONTH-NO             PIC ZZ9.
           77 WS-INITIAL-AMOUNT       PIC S9(14)V9(14).
           77 WS-NO-OF-YEARS          PIC 99.
           77 WS-N                    PIC 999.
           77 WS-INTEREST-RATE        PIC S9(3)V9(3).
           77 WS-R                    PIC S9(1)V9(5).
           77 WS-START-AMOUNT         PIC S9(13)V9(13).
           77 WS-PAYMENT-AMOUNT       PIC S9(13)V9(13).
           77 WS-PRINCIPAL-AMOUNT     PIC S9(13)V9(13).
           77 WS-INTEREST-AMOUNT      PIC S9(13)V9(13).
           77 WS-FINISH-AMOUNT        PIC S9(13)V9(13) VALUE ZERO.
           77 WS-NO-MONTHS            PIC 9(3) VALUE 0.
           77 WS-TOTAL-INTEREST       PIC S9(13)V9(13) VALUE ZERO.
           77 WS-INTEREST-PAID        PIC $$,$$$,$$9.99.
      ******************************************************************
      * DELAY RESPONSE CHARACTER
      ******************************************************************
           77 WS-DUMMY-CHARACTER        PIC X VALUE 'X'.
      ******************************************************************
      * THESE VERSIONS OF THE RECORDS REINTERPRET THE NUMERIC FIELDS
      * ABOVE SO THAT THEY'RE PROPERLY FORMATTED AS DECIMAL VALUES
      ******************************************************************
           01 WS-AMORTIZATION-TABLE.
               05 WS-MONTHLY-RECORD OCCURS 360 INDEXED BY IDX.
                10 FILLER             PIC X.
                10 WS-PERIOD          PIC ZZZZ9.
                10 FILLER             PIC X.
                10 WS-BEGIN           PIC $,$$$,$$$,$$9.99.
                10 FILLER             PIC X.
                10 WS-PMT             PIC $,$$$,$$$,$$9.99.
                10 FILLER             PIC X.
                10 WS-P               PIC $,$$$,$$$,$$9.99.
                10 FILLER             PIC X.
                10 WS-I               PIC $,$$$,$$$,$$9.99.
                10 FILLER             PIC X.
                10 WS-END             PIC $,$$$,$$$,$$9.99.
           01 WS-TOTAL-INTEREST-PAID-BUFFER.
               05 INFO-FIELD          PIC X(22)
                   VALUE "TOTAL INTEREST PAID = ".
               05 VALUE-FIELD         PIC $$,$$$,$$9.99.
           01 WS-HDR-REC.
               05 MONTH-HDR           PIC X(6)  VALUE "MONTH".
               05 START-HDR           PIC X(16) VALUE "       START   ".
               05 PMT-HDR             PIC X(16) VALUE "         PMT   ".
               05 P-HDR               PIC X(16) VALUE "           P   ".
               05 I-HDR               PIC X(16) VALUE "           I   ".
               05 END-HDR             PIC X(16) VALUE "         END   ".
      ******************************************************************
      * MAIN PROGRAM
      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "Content-Type: text/html"
           DISPLAY X"0D"
           SET WS-COUNTER TO 0
           OPEN INPUT STDIN
           PERFORM FOREVER
              READ STDIN
              AT END
                 EXIT PERFORM
              END-READ
              MOVE CHUNK-OF-POST TO WS-TMP-STR
              IF WS-TMP-STR(1:1) IS NUMERIC
                 SET WS-COUNTER UP BY 1
                 IF WS-COUNTER IS EQUAL TO 1 THEN 
                    MOVE WS-TMP-STR TO WS-INITIAL-AMOUNT
                 END-IF
                 IF WS-COUNTER IS EQUAL TO 2 THEN
                    MOVE WS-TMP-STR TO WS-NO-OF-YEARS
                 END-IF
                 IF WS-COUNTER IS EQUAL TO 3 THEN
                    MOVE WS-TMP-STR TO WS-INTEREST-RATE
                 END-IF
              END-IF
           END-PERFORM           
           CLOSE STDIN
           IF (WS-INITIAL-AMOUNT <= 0 OR WS-INTEREST-RATE <= 0 OR WS-NO-
      -OF-YEARS <= 0) THEN
              DISPLAY "ABEND 054"
              STOP RUN
           END-IF
           PERFORM PROGRAM-PROCESSING.
           PERFORM PROGRAM-OUTPUT-HTML-HEADER.
           PERFORM PROGRAM-OUTPUT-HTML-TABLE.
           PERFORM PROGRAM-OUTPUT-HTML-FOOTER.
           GOBACK.
      ******************************************************************
      * this is the main processing section where we shall compute the
      * values to be loaded into the amortization table and also the
      * total no. of months necessary to pay off the loan, the total
      * interest paid, and why not, the total principal too.
      ******************************************************************
       PROGRAM-PROCESSING SECTION.
           SET IDX TO 1.
           DIVIDE WS-INTEREST-RATE BY 1200.0 GIVING WS-R.
           MULTIPLY WS-NO-OF-YEARS BY 12.0 GIVING WS-N.
           COMPUTE WS-TMP = (WS-INITIAL-AMOUNT * (1.0 + WS-R)**WS-N
      -         * WS-R) / ((1.0 + WS-R)**WS-N - 1.0).
           MOVE WS-TMP TO WS-PAYMENT-AMOUNT.
           MOVE WS-INITIAL-AMOUNT TO WS-START-AMOUNT.
           PERFORM UNTIL WS-START-AMOUNT - 0 < 0.01
      -          OR WS-NO-MONTHS >= 360
      ******************************************************************
      * CALCULATE THE PRINCIPAL AND INTEREST PAID FOR EACH MONTH AND
      ******************************************************************
            MULTIPLY WS-START-AMOUNT BY WS-R GIVING WS-INTEREST-AMOUNT
      *****************************************************************
      * DEAL WITH AN INSUFFICIENT PAYMENT
      *****************************************************************
            IF WS-PAYMENT-AMOUNT < WS-INTEREST-AMOUNT
               DISPLAY "INSUFFICIENT PAYMENT - LOAN CANNOT BE REPAID"
               STOP RUN
            END-IF
      ******************************************************************
      * INSUFFICIENT PAYMENT DEALT WITH
      ******************************************************************
            SUBTRACT WS-INTEREST-AMOUNT FROM WS-PAYMENT-AMOUNT
                GIVING WS-PRINCIPAL-AMOUNT
            SUBTRACT WS-PRINCIPAL-AMOUNT FROM WS-START-AMOUNT
                GIVING WS-FINISH-AMOUNT
            ADD WS-INTEREST-AMOUNT TO WS-TOTAL-INTEREST
      ******************************************************************
      * THEN WRITE THE CALCULATED VALUES TO THE PROPERLY FORMATTED
      * FIELDS OF THE MONTHLY RECORD ROW OF THE AMORTIZATION TABLE
      * BUFFER, AND THE TOTAL INTEREST TO THE INTEREST PAID FOR PROPER
      * FORMATTING
      ******************************************************************
            MOVE IDX TO WS-PERIOD(IDX)
            MOVE WS-START-AMOUNT TO WS-BEGIN(IDX)
            MOVE WS-INTEREST-AMOUNT TO WS-I(IDX)
            IF (WS-FINISH-AMOUNT > 0)
               MOVE WS-PAYMENT-AMOUNT TO WS-PMT(IDX)
               MOVE WS-PRINCIPAL-AMOUNT TO WS-P(IDX)
               MOVE WS-FINISH-AMOUNT TO WS-END(IDX)
            ELSE
               ADD WS-INTEREST-AMOUNT TO WS-START-AMOUNT
                   GIVING WS-PAYMENT-AMOUNT
               MOVE WS-PAYMENT-AMOUNT TO WS-PMT(IDX)
               MOVE WS-START-AMOUNT TO WS-P(IDX)
               MOVE 0 TO WS-END(IDX)
            END-IF
            SET IDX UP BY 1
            ADD 1 TO WS-NO-MONTHS
            MOVE WS-FINISH-AMOUNT TO WS-START-AMOUNT
           END-PERFORM
           MOVE WS-TOTAL-INTEREST TO WS-INTEREST-PAID
           MOVE WS-TOTAL-INTEREST TO VALUE-FIELD
           IN WS-TOTAL-INTEREST-PAID-BUFFER
           EXIT.
      ******************************************************************
      * ONCE WE HAVE ALL THE PROPERLY FORMATTED FIELDS OF THE MONTHLY
      * RECORDS WRITTEN TO THE AMORTIZATION TABLE BUFFER WE DUMP THEM
      * TO THE ACTUAL AMORTIZATION TABLE FILE RECORDS
      ******************************************************************
       PROGRAM-OUTPUT-HTML-TABLE SECTION.
           SET IDX TO 1.
           DISPLAY "<table id='amortTable'>"
           DISPLAY "<tr>" WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(MONTH-HDR), "</th>" 
           WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(START-HDR), "</th>" 
           WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(PMT-HDR), "</th>" 
           WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(P-HDR), "</th>" 
           WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(I-HDR), "</th>"  
           WITH NO ADVANCING
           DISPLAY "<th>", FUNCTION TRIM(END-HDR), "</th>" 
           WITH NO ADVANCING  
           DISPLAY "</tr>"
           PERFORM UNTIL IDX > WS-NO-MONTHS
              DISPLAY "<tr>" WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-PERIOD(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-BEGIN(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-PMT(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-P(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-I(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "<td>", FUNCTION TRIM(WS-END(IDX)), "</td>" 
              WITH NO ADVANCING
              DISPLAY "</tr>"
              SET IDX UP BY 1               
           END-PERFORM
           DISPLAY "</table>"
           DISPLAY "<p>", WS-TOTAL-INTEREST-PAID-BUFFER "</p>"
           EXIT.
      ******************************************************************
      * EMIT TOP OF HTML PAGE
      ******************************************************************
       PROGRAM-OUTPUT-HTML-HEADER SECTION.
           DISPLAY "<!DOCTYPE HTML>"
           DISPLAY "<html>"
           DISPLAY "<head>"
           DISPLAY "<meta charset='utf-8'>"
           DISPLAY "<meta name='viewport' content='width=device-width, "
           "initial-scale=1.0'>"
           DISPLAY "<meta name='description' content='COBOL AMORTIZATIO"
           "N TABULATOR'>"
           DISPLAY "<link id='tableStyle' media='all'>"
           DISPLAY "<title>COBOL AMORTIZATION TABULATOR</title>"
           DISPLAY "<script type='text/javascript' src='/amort.js' defe"
           "r></script>"
           DISPLAY "</head>"
           DISPLAY "<body>"
           DISPLAY "<header><p>COBOL AMORTIZATION TABULATOR</p></header"
           ">"
           DISPLAY "<div><a href='/index.html'>Home</a> | <a href='/amo"
           "rt.html'>Back</a></div>"
           DISPLAY "<h1>AMORTIZATION TABLE</h1>"
           EXIT.
      ******************************************************************
      * EMIT BOTTOM OF HTML PAGE
      ******************************************************************
       PROGRAM-OUTPUT-HTML-FOOTER SECTION.
           DISPLAY "<div><a href='/index.html'>Home</a> | <a href='/amo"
           "rt.html'>Back</a></div>"
           DISPLAY "<footer><p>Copyright &copy; 2019 Josh Roybal</p></f"
           "ooter>"
           DISPLAY "</body>"
           DISPLAY "</html>"
           EXIT.
