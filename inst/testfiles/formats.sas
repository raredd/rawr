/*
  sas format file
*/

proc format;
  value jc 1 = 'management'
           2 = 'non-management';
  value rate 
         -99 = 'not applicable'
          -1 = 'missing'
           0 = 'terrible'
           1 = 'poor'
           2 = 'fair'
           3 = 'good'
           4 = 'excellent';
  value with_comments
         -99 = 'not applicable'
          -1 = 'missing'
           0 = 'terrible' /* comment */
           1 = 'poor'
           2 = 'fair'
           3 = 'good, comma'
           /* 
             long
             comment
           */
           4 = 'excellent';
run;
