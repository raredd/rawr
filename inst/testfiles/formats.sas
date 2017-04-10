/*
  sas format file
*/

proc format;
  value $JC 'man'  = 'management'
            'nman' = 'non-management'
            "_"    = "something"
            "_se"  ="something else ";
  value
    $JCa    'man'  = 'management'
            'nman' = 'non-management' ;
            
  value
    $JCb    'man'  = 'management' 'nman' = 'non-management' ;
  value rate
         * missing values;
         -99 = 'not applicable'
          -1 = 'missing'
          * bad responses ;
           0 = 'terrible'
           1 = 'poor'
           * okay;
           2 = 'fair'
           * good responses;
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
           
           /*
             ignore this one
           4 = "outstanding";
           */
           
           4 = 'excellent';
run;
