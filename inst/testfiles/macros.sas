/*
macros.sas

this is a sample file with some sas macros
rawr::get_margs should find any lines containing "%macro" and 
evaluate them for being valid %macro calls

these above are not returned since these are not valid macro statements

this algorithm is not perfect:
a line containing "%macro" and something (in parentheses)
would be incorrectly interpreted by get_margs as a valid macro

another test: %macro (not, a, macro);

** %macro;     * not interpreted as a macro since there are no parentheses;
** %macro; ( ) * this would be, however;
** %macro; ()  * nothing inside () so not included;

*** UPDATE: fixed above by ignoring everything after first semicolon;

*/

** macro1;

%macro macro1(arg1, arg2); * need to fix: this text should not appear in mname;
data data;
  set data;
run;
%mend;

** macro2 :

%macro macro2(arg1 = 1, arg2 = 2, arg3 = 3);
%mend;

** macro3;

%macro macro3(arg1=,arg2=);
%mend;

** macro4;

%macro macro4(this=,
              macro=,
              has=,
              many=,
              params=,
              on=,
              multiple=,
              lines=);
%mend;

** macro5--special case;

%macro macro5(this= /* this */,
              macro= /* macro */,
              has= /* writer */,
              comments= /* is */,
              between= /* being */,
              each= /* very */,
              parameter= /* annoying */)
      ;
%mend;

/*
end macros.sas
*/
