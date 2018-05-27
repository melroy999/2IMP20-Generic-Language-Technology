module Abstract

// Added the boolean() type to the types.
public data TYPE = natural() | string() | boolean();    
	  
public alias PicoId = str;                  
	  
public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(PicoId name, TYPE tp);

// Added the boolCon, not, equals, not equals, and and or expressions to the expressions.
public data EXP = 
       id(PicoId name)
     | natCon(int iVal)
     | strCon(str sVal)
     | boolCon(bool bVal)
     | not(EXP exp)
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
     | conc(EXP left, EXP right)
     | equals(EXP left, EXP right)
     | nequals(EXP left, EXP right)
     | and(EXP left, EXP right)
     | or(EXP left, EXP right)
     ;

// Added the for statement to the statements. 
public data STATEMENT =
       asgStat(PicoId name, EXP exp)
     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
     | whileStat(EXP exp, list[STATEMENT] body)
     | forStat(STATEMENT init, EXP cond, STATEMENT inc, list[STATEMENT] body)
     ;

anno loc TYPE@location;                   
anno loc PROGRAM@location;
anno loc DECL@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, PicoId name, STATEMENT stat];