module Abstract
	  
public alias Id = str;               
	  
// public data PROGRAM =                       
//  program(list[STATEMENT] stats);

public data EXP = 
       state(str name)
     | transition(str name)
     | action(EXP left, EXP right)
     | sequential(EXP left, EXP right)
     | choice(EXP left, EXP right)
     ;

// Added the for statement to the statements. 
//public data STATEMENT =
//       asgStat(PicoId name, EXP exp)
//     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
//     | whileStat(EXP exp, list[STATEMENT] body)
//     | forStat(STATEMENT init, EXP cond, STATEMENT inc, list[STATEMENT] body)
//     ;
                
// anno loc PROGRAM@location;
anno loc EXP@location;
// anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, Id name, STATEMENT stat];