module Abstract

public alias Id = str;               
	  
public data PROGRAM = program(list[STATEMENT] stats);

public data EXP = state(str name)
			    | transition(str name)
			    | action(EXP left, EXP right)
			    | sequential(EXP left, EXP right)
			    | choice(EXP left, EXP right)
			    ;
     
public data STATEMENT = processStatement(str name, STATEMENT initialState, list[STATEMENT] states)
 					  | stateStatement(str name, EXP exp)
 					  | initialStatement(STATEMENT state);
      
anno loc PROGRAM@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, Id name, STATEMENT stat];
