module Abstract

public alias Id = str;               
	  
public data PROGRAM = program(list[STATEMENT] stats);

public data EXP = state(str name)
 				| recursion(str name, EXP exp)
			    | transition(str name)
			    | action(str label, EXP right)
			    | sequential(EXP left, EXP right)
			    | choice(EXP left, EXP right)
			    | id(str name)
			    | natCon(int i)
			    | add(EXP left, int const)
			    | sub(EXP left, int const)
			    | rangeContext(int min, int max)
			    | naturalContext()
			    ;
     
public data STATEMENT = processStatement(str name, STATEMENT initialState, list[STATEMENT] states)
 					  | stateStatement(str name, EXP exp)
 					  | recursiveVarStatement(str name, str var, EXP exp)
 					  | recursiveConstStatement(str name, int const, EXP exp)
 					  | contRecursiveVarStatement(str name, str var, EXP exp, EXP context)
 					  | contRecursiveConstStatement(str name, int const, EXP exp, EXP context)
 					  | initialStatement(STATEMENT state);
      
anno loc PROGRAM@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, Id name, STATEMENT stat];
