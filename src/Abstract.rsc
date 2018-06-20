module Abstract

public alias Id = str;               
	  
public data PROGRAM = program(list[PROCESS_STATEMENT] stats);

public data EXP = state(str name)
			    | recState(str name, REC_EXP exp)
			    | transition(str name)
			    | action(EXP left, EXP right)
			    | sequential(EXP left, EXP right)
			    | choice(EXP left, EXP right)
			    ;
     
public data REC_EXP = id(str name)
				    | natCon(int natCon)
				    | add(REC_EXP left, REC_EXP right)
				    | sub(REC_EXP left, REC_EXP right)
					;

public data RANGE_EXP = rangeContext(str var, int min, int max)
 					  | naturalContext(str var)
 					  ;

public data PROCESS_STATEMENT = processStatement(str name, INITIAL_STATEMENT initialState, list[(SIMPLE_STATE_STATEMENT | RECURSIVE_STATEMENT)] states);

public data SIMPLE_STATE_STATEMENT = stateStatement(str name, EXP exp);

public data RECURSIVE_STATEMENT = recursiveStatement(str name, str var, EXP exp)
								| recursiveStatement(str name, int const, EXP exp)
								| recursiveStatement(str name, str var, EXP exp, RANGE_EXP context)
								| recursiveStatement(str name, int const, EXP exp, RANGE_EXP context);			

public data INITIAL_STATEMENT = initialStatement((SIMPLE_STATE_STATEMENT | RECURSIVE_STATEMENT) stat);
      
anno loc PROGRAM@location;
anno loc EXP@location;
anno loc PROCESS_STATEMENT@location;
anno loc SIMPLE_STATE_STATEMENT@location;
anno loc RECURSIVE_STATEMENT@location;
anno loc INITIAL_STATEMENT@location;

public alias Occurrence = tuple[loc location, Id name, (PROCESS_STATEMENT | SIMPLE_STATE_STATEMENT | RECURSIVE_STATEMENT | INITIAL_STATEMENT) stat];
