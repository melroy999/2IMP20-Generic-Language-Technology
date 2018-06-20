module Typecheck

import Prelude;
import Abstract;
import Load;

import List;

// Alias for the error collection.
alias ERR = list[tuple[loc l, str msg]];

// A symbol is defined as a tuple, with the symbol id and a min and max range for recursion.
// min = 1, max = -1 => natural number
// min = -1, max = -1 => variable is not a recursion variable.
alias SYM = tuple[Id Id, str name, int min, int max];

// Alias for symbol locations.
alias SYM_LOC = tuple[SYM symbol, loc location];

// =========================================================================================
//
// Pre-processing checks for variable declarations.
//
// =========================================================================================

// Get the defined range in the context based on the expression used.
public tuple[int min, int max] getRange(exp:rangeContext(int min, int max)) {
	// Take the declared min and max values.
	return <min, max>;
}

// Get the defined range in the context based on the expression used.
public tuple[int min, int max] getRange(exp:naturalContext()) {
	// min = 1, max = -1 => natural number
	return <1, -1>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:initialStatement(STATEMENT state)) {
	// Take whatever the state itself declares.
	return getDeclaredSymbol(state);
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:stateStatement(str name, EXP exp)) {
	// With the pair -1, -1 we denote that the variable takes the full range.
	return <<name, "", -1, -1>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:recursiveVarStatement(str name, str var, EXP exp)) {
	// This case should never occur, as the variable is unbounded!
	// Use special code -2.
	return <<name, var, -2, -2>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:recursiveConstStatement(str name, int const, EXP exp)) {
	// The symbol is recursive, with the variable of this instance being declared as a constant.
	return <<name, "<const>", const, const>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context)) {
	// Find the range.
	tuple[int min, int max] range = getRange(context);
	return <<name, var, range.min, range.max>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context)) {
	// The context does not matter here, as only const is declared.
	return <<name, "<const>", const, const>, stat@location>;
}

// Fetch all symbols used in the statements, and report errors where neccessary.
public tuple[list[SYM] symbols, map[STATEMENT, SYM] decs, ERR errors] getAndValidateSymbols(STATEMENT initialState, list[STATEMENT] states, ERR errors) {
	// Start by finding all the declared symbols, since we allow state names to be used before declaration.
	symbols = [];
	map[STATEMENT, SYM] decs = ();
	
	for(state <- initialState + states) {
		<symbol, location> = getDeclaredSymbol(state);
		decs[state] = symbol;
		
		if(symbol.min == -2) {
			// Invalid construct, error and skip.
			errors += <location, "The recursion variable \'<symbol.Id>\' is undefined (a in operator with a range is required).">;
			continue;
		}
		
		// Check whether the state already exists in one form or another.
		for(s <- symbols) {
			if(s.Id == symbol.Id) {
				// The state names match, we possibly have overlap.
				if(symbol.max == -1) {
					// The current state has infinite range, implying a natural or non-recursive variable. 
					if(symbol.min == -1) {
						// The variable is a non-recursive variable. In any case, we have a conflict.
						errors += <location, "The definition exists already as a recursive state with id \'<symbol.Id>\'.">;
						break;
					} else if(s.min != s.max || s.min != 0) {
						// We only allow min = max = 0, since it is the only free value with a natural definition.
						errors += <location, "The definition overlaps with an existing recursive state with id \'<symbol.Id>\'.">;
						break;
					}
				} else {
					// We have a well-defined range, what about the variable we compare to?
					if(s.max == -1) {
						// s is an infinite range variable, implying a natural or non-recursive variable.  
						if(s.min == -1) {
							// The variable is a non-recursive variable. In any case, we have a conflict.
							errors += <location, "The definition exists already as a non-recursive state with id \'<symbol.Id>\'.">;
							break;
						} else if(symbol.min != symbol.max || symbol.min != 0) {
							// We only allow min = max = 0, since it is the only free value with a natural definition.
							errors += <location, "The definition overlaps with an existing recursive state with id \'<symbol.Id>\'.">;
							break;
						}
					} else {
						// Both are well defined ranges. Look for an overlap.
						if(symbol.min < s.min) {
							if(symbol.max >= s.min) {
								// We have overlap.
								errors += <location, "The definition overlaps with an existing recursive state with id \'<symbol.Id>\'.">;
							}
						} else {
							if(s.max >= symbol.min) {
								// We have overlap.
								errors += <location, "The definition overlaps with an existing recursive state with id \'<symbol.Id>\'.">;
							}
						}
					}
				}
			}
		} 
		
		// The variables are distinct, add the symbol to the list.
		symbols += symbol;
	}
	
	return <symbols, decs, errors>;
}








public ERR checkExpression(exp:state(str name), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// Check if the state is declared. Next to that, check whether the variable source is non-recursive.
	if(name != "1" && name != "0") {
		for(symbol <- symbols) {
			if(symbol.Id == name) {
				// We have a match, is it a non-recursive variable?
				if(symbol.min != -1) {
					errors += <exp@location, "The state \'<name>\' is defined to be a recursive state, and should be accompanied by a recursive variable.">;
				} 
				return errors;
			}
		}
		
		// If we reach this point, the variable is not declared.
		errors += <exp@location, "The state \'<name>\' has not been declared.">;
	}
	return errors;
}

public ERR checkExpression(exp:recursion(str name, EXP exp), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// Check if the name is declared, and whether the variable is compatible with the declaration.
	compatibles = [];
	for(symbol <- symbols) {
		if(symbol.Id == name) {
			compatibles += symbol;
		}
	}
	
	// If we have no compatible variables, we have an undefined variable.
	if(isEmpty(compatibles)) {
		errors += <exp@location, "The state \'<name>\' has not been declared.">;
		return errors;
	}
	
	// Are the given variables compatible?
	if(any([c.min == -1 | c <- compatibles])) {
		// We have a non-recursive variable as an option.
		errors += <exp@location, "The state \'<name>\' is defined to be a non-recursive state.">;
	}
	
	// Is the range associated with the expression present in the compatible variables?
	// TODO What is the range declared by the context? We cannot determine the validity without it.
	

	// TODO.
	// Check whether state is declared.
	// Other stuff?
	return errors;
}

public ERR checkExpression(exp:transition(str name), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// Trivially true.
	return errors;
}

public ERR checkExpression(exp:action(EXP left, EXP right), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// TODO left HAS to yield a transition.
	return checkExpression(left, checkExpression(right, errors, symbols, decs), symbols, decs);
}

public ERR checkExpression(exp:sequential(EXP left, EXP right), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	return checkExpression(left, checkExpression(right, errors, symbols, decs), symbols, decs);
}

public ERR checkExpression(exp:choice(EXP left, EXP right), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	return checkExpression(left, checkExpression(right, errors, symbols, decs), symbols, decs);
}

public ERR checkStatement(stat:stateStatement(str name, EXP exp), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	return checkExpression(exp, errors, symbols, decs);	
}

public ERR checkStatement(stat:initialStatement(STATEMENT state), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	return checkStatement(state, errors, symbols, decs);
}



public ERR checkStatement(stat:recursiveVarStatement(str name, str var, EXP exp), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// TODO.
	return errors;
}

public ERR checkStatement(stat:recursiveConstStatement(str name, int const, EXP exp), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// TODO.
	return errors;
}

public ERR checkStatement(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// TODO.
	return errors;
}

public ERR checkStatement(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context), ERR errors, list[SYM] symbols, map[STATEMENT, SYM] decs) {
	// TODO.
	return errors;
}








/*
 * Since the scope of the symbols is local, we should look for symbols and errors in succession.
 */
public tuple[ERR errors, list[str] names] checkStatement(stat:processStatement(str name, STATEMENT initialState, list[STATEMENT] states), ERR errors, list[str] pNames) {
	
	// Make sure that the process name is unique.
	if(lastIndexOf(pNames, name) != -1) {
		errors += <stat@location, "The process \'<name>\' has already been declared.">;
	} else {
		pNames += name;
	}

	// Fetch and validate the symbols.
	<symbols, decs, errors> = getAndValidateSymbols(initialState, states, errors);
	
	// Start checking the states and consequently, the expressions.
	errors = checkStatement(initialState, errors, symbols, decs);
	for(state <- states) {
		errors = checkStatement(state, errors, symbols, decs);
	}
	
	return <errors, pNames>;
}

public ERR checkStatements(list[STATEMENT] statements) {
	errors = [];
	pNames = [];
	
	for(statement <- statements) {
		<errors, pNames> = checkStatement(statement, errors, pNames);
	}
	
	return errors;
}

public ERR checkProgram(PROGRAM P){                                                
  if(program(list[STATEMENT] statements) := P){
  	
  
    return checkStatements(statements);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt));