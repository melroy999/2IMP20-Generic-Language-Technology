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
// Here the name corresponds to the recursive variable name.
// rmin and rmax correspond to the range as defined by the context. Both will be -1 if undefined.
alias SYM = tuple[Id Id, str name, int min, int max, int rmin, int rmax];

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
	return <<name, "", -1, -1, -1, -1>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:recursiveVarStatement(str name, str var, EXP exp)) {
	// This case should never occur, as the variable is unbounded!
	// Use special code -2.
	return <<name, var, -2, -2, -1, -1>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:recursiveConstStatement(str name, int const, EXP exp)) {
	// The symbol is recursive, with the variable of this instance being declared as a constant.
	return <<name, "<const>", const, const, -1, -1>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context)) {
	// Find the range.
	tuple[int min, int max] range = getRange(context);
	return <<name, var, range.min, range.max, range.min, range.max>, stat@location>;
}

// Get the symbol declared by the statement, with recursive bounds if possible.
public SYM_LOC getDeclaredSymbol(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context)) {
	// The context does not matter here, as only const is declared.
	tuple[int min, int max] range = getRange(context);
	return <<name, "<const>", const, const, range.min, range.max>, stat@location>;
}

// Fetch all symbols used in the statements, and report errors where neccessary.
public tuple[list[SYM] symbols, ERR errors] getAndValidateSymbols(STATEMENT initialState, list[STATEMENT] states, ERR errors) {
	// Start by finding all the declared symbols, since we allow state names to be used before declaration.
	symbols = [];
	
	for(state <- initialState + states) {
		<symbol, location> = getDeclaredSymbol(state);
		
		if(symbol.min == -2) {
			// Invalid construct, error and skip.
			errors += <location, "The recursion variable \'<symbol.Id>\' is undefined, an in operator with a range is required.">;
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
	
	return <symbols, errors>;
}

// =========================================================================================
//
// Check whether the given expression is guarded.
//
// =========================================================================================

public list[bool] isGuarded(exp:state(str name)) {
	// Note that 1 and 0 are always guarded.
	return name == "0" || name == "1" ? [true] : [false];
}

public list[bool] isGuarded(exp:recursion(str name, EXP var)) {
	return [false];
}

public list[bool] isGuarded(exp:action(str label, EXP var)) {
	return [true | b <- isGuarded(var)];
}

public list[bool] isGuarded(exp:choice(EXP left, EXP right)) {
	return isGuarded(left) + isGuarded(right);
}

// =========================================================================================
//
// Checks to find otiose range statements.
//
// =========================================================================================

public bool isRangeRequired(exp:state(str name)) {
	return false;
}

public bool isRangeRequired(exp:recursion(str name, EXP var)) {
	<varId, offset> = getRecursionVariable(var);
	
	try {
		toInt(varId);
		return false;
	} catch IllegalArgument: {
		// It is a string, so a recursion variable is used.
		return true;
	}
}

public bool isRangeRequired(exp:action(str label, EXP var)) {
	return isRangeRequired(var);
}

public bool isRangeRequired(exp:choice(EXP left, EXP right)) {
	return isRangeRequired(left) || isRangeRequired(right);
}

// =========================================================================================
//
// Checks in which the initial state is checked for simplicity.
//
// =========================================================================================

public bool isStateSimple(stat:stateStatement(str name, EXP exp)) {
	return true;
}

public bool isStateSimple(stat:recursiveVarStatement(str name, str var, EXP exp)) {
	return false;
}

public bool isStateSimple(stat:recursiveConstStatement(str name, int const, EXP exp)) {
	return true;
}

public bool isStateSimple(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context)) {
	return false;
}

public bool isStateSimple(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context)) {
	return true;
}

// =========================================================================================
//
// Validating statements and expressions.
//
// =========================================================================================

public tuple[str var, int offset] getRecursionVariable(exp:id(str name)) {
	return <name, 0>;
}

public tuple[str var, int offset] getRecursionVariable(exp:natCon(int i)) {
	return <"<i>", 0>;
}

public tuple[str var, int offset] getRecursionVariable(exp:add(str left, int const)) {
	return <left, const>;
}

public tuple[str var, int offset] getRecursionVariable(exp:sub(str left, int const)) {
	return <left, -const>;
}

public ERR checkExpression(exp:state(str name), ERR errors, list[SYM] symbols, SYM symbol) {
	// Check if the state is declared. Next to that, check whether the declared variable source is non-recursive.
	if(name != "1" && name != "0") {
		for(s <- symbols) {
			if(s.Id == name) {
				// We have a match, is it a non-recursive variable?
				if(s.min != -1) {
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

public ERR checkExpression(exp:recursion(str name, EXP var), ERR errors, list[SYM] symbols, SYM symbol) {
	// Check if the name is declared, and whether the variable is compatible with the declaration.
	compatibles = [];
	for(s <- symbols) {
		if(s.Id == name) {
			compatibles += s;
		}
	}
	
	// If we have no compatible variables, we have an undefined variable.
	if(isEmpty(compatibles)) {
		errors += <exp@location, "The state \'<name>\' has not been declared.">;
		return errors;
	}
	
	// Are the given variables compatible?
	if(any(c <- compatibles, c.min == -1)) {
		// We have a non-recursive variable as an option.
		errors += <exp@location, "The state \'<name>\' is defined to be a non-recursive state.">;
		return errors;
	}
	
	// Given the expression, should a context be given?
	<varId, offset> = getRecursionVariable(var);
	try {
		// We expect here to receive an constant integer recursion variable.
		int varValue = toInt(varId);
		
		// Is the state connected to the integer recursion variable declared?
		bool isDeclared = false;
		for(c <- compatibles) {
			// What kind of recursive variable are we looking at?
			if(c.max == -1) {
				// natural number, starting at 1. So report error if varValue is 0.
				if(varValue != 0) {
					isDeclared = true;
					break;
				}
			} else if(c.min == c.max) {
				// constant.
				if(varValue == c.min) {
					isDeclared = true;
					break;
				}
			} else {
				// It must be a range.
				if(c.min <= varValue && varValue <= c.max) {
					isDeclared = true;
					break;
				}
			}
		}
		
		if(!isDeclared) {
			errors += <exp@location, "The given recursive state \'<name>\'(n) is undefined for recursive variable \'<varId>\'.">;
		}
	} catch IllegalArgument: {
		// The variable is not an int, since we had an error.
		if(symbol.rmin == -1) {
			// The recursion variable is undeclared, throw an error.
			errors += <exp@location, "The recursion variable \'<varId>\' requires a context.">;
			return errors;
		}
		
		// Is the recursion variable used in the expression declared?
		if(varId != symbol.name) {
			errors += <exp@location, "The recursion variable \'<varId>\' is undeclared (it should instead use the variable \'<symbol.name>\').">;
			return errors;
		}
		
		// Is the range of the recursion variable declared? We have two cases: the range is natural or defined:
		if(symbol.rmax == -1) {
			// The range is natural, base the decision on the offset.
			if(offset < -1) {
				errors += <exp@location, "The recursive variable \'<varId>\'(n) is not allowed to be negative.">;
				return errors;
			} else {
				// Any natural range is valid.
				if(!any(c <- compatibles, c.max == -1)) {
					errors += <exp@location, "The recursive state \'<name>\'(n) requires a recursion variable with natural range.">;
				}
				
				// For offset -1, we require the state with recursive variable 0 to also be defined.
				if(offset == -1) {
					if(!any(c <- compatibles, c.min == 0)) {
						errors += <exp@location, "The recursive state \'<name>\'(n) is undefined for \'<name>\'(0).">;
					}
				}
			}
		} else {
			// The range is defined. We have to check whether the entire range is covered.
			range_min = symbol.rmin + offset;
			range_max = symbol.rmax + offset;
			
			// We do not allow negative recursion variables.
			if(range_min < 0) {
				errors += <exp@location, "The recursive variable \'<varId>\'(n) is not allowed to be negative.">;
				return errors;
			}
			
			// Is the entire range covered by the compatible variables?
			if(any(c <- compatibles, c.max == -1)) {
				if(range_min > 0) {
					// Yes, there is a natural variable covering the entire thing.
					return errors;
				} else {
					// If 0 is covered, we are good.
					if(any(c <- compatibles, c.min == 0)) {
						return errors;
					}
				}
			}
			
			// We have no natural ranges, so we get to puzzle. Sort the compatibles on min, and check if the entire range is covered.
			sortedCompatibles = sort(compatibles, bool (SYM a, SYM b) { return a.min < b.min; });
			
			for(c <- sortedCompatibles) {
				if(c.min <= range_min && range_min <= c.max) {
					// We have covered at least up to max of this symbol, so increment.
					range_min = c.max + 1;
				}
			}
			
			// If range_max < range_min, we have, by above, covered all options.
			if(range_max < range_min) {
				return errors;
			}
			
			// Otherwise, we do not cover the entire range.
			errors += <exp@location, "The recursive state \'<name>\'(n) is undefined for \'<name>\'(<range_min>).">;
		}
	}
	
	return errors;
}

public ERR checkExpression(exp:action(str label, EXP right), ERR errors, list[SYM] symbols, SYM symbol) {
	return checkExpression(right, errors, symbols, symbol);
}

public ERR checkExpression(exp:choice(EXP left, EXP right), ERR errors, list[SYM] symbols, SYM symbol) {
	return checkExpression(left, checkExpression(right, errors, symbols, symbol), symbols, symbol);
}

public ERR checkExpression(exp:action(EXP left, EXP right), ERR errors, list[SYM] symbols, SYM symbol) {
	return checkExpression(left, checkExpression(right, errors, symbols, symbol), symbols, symbol);
}

public ERR checkExpression(exp:id(str name), ERR errors, list[SYM] symbols, SYM symbol) {
	// This expression is simple, and has no errors.
	return errors;
}

public ERR checkExpression(exp:natCon(int i), ERR errors, list[SYM] symbols, SYM symbol) {
	// This expression is simple, and has no errors.
	return errors;
}

public ERR checkExpression(exp:add(str left, int const), ERR errors, list[SYM] symbols, SYM symbol) {
	// This expression is simple, and has no errors.
	return errors;
}

public ERR checkExpression(exp:sub(str left, int const), ERR errors, list[SYM] symbols, SYM symbol) {
	// This expression is simple, and has no errors.
	return errors;
}

public ERR checkExpression(exp:rangeContext(int min, int max), ERR errors, list[SYM] symbols, SYM symbol) {
	if(max < min) {
		errors += <exp@location, "The given range is empty.">;
	}

	return errors;
}

public ERR checkExpression(exp:naturalContext(), ERR errors, list[SYM] symbols, SYM symbol) {
	// This expression is simple, and has no errors.
	return errors;
}

public ERR checkStatement(stat:initialStatement(STATEMENT state), ERR errors, list[SYM] symbols, SYM symbol) {
	if(!isStateSimple(state), b) {
		errors += <stat@location, "The initial state is not allowed to have the in keyword.">;
		return errors;
	}

	return checkStatement(state, errors, symbols, symbol);
}

public ERR checkStatement(stat:stateStatement(str name, EXP exp), ERR errors, list[SYM] symbols, SYM symbol) {
	// Is the expression guarded?
	if(!all(b <- isGuarded(exp), b)) {
		errors += <exp@location, "The expression is unguarded.">;
	}
	
	return checkExpression(exp, errors, symbols, symbol);	
}

public ERR checkStatement(stat:recursiveConstStatement(str name, int const, EXP exp), ERR errors, list[SYM] symbols, SYM symbol) {
	// Is the expression guarded?
	if(!all(b <- isGuarded(exp), b)) {
		errors += <exp@location, "The expression is unguarded.">;
	}
	
	return checkExpression(exp, errors, symbols, symbol);
}

public ERR checkStatement(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context), ERR errors, list[SYM] symbols, SYM symbol) {
	// Is the range check superfluous?
	if(!isRangeRequired(exp)) {
		errors += <context@location, "The range definition is superfluous.">;
	}
	
	// Is the expression guarded?
	if(!all(b <- isGuarded(exp), b)) {
		errors += <exp@location, "The expression is unguarded.">;
	}
	
	// Is the defined context valid? I.e. is min <= max?
	errors = checkExpression(context, errors, symbols, symbol);

	return checkExpression(exp, errors, symbols, symbol);
}

public ERR checkStatement(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context), ERR errors, list[SYM] symbols, SYM symbol) {
	// Is the range check superfluous?
	if(!isRangeRequired(exp)) {
		errors += <context@location, "The range definition is superfluous.">;
	}
	
	// Is the expression guarded?
	if(!all(b <- isGuarded(exp), b)) {
		errors += <exp@location, "The expression is unguarded.">;
	}
	
	// Is the defined context valid? I.e. is min <= max?
	errors = checkExpression(context, errors, symbols, symbol);
	
	return checkExpression(exp, errors, symbols, symbol);
}

public ERR checkStatement(stat:recursiveVarStatement(str name, str var, EXP exp), ERR errors, list[SYM] symbols, SYM symbol) {
	// This case has already been marked as an error beforehand, since the recursion variable is undefined. Return.
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
	<symbols, errors> = getAndValidateSymbols(initialState, states, errors);
	
	// Start checking the states and consequently, the expressions.
	for(state <- initialState + states) {
		// Before calling check statement, we have to find the symbol again for cross verification.
		s = getDeclaredSymbol(state).symbol;
		errors = checkStatement(state, errors, symbols, s);
	}
	
	return <errors, pNames>;
}

/*
 * Since the scope of the symbols is local, we should look for symbols and errors in succession.
 */
public tuple[ERR errors, list[str] names] checkStatement(stat:processCaptionStatement(str name, STATEMENT initialState, list[STATEMENT] states, str caption), ERR errors, list[str] pNames) {
	return checkStatement(processStatement(name, initialState, states), errors, pNames);
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