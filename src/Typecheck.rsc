module Typecheck

import Prelude;
import Abstract;
import Load;

import List;

alias ERR = list[tuple[loc l, str msg]];
alias SYM = list[Id];


public tuple[Id Id, loc Loc] getDeclaredSymbol(stat:initialStatement(STATEMENT state)) {
	return getDeclaredSymbol(state);
}

public tuple[Id Id, loc Loc] getDeclaredSymbol(stat:stateStatement(str name, EXP exp)) {
	return <name, stat@location>;
}

public ERR checkExpression(exp:state(str name), ERR errors, SYM symbols) {
	// Rather trivial. Is the used state declared?
	if(name != "1" && name != "0") {
		if(lastIndexOf(symbols, name) == -1) {
			// It is not declared, throw an error.
			errors += <exp@location, "The state \'<name>\' has not been declared.">;
		}
	}
	return errors;
}

public ERR checkExpression(exp:transition(str name), ERR errors, SYM symbols) {
	// Trivially true.
	return errors;
}

public ERR checkExpression(exp:action(EXP left, EXP right), ERR errors, SYM symbols) {
	return checkExpression(left, checkExpression(right, errors, symbols), symbols);
}

public ERR checkExpression(exp:sequential(EXP left, EXP right), ERR errors, SYM symbols) {
	return checkExpression(left, checkExpression(right, errors, symbols), symbols);
}

public ERR checkExpression(exp:choice(EXP left, EXP right), ERR errors, SYM symbols) {
	return checkExpression(left, checkExpression(right, errors, symbols), symbols);
}

public ERR checkStatement(stat:stateStatement(str name, EXP exp), ERR errors, SYM symbols) {
	return checkExpression(exp, errors, symbols);	
}

public ERR checkStatement(stat:initialStatement(STATEMENT state), ERR errors, SYM symbols) {
	return checkStatement(state, errors, symbols);
}

/*
 * Since symbols are only declared in the scope of the process, symbols have to be searched for for each process statement.
 * Since the scope of the symbols is local, we should look for symbols and errors in succession.
 */
public ERR checkStatement(stat:processStatement(str name, STATEMENT initialState, list[STATEMENT] states), ERR errors) {
	// Start by finding all the declared symbols, since we allow state names to be used before declaration.
	symbols = [getDeclaredSymbol(initialState).Id];
	
	for(state <- states) {
		symbol = getDeclaredSymbol(state);
		
		if(lastIndexOf(symbols, symbol.Id) != -1) {
			// State names should be unique.
			errors += <symbol.Loc, "The state names within a process have to be unique (violated by state \'<symbol.Id>\').">;
		} else {
			symbols += symbol.Id;
		}
	}
	
	// Start checking the states and consequently, the expressions.
	errors = checkStatement(initialState, errors, symbols);
	for(state <- states) {
		errors = checkStatement(state, errors, symbols);
	}
	
	return errors;
}

public ERR checkStatements(list[STATEMENT] statements) {
	errors = [];
	
	for(statement <- statements) {
		errors = checkStatement(statement, errors);
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