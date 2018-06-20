module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[list[Id] symbols, list[tuple[loc l, str msg]] errors];







// check a list of statements
TENV checkStats(list[PROCESS_STATEMENT] statements, TENV env) {                                 
  for(s <- statements){
      env = checkStatement(s, env);
  }
  return env;
}

public list[Id] getSymbols(list[Id] symbols) {
	return symbols;
}

public TENV checkProgram(PROGRAM P){                                                
  if(program(list[PROCESS_STATEMENT] statements) := P){
  // TODO find declared symbols.
  	env = <getSymbols([]), []>;
  
    return checkStats(statements, env);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt)).errors;