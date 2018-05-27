module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[ map[PicoId, TYPE] symbols, list[tuple[loc l, str msg]] errors]; 

TENV addError(TENV env, loc l, str msg) = env[errors = env.errors + <l, msg>];      

str required(TYPE t, str got) = "Required <getName(t)>, got <got>";                 
str required(TYPE t1, TYPE t2) = required(t1, getName(t2));

// compile Expressions.

// For each of the expressions, we define functions that will return the expected return type.
// This extension of the code is required, such that we can match the types on the rhs and lhs of the equals and nequals operator.
// By using these methods in the expression checks, instead of the hardcoded types, we eliminate some redundency.
// Here, we start with the original expressions, which all retain their original type.


// TODO improve exp:id, as it errors out on invalid ids.
TYPE getType(exp:id(PicoId Id), TENV env) = env.symbols[Id];
TYPE getType(exp:natCon(int N), TENV env) = natural();
TYPE getType(exp:strCon(str S), TENV env) = string();
TYPE getType(exp:add(EXP E1, EXP E2), TENV env) = natural();
TYPE getType(exp:sub(EXP E1, EXP E2), TENV env) = natural();
TYPE getType(exp:conc(EXP E1, EXP E2), TENV env) = string();

// The new expressions introduced in assignment 1, which all return boolean values.
TYPE getType(exp:boolCon(bool B), TENV env) = boolean();
TYPE getType(exp:not(EXP E), TENV env) = boolean();
TYPE getType(exp:and(EXP E1, EXP E2), TENV env) = boolean();
TYPE getType(exp:or(EXP E1, EXP E2), TENV env) = boolean();
TYPE getType(exp:equals(EXP E1, EXP E2), TENV env) = boolean();
TYPE getType(exp:nequals(EXP E1, EXP E2), TENV env) = boolean();


// For all of the checkExp methods, we have replaced the natural(), string() or boolean() with the getType(exp, env) method above.
// This way, we don't accidently mismatch the type stored here and in the getType(), they will always match as the source is the same.
// Here, we start with the originally defined expressions, which all retain the original functionality.
TENV checkExp(exp:natCon(int N), TYPE req, TENV env) =                              
  req == getType(exp, env) ? env : addError(env, exp@location, required(req, getType(exp, env)));

TENV checkExp(exp:strCon(str S), TYPE req, TENV env) =
 req == getType(exp, env) ? env : addError(env, exp@location, required(req, getType(exp, env)));
 
TENV checkExp(exp:id(PicoId Id), TYPE req, TENV env) {                              
  if(!env.symbols[Id]?)
     return addError(env, exp@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return req == tpid ? env : addError(env, exp@location, required(req, tpid));
}

TENV checkExp(exp:add(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == getType(exp, env) ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, getType(exp, env)));
  
TENV checkExp(exp:sub(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == getType(exp, env) ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, getType(exp, env)));

TENV checkExp(exp:conc(EXP E1, EXP E2), TYPE req, TENV env) =                    
  req == getType(exp, env) ? checkExp(E1, string(), checkExp(E2, string(), env))
                   : addError(env, exp@location, required(req, getType(exp, env)));

// Here, we continue with defining checks for the new expressions, namely boolcon, not, and, or, equals and not equals.
// Note that all of these checks validate whether the return type of the expression matches the expected type.
TENV checkExp(exp:boolCon(bool B), TYPE req, TENV env) =
 req == getType(exp, env) ? env : addError(env, exp@location, required(req, getType(exp, env)));
 
// Added a check for the not operator, which validates whether the input is a boolean value.
TENV checkExp(exp:not(EXP E), TYPE req, TENV env) =                        
 req == getType(exp, env) ? checkExp(E, boolean(), env)
  				   : addError(env, exp@location, required(req, getType(exp, env)));
  				   
// Added a check for the and operator.
TENV checkExp(exp:and(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == getType(exp, env) ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, getType(exp, env)));
  
// Added a check for the or operator.
TENV checkExp(exp:or(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == getType(exp, env) ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, getType(exp, env)));

// Added a check for the equals operator.
// Note here that we consider the left-hand side of the equation to be the desired type, which is used to validate the type of the right-hand side.
// In other words, the left-hand side of the equation will never create an invalid-type error, given that the left-hand-side expression is correct.
// Note that it may occur that the left-hand side is an undefined id, which should be handled appropriately through an error.
TENV checkExp(exp:equals(EXP E1, EXP E2), TYPE req, TENV env) {
  try {
	return req == getType(exp, env) ? checkExp(E1, getType(E1, env), checkExp(E2, getType(E1, env), env))
	                   : addError(env, exp@location, required(req, getType(exp, env)));
  } catch NoSuchKey(value key): {
  	// The left-hand side of the equation is an undefined variable. The type of the expression cannot be established.
  	env2 = addError(env, E1@location, "Undeclared variable <key>");
  	
  	// Can we resolve the type of the right-hand side?
  	try {
  	  // If we can, add the errors we find on the right-hand side.
  	  return checkExp(E2, getType(E2, env2), env2);
  	} catch NoSuchKey(value key2): {
  	  // We failed to find the type of the right-hand side, mark both sides as undeclared.
  	  return addError(env2, E2@location, "Undeclared variable <key2>");
  	}
  }
}
  
// Added a check for the equals operator.
// Note here that we consider the left-hand side of the equation to be the desired type, which is used to validate the type of the right-hand side.
// In other words, the left-hand side of the equation will never create an invalid-type error, given that the left-hand-side expression is correct.
// Note that it may occur that the left-hand side is an undefined id, which should be handled appropriately through an error.
TENV checkExp(exp:nequals(EXP E1, EXP E2), TYPE req, TENV env) {
  try {
	return req == getType(exp, env) ? checkExp(E1, getType(E1, env), checkExp(E2, getType(E1, env), env))
	                   : addError(env, exp@location, required(req, getType(exp, env)));
  } catch NoSuchKey(value key): {
  	// The left-hand side of the equation is an undefined variable. The type of the expression cannot be established.
  	env2 = addError(env, E1@location, "Undeclared variable <key>");
  	
  	// Can we resolve the type of the right-hand side?
  	try {
  	  // If we can, add the errors we find on the right-hand side.
  	  return checkExp(E2, getType(E2, env2), env2);
  	} catch NoSuchKey(value key2): {
  	  // We failed to find the type of the right-hand side, mark both sides as undeclared.
  	  return addError(env2, E2@location, "Undeclared variable <key2>");
  	}
  }
}

// check a statement

TENV checkStat(stat:asgStat(PicoId Id, EXP Exp), TENV env) {                        
  if(!env.symbols[Id]?)
     return addError(env, stat@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return checkExp(Exp, tpid, env);
}

// Changed the if statement to require a boolean expression, instead of a string.
TENV checkStat(stat:ifElseStat(EXP Exp, list[STATEMENT] Stats1, list[STATEMENT] Stats2), TENV env){
    env0 = checkExp(Exp, boolean(), env);
    env1 = checkStats(Stats1, env0);
    env2 = checkStats(Stats2, env1);
    return env2;
}

// Changed the while statement to require a boolean expression, instead of a string.
TENV checkStat(stat:whileStat(EXP Exp, list[STATEMENT] Stats1), TENV env) {
    env0 = checkExp(Exp, boolean(), env);
    env1 = checkStats(Stats1, env0);
    return env1;
}

// Added a check for the for statement.
TENV checkStat(stat:forStat(STATEMENT init, EXP cond, STATEMENT inc, list[STATEMENT] Stats1), TENV env) {
    env0 = checkExp(cond, boolean(), env);
    env1 = checkStat(init, env0);
    env2 = checkStat(inc, env1);
    env3 = checkStats(Stats1, env2);
    return env3;
}

// check a list of statements
TENV checkStats(list[STATEMENT] Stats1, TENV env) {                                 
  for(S <- Stats1){
      env = checkStat(S, env);
  }
  return env;
}
  
// check declarations

TENV checkDecls(list[DECL] Decls) =                                                 
    <( Id : tp  | decl(PicoId Id, TYPE tp) <- Decls), []>;

// check a Pico program

public TENV checkProgram(PROGRAM P){                                                
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
     TENV env = checkDecls(Decls);
     return checkStats(Series, env);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt)).errors;