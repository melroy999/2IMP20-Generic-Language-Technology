module Syntax

import Prelude;

keyword PAKeywords = "state" | "initial" | "process" | "1" | "0";
				 	 
lexical UpperCaseId = ([A-Z][A-Z0-9]* !>> [A-Z0-9]) \ PAKeywords;
lexical Natural = [0-9]+;
lexical LowerCaseId = [a-z]+ \ PAKeywords;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "/*" (![*] | [*] !>> [/])* "*/" 
   | @category="Comment" "//" ![\n]* $
   ;
							 
start syntax Program = program: ProcessStatement* body;

syntax ProcessStatement = processStatement: "process" UpperCaseId name "{" InitialStateStatement initialstate ";" (StateStatement ";")* states "}";
syntax StateStatement = stateStatement: "state" UpperCaseId name ":=" Expression exp;
syntax InitialStateStatement = initialStatement: "initial" StateStatement state;

syntax Expression = state: (UpperCaseId | "1" | "0") name
				  | transition: LowerCaseId name
				  | bracket "(" Expression e ")"
				  > left action: Expression lhs "." Expression rhs
				  > left sequential: Expression lhs "*" Expression rhs
				  > left choice: Expression lhs "+" Expression rhs;
				  
public start[Program] program(str s) = parse(#start[Program], s);
public start[Program] program(str s, loc l) = parse(#start[Program], s, l);