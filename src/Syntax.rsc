module Syntax

import Prelude;

keyword PAKeywords = "state" | "initial" | 
				 	 "process" | "in" | "range";
				 	 
lexical UpperCaseId = ([A-Z][A-Z0-9]* !>> [A-Z0-9]) \ PAKeywords;
lexical Natural = [0-9]+;
lexical LowerCaseId = [a-z]+ \ PAKeywords;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];
lexical WhitespaceAndComment = [\ \t\n\r]
							 | @category="Comment" "//" ![\n]* $;
							 
start syntax Program = program: Statement* body;

syntax Statement = "process" UpperCaseId name "{" InitialStatement initialstate ";" (StateStatement ";")* states "}";
syntax StateStatement = "state" UpperCaseId name ":=" Expression exp;
syntax InitialStatement = "initial" StateStatement state;
// syntax RecursiveStatement = "state" UpperCaseId name "(" ;

syntax Expression = state: UpperCaseId name
				  | transition: LowerCaseId name
				  | bracket "(" Expression e ")"
				  > left action: Expression lhs "." Expression rhs
				  > left sequential: Expression lhs "*" Expression rhs
				  > left choice: Expression lhs "+" Expression rhs;
				  
public start[Program] program(str s) = parse(#start[Program], s);
public start[Program] program(str s, loc l) = parse(#start[Program], s, l);