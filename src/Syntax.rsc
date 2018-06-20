module Syntax

import Prelude;

keyword PAKeywords = "state" | "initial" | "process" | "1" | "0"
 				   | "with" | "in" | "range" | "natural";
				 	 
lexical UpperCaseId = ([A-Z][A-Z0-9]* !>> [A-Z0-9]) \ PAKeywords;
lexical Natural = [0-9]+;
lexical LowerCaseId = [a-z]+ \ PAKeywords;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "//" ![\n]* $
   ;
							 
start syntax Program = program: Statement* body;

syntax Statement = "process" UpperCaseId name "{" InitialStatement initialstate ";" ((StateStatement | RecursiveStatement) ";")* states "}";
syntax StateStatement = "state" UpperCaseId name ":=" Expression exp;
syntax RecursiveStatement = "state" UpperCaseId name "(" (LowerCaseId | Natural) var ")" ":=" Expression exp ("with" RangeExpression context)?;
syntax InitialStatement = "initial" (StateStatement | RecursiveStatement) state;

syntax Expression = state: (UpperCaseId | "1" | "0") name
				  | transition: LowerCaseId name
				  | recState: UpperCaseId name "(" RecExpression ")"
				  | bracket "(" Expression e ")"
				  > left action: Expression lhs "." Expression rhs
				  > left sequential: Expression lhs "*" Expression rhs
				  > left choice: Expression lhs "+" Expression rhs;
				  
syntax RecExpression = id: LowerCaseId name
					 | natCon: Natural natcon
					 | bracket "(" RecExpression e ")"
					 > left ( add: RecExpression lhs "+" RecExpression rhs
					 		| sub: RecExpression lhs "-" RecExpression rhs
					 		);
					 		
syntax RangeExpression = rangeContext: LowerCaseId recVar "in" "range" "(" Natural min "," Natural max ")"
					   | naturalContext: LowerCaseId recVar "in" "natural";
				  
public start[Program] program(str s) = parse(#start[Program], s);
public start[Program] program(str s, loc l) = parse(#start[Program], s, l);