module Syntax

import Prelude;

// The and, or and negation operator keywords are the rather verbose "and", "or" and "not", with the following reasons:
// 	- In general, the pico imperical language seems to be rather verbose; similarly to the keywords "and", "or" and "not".
// 	- In many other languages, and, or and not are represented by "&&", "||" and "!" respectively.
// 		However, "||" is already in use by the string concatenation, which does not display similar behavior to the or operation.
// 		Thus, to maintain syntactical clearity and non-conflictness, "||" should not be used for two purposes simultaniously.
// 		Since "&&", "!" are in the same syntactical family, they should not be used either.
// 	- The representations "&" and "|" are often used in bit operations; giving an unnatural feel when used in boolean expressions.
// 
// Next to that, we represent boolean values as the classical keywords "true" and "false", as used in many other languages.
// Obviously, these two keywords and the boolean operations should be included in the keyword collection.
keyword PicoKeywords = "begin" | "end" | 
                       "declare" | 
                       "if" | "then" | "else" | "fi" | 
                       "while" | "for" | "do" | "od" | 
                       "and" | "or" | "not" |
                       "true" | "false"
                       ;                       

// Lexical representations of tokens in the language.
// Here, we would like to differentiate Ids used in the left-hand side and right-hand side:
// 	- The Ids used in the left-hand side may not contain any of the keywords, to avoid confusions.
// 	- The Ids used in the right-hand side should not contain any structural keywords, value keywords are obviously allowed.
//
// As mentioned before, the Boolean lexical representation are the keywords "true" and "false".
lexical Id = ([a-z][a-z0-9]* !>> [a-z0-9]) \ PicoKeywords;
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";
lexical Boolean = "true" | "false";



layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"
   | @category="Comment" "%%" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

// The boolean type is represented by the keyword "boolean".
syntax Type 
   = natural:"natural" 
   | string :"string" 
   | boolean:"boolean"
   ;

// TODO define the for statement.
// Note that the assignment states that the purpose of the loop is to perform a specific action a number of times. 
// This can be realised in my ways, and has been realised in many ways in other languages.
//
// In most languages, for loops consist of the following parts:
// 	- Setup statement (<init>): declaring and/or initializing the loop variable;
// 	- Termination condition expression (<cond>): a guard expression that determines when the for loop should terminate;
//  - Incrementation statement (<inc>): an expression that increments the loop variable by the desired amount;
//  - Body statement (<body>): the code executed within the for loop.
//
// In general, each for loop can be represented by a while loop. This can be done easily with the definitions above.
// As an illustration of our claim, we have the following code snippet example, using PICOs syntax:
// 1. <init>
// 2. while <cond> do
// 3. 	<body>
// 4. 	<inc>
// 5. od
//
// Thus, by providing the setup, termination condition and incrementation statements/expressions, the for loop can be easy translated to a while loop.
// The only remaining choice is the syntax itself of the for loop. 
// Similarly to the while loop, we will start the body of the loop with "do", and end it with "od".
// In most other languages, loops with an incrementation component are represented by a semicolon separated tuple, which we will adhere to too.
// 
// Note that we have split the assignment statement and other statements.
// We noticed that by the definition of the for loop, we could nest if, while and for constructs as init and inc values for the for loop.
// We are not sure whether this is syntactically incorrect or a semantical problem.
// So to be sure, we have solved this issue by being more specific with our definition of statements.
syntax AsgStatement = asgStat: Id var ":=" Expression val;

syntax Statement 
   = AsgStatement 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   | forStat: "for" AsgStatement init ";" Expression cond ";" AsgStatement inc "do" {Statement ";"}* body "od"
  ;  
     
// Note that we have added an additional boolean expression "boolCon", introducing the "true" and "false" keywords to expressions.
//
// In general, natural number binary operations have precedence over logical binary operations, which we will adhere to in our definition. 
// Note that the negation operator, being an unary operator, is excluded in the above observation.
// Instead, the negation operation usually has high precedence, in fact the same precedence level as brackets.
//
// The assignment subsection for the logical expressions does not state that "==" and "!=" have to be defined.
// However, since the introduction does, we have added them anyhow. 
// The operators "==" and "!=" take precedence over the "and" and "or" operators, as seen in most if not all programming languages. 
// Note that they should be at the same level of precedence, since "==" and "!=" have similar functionality/behavior.
// Operator "and" has precedence over "or", since this is standard in boolean logic.
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | boolCon: Boolean boolean
   | bracket "(" Expression e ")"
   | not: "not" Expression e
   > left conc: Expression lhs "||" Expression rhs
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
   > left ( equals: Expression lhs "==" Expression rhs
		  | nequals: Expression lhs "!=" Expression rhs
		  )
   > left and: Expression lhs "and" Expression rhs
   > left or: Expression lhs "or" Expression rhs
  ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}