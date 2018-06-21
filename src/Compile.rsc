module Compile

import Prelude;
import Abstract;
import Load;

alias NODE = tuple[str id, bool isRec, int n];
alias GRAPH = tuple[list[NODE] states, list[tuple[NODE from, NODE to, str label]] transitions];


set[NODE] getInitialStates(stat:initialStatement(STATEMENT state), set[NODE] states) {
	return getInitialStates(state, states);
}

set[NODE] getInitialStates(stat:stateStatement(str name, EXP exp), set[NODE] states) {
	return states + <name, false, 0>;
}

set[NODE] getInitialStates(stat:recursiveConstStatement(str name, int const, EXP exp), set[NODE] states) {
	return states + <name, true, const>;
}

set[NODE] getInitialStates(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context), set[NODE] states) {
	// This state will have multiple options, depending on the range.
	tuple[int min, int max] range = getRange(context);
	
	if(min == -1) {
		println("Sadly, natural ranges are not supported. Skipping the variable.");
		return states;
	}
	
	return states + {<name, true, i> | i <- [min .. max + 1]};
}

set[NODE] getInitialStates(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context), set[NODE] states) {
	return states + <name, true, const>;
}

str compileStat(stat:processStatement(str name, STATEMENT initialState, list[STATEMENT] states)) {
	// Maintain a queue of states, holding the states we have not yet visited, and populate it with the initial state.
	unvisitedStates = getInitialStates(initialState, {});
	visitedStates = {};
	
	println(unvisitedStates);

	return 
	"%Process: <name>
	'\\begin{tikzpicture}
	'\\draw [red] (0,0) rectangle (1,1);
	'\\end{tikzpicture}";
}

public str compileProgram(PROGRAM P) {    
  	if(program(list[STATEMENT] statements) := P){    
	
		str result = 
		"
		'\\documentclass[varwidth]{standalone}
		'\\usepackage{tikz}
		'
		'\\begin{document} <for (m <- statements) {> 
		'<compileStat(m)>
		'<}>\\end{document}";
  		
  		println(result);
  		
  		return result;
 	} else
    throw "Cannot happen";
}

public str compileProgram(str txt) = compileProgram(load(txt));