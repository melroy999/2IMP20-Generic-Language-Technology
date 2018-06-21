module Compile

import Prelude;
import Abstract;
import Load;

str compileStat(stat:processStatement(str name, STATEMENT initialState, list[STATEMENT] states)) {
	
}

public str compileProgram(PROGRAM P) {    
  	if(program(list[STATEMENT] statements) := P){    
	
		str result = 
		"
		'\\documentclass{standalone}
		'\\usepackage{tikz}
		'
		'\\begin{document} <for (m <- statements) {> 
		'	\\begin{tikzpicture}
		'	\\draw [red] (0,0) rectangle (1,1);
		'	\\end{tikzpicture}
		'<}>\\end{document}";
  		
  		println(result);
  		
  		return result;
 	} else
    throw "Cannot happen";
}

public str compileProgram(str txt) = compileProgram(load(txt));