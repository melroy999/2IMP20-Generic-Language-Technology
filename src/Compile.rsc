module Compile

import Prelude;
import Abstract;
import Typecheck;
import Load;

alias NODE = tuple[str id, bool isRec, int n];
alias EDGE = tuple[NODE from, NODE to, str label];
alias GRAPH = tuple[set[NODE] nodes, set[EDGE] edges];
alias DATA = tuple[set[NODE] unvisited, set[NODE] visited, set[NODE] finalStates, set[EDGE] edges];

// =========================================================================================
//
// Find initial states to start the exploration from.
//
// =========================================================================================

set[NODE] getInitialStates(stat:initialStatement(STATEMENT state), set[NODE] states) {
	return getInitialStates(state, states);
}

set[NODE] getInitialStates(stat:stateStatement(str name, EXP exp), set[NODE] states) {
	return states + <name, false, 0>;
}

set[NODE] getInitialStates(stat:recursiveConstStatement(str name, int const, EXP exp), set[NODE] states) {
	return states + <name, true, const>;
}

// =========================================================================================
//
// Building blocks for different types of states and edges.
//
// =========================================================================================

str drawState(NODE n, map[str, int] indents) {
	return "\\node[draw, ellipse] at (<indents[n.id]>, <3 * n.n>) (<n.id><n.n>) {$<n.isRec ? "<n.id>_<n.n>" : "<n.id>">$};";
}

str drawEdge(EDGE e, map[str, int] indents) {
	// If we go to the same state, create a self-loop.
	if(e.from == e.to) {
		return "\\draw[-\>] (<e.from.id><e.from.n>) edge[loop above] node[midway, above] {<e.label>} (<e.to.id><e.to.n>);";
	}
	
	// Place the label taking the direction of the edge in mind.
	str placement = "midway";
	
	if(indents[e.from.id] != indents[e.to.id]) {
		placement += "," + (indents[e.from.id] < indents[e.to.id] ? "above" : "below");
	}
	
	if(e.from.n != e.to.n) {
		placement += "," + (e.from.n < e.to.n ? "left" : "right");
	}

	return "\\draw[-\>] (<e.from.id><e.from.n>) -- (<e.to.id><e.to.n>) node[<placement>] {<e.label>};";
}

str drawInitialEdge(NODE n, map[str, int] indents) {
	return "\\draw[-\>] (<indents[n.id] - 1>, <3 * n.n - 1>) -- (<n.id><n.n>) node[midway] {};";
}

str drawFinalEdge(NODE n, map[str, int] indents) {
	return "\\draw[-\>] (<n.id><n.n>) -- (<indents[n.id] + 1>, <3 * n.n - 1>) node[midway] {};";
}


// =========================================================================================
//
// Check whether the given state is a final state. I.e. is there an unguarded 1.
//
// =========================================================================================

public list[bool] hasUnguardedFS(exp:state(str name)) {
	// Note that 1 and 0 are always guarded.
	return name == "1" ? [true] : [false];
}

public list[bool] hasUnguardedFS(exp:recursion(str name, EXP var)) {
	return [false];
}

public list[bool] hasUnguardedFS(exp:action(str label, EXP var)) {
	return [false | b <- hasUnguardedFS(var)];
}

public list[bool] hasUnguardedFS(exp:choice(EXP left, EXP right)) {
	return hasUnguardedFS(left) + hasUnguardedFS(right);
}

// =========================================================================================
//
// Explore edges and nodes in the graph.
//
// =========================================================================================

GRAPH getGraphStructure(exp:state(str name), int n, NODE origin) {
	set[EDGE] edges = {};
	set[NODE] nodes = {<name, false, 0>};
	return <nodes, edges>;
}

GRAPH getGraphStructure(exp:recursion(str name, EXP var), int n, NODE origin) {
	// Get the offset associated with the recursive variable.
	<varId, offset> = getRecursionVariable(var);
	set[EDGE] edges = {};
	
	try {
		set[NODE] nodes = {<name, true, toInt(varId)>};
		return <nodes, edges>;
	} catch IllegalArgument: {
		// It is a string, use offset.
		set[NODE] nodes = {<name, true, n + offset>};
		return <nodes, edges>;
	}
}

GRAPH getGraphStructure(exp:action(str label, EXP right), int n, NODE origin) {
	// We have to be careful with consecutive edges, as we want it to represent one transition.
	GRAPH result = getGraphStructure(right, n, origin);
	
	// If the result already has edges, append the current label to all the transitions in the result, and return.
	// Otherwise, define new edges to all of the states given in the result.
	if(isEmpty(result.edges)) {
		return <result.nodes, {<origin, v, label> | v <- result.nodes}>;
	} else {
		return <result.nodes, {<origin, v, label + "." + l> | <_, v, l> <- result.edges}>;
	}
}

GRAPH getGraphStructure(exp:sequential(EXP left, EXP right), int n, NODE origin) {
	// TODO, should be close to reversing the order of the edge, but not sure yet. 
}

GRAPH getGraphStructure(exp:choice(EXP left, EXP right), int n, NODE origin) {
	// Simply merge the choices of the two options.
	GRAPH result1 = getGraphStructure(left, n, origin);
	GRAPH result2 = getGraphStructure(right, n, origin);
	
	return <result1.nodes + result2.nodes, result1.edges + result2.edges>;
}

DATA getGraphStructure(stat:initialStatement(STATEMENT state), DATA g, NODE target) {
	return getGraphStructure(state, g, target);
}

DATA getGraphStructure(stat:stateStatement(str name, EXP exp), DATA g, NODE target) {
	if(name == target.id) {
		// The statement matches the proposed variable, explore!
		GRAPH result = getGraphStructure(exp, 0, target);
		
		// Which states are new?
		set[NODE] discovered = result.nodes - g.visited;
		set[NODE] finalStates = g.finalStates + (any(b <- hasUnguardedFS(exp), b) ? target : {});
		g = <g.unvisited + discovered, g.visited, finalStates, g.edges + result.edges>;
	}

	return g;
}

DATA getGraphStructure(stat:recursiveConstStatement(str name, int const, EXP exp), DATA g, NODE target) {
	if(name == target.id && const == target.n) {
		// The statement matches the proposed variable, explore!
		GRAPH result = getGraphStructure(exp, const, target);
		
		// Which states are new?
		set[NODE] discovered = result.nodes - g.visited;
		set[NODE] finalStates = g.finalStates + (any(b <- hasUnguardedFS(exp), b) ? target : {});
		g = <g.unvisited + discovered, g.visited, finalStates, g.edges + result.edges>;
	}
	
	return g;
}

DATA getGraphStructure(stat:contRecursiveVarStatement(str name, str var, EXP exp, EXP context), DATA g, NODE target) {
	// Is the given variable in range?
	tuple[int min, int max] range = getRange(context);
	
	if(min == -1) {
		println("Sadly, natural ranges are not supported. Skipping the variable.");
		return states;
	}
	
	if(name == target.id && range.min <= target.n && target.n <= range.max) {
		// The statement matches the proposed variable, explore!
		GRAPH result = getGraphStructure(exp, target.n, target);
		
		// Which states are new?
		set[NODE] discovered = result.nodes - g.visited;
		set[NODE] finalStates = g.finalStates + (any(b <- hasUnguardedFS(exp), b) ? target : {});
		g = <g.unvisited + discovered, g.visited, finalStates, g.edges + result.edges>;
	}
	
	return g;
}

DATA getGraphStructure(stat:contRecursiveConstStatement(str name, int const, EXP exp, EXP context), DATA g, NODE target) {
	// This state will have multiple options, depending on the range.
	tuple[int min, int max] range = getRange(context);
	
	if(min == -1) {
		println("Sadly, natural ranges are not supported. Skipping the variable.");
		return states;
	}
	
	if(name == target.id && const == target.n) {
		for(i <- [min .. max + 1]) {
			// The statement matches the proposed variable, explore!
			GRAPH result = getGraphStructure(exp, i, target);
			
			// Which states are new?
			set[NODE] discovered = result.nodes - g.visited;
		set[NODE] finalStates = g.finalStates + (any(b <- hasUnguardedFS(exp), b) ? target : {});
			g = <g.unvisited + discovered, g.visited, finalStates, g.edges + result.edges>;
		}
	}
	
	return g;
}


str compileStat(stat:processStatement(str name, STATEMENT initialState, list[STATEMENT] states)) {
	// Maintain a queue of states, holding the states we have not yet visited, and populate it with the initial state.
	initialStates = getInitialStates(initialState, {});
	unvisitedStates = {} + initialStates;
	visitedStates = {};
	finalStates = {};
	
	// Now, find new states, as long as we find states that have not yet been visited.
	// Also keep track of transitions that are found.
	edges = {};
	while(!isEmpty(unvisitedStates)) {
		// Extract a random state.
		<target, unvisitedStates> = takeOneFrom(unvisitedStates);
		visitedStates += target;
	
		// Look for unvisited states, and transitions that lead there.
		for(state <- initialState + states) {
			// Check for new states and transitions.
			<unvisitedStates, visitedStates, finalStates, edges> = getGraphStructure(state, <unvisitedStates, visitedStates, finalStates, edges>, target);
		}
	}
	
	// If there is no edge to the state 0, the state 0 should be eliminated.
	if(!any(e <- edges, e.to.id == "0")) {
		visitedStates -= <"0", false, 0>;
	}
	
	// If there is no edge to the state 1, the state 1 should be eliminated.
	if(!any(e <- edges, e.to.id == "1")) {
		visitedStates -= <"1", false, 0>;
	}
	
	// Get some "theoretical" positions for the states.
	set[str] baseStates = {s.id | s <- visitedStates, s.id != "0" && s.id != "1"};
	
	// Sort the states on alphabetical order, with 0 and 1 having a minimum and maximum override.
	list[str] baseStatesList = (any(s <- visitedStates, s.id == "0") ? ["0"] : []) + sort(baseStates) + (any(s <- visitedStates, s.id == "1") ? ["1"] : []);
	
	if(any(s <- visitedStates, s.id == "1")) {
		finalStates += <"1", false, 0>;
	}
	
	visitedStatesList = sort(visitedStates); 
	edgesList = sort(edges); 
	
	map[str, int] indents = ();
	i = 0;
	for(s <- baseStatesList) {
		indents[s] = i;
		i += 3;
	}
	
	return 
	"%Process: <name>
	'\\begin{tikzpicture}[baseline, y=-0.7cm]
	'
	'    % Declare the states<for(s <- visitedStatesList) {>
	'    <drawState(s, indents)><}>
	'
	'    % Declare edges<for(e <- edgesList) {>
	'    <drawEdge(e, indents)><}>
	'
	'    % Declare initial edges<for(e <- initialStates) {>
	'    <drawInitialEdge(e, indents)><}>
	'
	'    % Declare final edges<for(e <- finalStates) {>
	'    <drawFinalEdge(e, indents)><}>
	'
	'\\end{tikzpicture}";
}


str compileStat(stat:processCaptionStatement(str name, STATEMENT initialState, list[STATEMENT] states, str caption)) {
	// Clean up the caption.
	caption = replaceAll(caption, "\"", "");

	return
	"\\begin{figure}
	'    \\centering
	'    <compileStat(processStatement(name, initialState, states))>
	'    \\caption{<caption>}
	'    \\label{fig:<toLowerCase(name)>}
	'\\end{figure}";
}




public str compileProgram(PROGRAM P) {    
  	if(program(list[STATEMENT] statements) := P){    
	
		str result = 
		"
		'\\documentclass[varwidth,border=20pt]{standalone}
		'\\usepackage{tikz}
		'\\usetikzlibrary{arrows.meta}
		'\\usetikzlibrary{shapes}
		'\\usetikzlibrary{backgrounds,shadows}
		'
		'\\begin{document} <for (m <- statements) {> 
		'<compileStat(m)>
		'\\vspace{2em}
		'<}>\\end{document}";
  		
  		println(result);
  		
  		return result;
 	} else
    throw "Cannot happen";
}

public str compileProgram(str txt) = compileProgram(load(txt));