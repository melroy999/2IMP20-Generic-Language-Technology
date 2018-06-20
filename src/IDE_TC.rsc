module IDE_TC

import Prelude;
import util::IDE;
import util::ValueUI;

import vis::Figure;
import vis::Render;

import Syntax;

//  define the language name and extension

private str PA_NAME = "PA";
private str PA_EXT = "pa";

//  Define the connection with the PA parser
Tree parser(str x, loc l) {
    return parse(#Program, x, l);
}

public void registerPA() {
  registerLanguage(PA_NAME, PA_EXT, parser);
  //registerAnnotator(PA_NAME, checkPAProgram);
}