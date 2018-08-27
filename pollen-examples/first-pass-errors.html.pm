#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{A document full of errors}
◊define-meta[author]{Konrad Hinsen}


◊+context{syntax-errors}
◊section{Syntax errors}

An illegal sort name in ◊+sort{=}.

An illegal op name in ◊+op{= : foo}.

A missing parenthesis in ◊+op{bar(baz : foo}

A missing second operand in ◊+term{foo +}

An incomplete condition in ◊+rule{true ⇒ false if }

A missing result value in ◊+test{true ⊻ true ⇒}


◊+context{declaration-errors}
◊section{Misplaced context declaration}

◊b{Attempt to define ◊+context{foo} inside a style element.}

◊section{Errors in asset declarations}

Equation without label (using ◊+sort{foo} and ◊+op{a-foo : foo}) :
  ◊+equation{a-foo = a-foo}
