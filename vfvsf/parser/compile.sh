ocamllex lexer.mll       # generates lexer.ml
        ocamlyacc parser.mly     # generates parser.ml and parser.mli
        ocamlc -c parser.mli
        ocamlc -c lexer.ml
        ocamlc -c parser.ml
        ocamlc -c run.ml
        ocamlc -o runEXE lexer.cmo parser.cmo run.cmo
