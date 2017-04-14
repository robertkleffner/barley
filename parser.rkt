#lang brag

bly-program : bly-expr

bly-expr : bly-word*
bly-word : INT
         | bly-prim
         | VAR
         | OPNAME
         | bly-block
         | bly-bind
         | bly-let
         | bly-handle

bly-prim : "add" | "not" | "call" | "fix" | "if"
         | "true" | "false" | "eq" | "less" | "resume"
bly-block : "{" bly-expr "}"
bly-bind : "bind" VAR "(" bly-expr ")"
bly-let : "let" VAR "=" bly-expr "(" bly-expr ")"

bly-handle : "handle" bly-expr "with" (bly-op-case)* bly-default-case
bly-op-case : OPNAME "=>" bly-expr ";"
bly-default-case : "otherwise" "=>" bly-expr ";"