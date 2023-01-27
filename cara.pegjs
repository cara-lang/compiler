{
  // JS helpers
}

Main
  = shebang:Shebang? EOL* h:Decl? t:(EOL+ d:Decl {return d})* EOL*
    { 
      return {
        type: 'main',
        shebang: !!shebang,
        decls: [h,...t],
      }
    }

Decl
    = stmt:Stmt { return { type: 'declaration', declType: 'statement', stmt } }
    // TODO Function
    // TODO TypeAlias
    // TODO Type
    // TODO Module
    // TODO ExtendModule
    // TODO Block
    // TODO EffectBlock
    // TODO BlockFunction
    // TODO EffectBlockFunction
    // TODO Annotation
    // TODO FunctionAnnotation
    
Stmt
    = bang:Bang                             { return { type: 'statement', stmtType: 'bang',         bang } }
    / id:UnqualifiedLower _ "=" _ bang:Bang { return { type: 'statement', stmtType: 'let-bang', id, bang } }
    / id:UnqualifiedLower _ "=" _ expr:Expr { return { type: 'statement', stmtType: 'let-expr', id, expr } }

Bang = id:Identifier "!" args:Args? { return { type: 'bang', id, args } }

Args = "(" _* h:Expr? t:(_* "," _* e:Expr {return e})* _* ")" { return [h,...t].filter(x=>x) }

Expr
    = n:Float   { return { type: 'expr', exprType: 'float', float: n } }
    / n:Int     { return { type: 'expr', exprType: 'int',   int: n } }
    / "(" _ ")" { return { type: 'expr', exprType: 'unit' } }
    // TODO Char
    // TODO String
    // TODO Bool
    // TODO Tuple
    // TODO List
    // TODO Record
    // TODO Constructor
    // TODO UnaryOp
    // TODO BinaryOp
    // TODO Call
    // TODO RecordGet
    // TODO Pipeline
    // TODO Identifier
    // TODO Lambda
    // TODO RecordGetter
    // TODO If

Int
    = neg:'-'?
      n:( ("0x" h:[0-9a-f]i t:[0-9a-f_]i* { return parseInt(h + t.filter(c => c != '_').join(''), 16) })
        / ("0b" h:[01]      t:[01_]*      { return parseInt(h + t.filter(c => c != '_').join(''), 2)  })
        / ("0o" h:[0-7]     t:[0-7_]*     { return parseInt(h + t.filter(c => c != '_').join(''), 8)  })
        / (     h:[0-9]     t:[0-9_]*     { return parseInt(h + t.filter(c => c != '_').join(''), 10) })
        )
      { return (!!neg ? -n : n) }
      
BoringInt
    = neg:'-'?
      n:(x:[0-9]+ { return parseInt(x, 10) })
      { return (!!neg ? -n : n) }

Float
    = neg:'-'?
      n:(ih:[0-9] it:[0-9_]*
         '.'
         dh:[0-9] dt:[0-9_]*
         e:([eE] x:BoringInt { return x })?
         { return parseFloat(`${ih}${it.join('')}.${dh}${dt.join('')}e${e}`.replace(/_/g,'')) }
        )
      { return (!!neg ? -n : n) }

Identifier = QualifiedIdentifier / UnqualifiedIdentifier
UnqualifiedIdentifier =                       name:Name      { return { type: 'identifier', qualifiers: [], name } }
QualifiedIdentifier   = qualifiers:Qualifier+ name:Name      { return { type: 'identifier', qualifiers,     name } }
UnqualifiedLower      =                       name:LowerName { return { type: 'identifier', qualifiers: [], name } }

Qualifier = name:UpperName "." { return name }

Name = UpperName / LowerName
UpperName = h:[A-Z] t:[a-zA-Z0-9_]*  { return h + t.join('') }
LowerName = h:[a-z] t:[a-zA-Z0-9_']* { return h + t.join('') }

EOLSequence = ("\n" / "\r\n" / "\r") { return null }
EOL         = __* EOLSequence        { return null }
WS "whitespace" = [ \t]              { return null }
Shebang         = "#!" (!EOL .)*     { return true } // truthiness is used in Main
LineComment     = "//" (!EOL .)*     { return null }
_  = WS
__ = WS / LineComment
