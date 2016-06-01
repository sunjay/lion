## Expr ##

f x = 22e7 / (3 cm + x cm)

## AST ##

Statement
    NamedFunction
        Symbol(x)
        FunctionParams
            Symbol(x)
        FunctionBody
            Number(22e7)
            Symbol(/)
            Expr
                Number(3)
                Symbol(cm)
                Symbol(+)
                Symbol(x)
                Symbol(cm)

## Expr ##

q = 492 ^ ((\ x = 22e7 / (3 cm + x cm)) 57) * 4

## AST ##

Statement
    Assignment
        Symbol(q)
        Expr
            Number(492)
            Symbol(^)
            Expr
                Expr
                    AnonymousFunction
                        FunctionParams
                            Symbol(x)
                        FunctionBody
                            Number(22e7)
                            Symbol(/)
                            Expr
                                Number(3)
                                Symbol(cm)
                                Symbol(+)
                                Symbol(x)
                                Symbol(cm)
                    Number(57)
            Symbol(*)
            Number(4)

## Expr ##

defineUnit "km/h"

## AST ##

Statement
    Expr
        Symbol(defineUnit)
        SingleWordString("km/h")

