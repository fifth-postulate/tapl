module ProjectUntypedLambdaCalculus exposing (main)

import General
import GenericPage
import UntypedLambdaCalculus as Expression exposing (empty)


main =
    GenericPage.create
        { title = "Untyped Lambda Calculus"
        , parse = Expression.parse
        , printer = Expression.toString
        , evaluator = General.eval (Expression.step empty)
        }
