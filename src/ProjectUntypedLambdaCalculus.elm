module ProjectUntypedLambdaCalculus exposing (main)

import GenericPage
import UntypedLambdaCalculus as Expression


main =
    GenericPage.create
        { title = "Untyped Lambda Calculus"
        , parse = Expression.parse
        , printer = Expression.toString
        , evaluator = Expression.eval
        }
