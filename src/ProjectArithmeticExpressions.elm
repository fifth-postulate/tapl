module ProjectArithmeticExpressions exposing (main)

import ArithmeticExpressions as Expression
import GenericPage


main =
    GenericPage.create
        { title = "Arithmetic Expression"
        , parse = Expression.parse
        , printer = Expression.toString
        , evaluator = Expression.eval
        }
