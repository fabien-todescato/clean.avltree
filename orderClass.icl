// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : orderClass
// Purpose : Overloaded strict total order operator.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------

implementation module orderClass

from StdEnv import <

import sequenceClass
import StdArray
import StdEnum

:: Order        = LT | EQ | GT
:: Order1 x :== ! x     -> Order
:: Order2 x :== ! x ! x -> Order

class order x :: Order2 x

? :: ! Order e e e -> e
? o l e g = case o of LT -> l ; EQ -> e ; GT -> g ;

(:>) infixr 1 :: ! Order Order -> Order
(:>) a b = ? a LT b GT

lower :: ! x ! x -> Bool | order x
lower x y = ? ( order x y ) True False False

equal :: ! x ! x -> Bool | order x
equal x y = ? ( order x y ) False True False

greater :: ! x ! x -> Bool | order x
greater x y = ? ( order x y ) False False True

fstOrder :: Order2 ( x , y ) | order x
fstOrder = o where o ( x1 , _ ) ( x2 , _ ) = order x1 x2

sndOrder :: Order2 ( x , y ) | order y
sndOrder = o where o ( _ , y1 ) ( _ , y2 ) = order y1 y2

instance order Int  where order = order_
instance order Char where order = order_
instance order Real where order = order_
instance order Bool where order = orderBool

instance order [  x ]        | order x where order = orderL
instance order {  x }        | order , uselect_u , usize_u x where order = orderA
instance order {! x }        | order , uselect_u , usize_u x where order = orderA
instance order {# x }        | order , uselect_u , usize_u x where order = orderA
instance order ( x , y )     | order x & order y where order = order2
instance order ( x , y , z ) | order x & order y & order z where order = order3

order_ x y | x < y = LT
           | y < x = GT
                   = EQ

orderBool False False = EQ
orderBool False True  = LT
orderBool True  False = GT
orderBool True  True  = EQ

orderL [ x : xl ] [ y : yl ] = order x y :> orderL xl yl
orderL [        ] [ _ : _  ] = LT
orderL [ _ : _  ] [        ] = GT
orderL [        ] [        ] = EQ

orderA a b = orderL [ x \\ x <-: a ] [ y \\ y <-: b ]

order2 ( x1 , y1 ) ( x2 , y2 ) = order x1 x2 :> order y1 y2
order3 ( x1 , y1 , z1 ) ( x2 , y2 , z2 ) = order x1 x2 :> order y1 y2 :> order z1 z2


//instance order ( k x ) | forward k & order x where order = forwardOrder

forwardOrder :: ( k x ) ( k x ) -> Order | forward k & order x
forwardOrder k1 k2 = order ( forward k1 ) ( forward k2 )

backwardOrder :: ( k x ) ( k x ) -> Order | backward k & order x
backwardOrder k1 k2 = order ( backward k1 ) ( backward k2 )
