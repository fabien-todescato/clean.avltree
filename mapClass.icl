// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : mapClass
// Purpose : Overloaded dynamic map operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------

implementation module mapClass

import orderClass

class mapInsertWith     k :: ( Order2 x )                ( x , y ) ( k ( x , y ) ) -> k ( x , y )
class mapFindWith       k :: ( Order2 x ) ( ( x , y ) -> z ) z x ( k ( x , y ) ) -> z
class mapFindInsertWith k :: ( Order2 x ) ( ( x , y ) -> z ) ( ( k ( x , y ) ) -> z ) ( x , y ) ( k ( x , y ) ) -> z
class mapEraseWith      k :: ( Order2 x )                  x       ( k ( x , y ) ) -> k ( x , y )
class mapUpdateWith     k :: ( Order2 x ) ( z y -> y ) y ( x , z ) ( k ( x , y ) ) -> k ( x , y )
class mapMap            k ::              (   y -> z )             ( k ( x , y ) ) -> k ( x , z )

mapInsert :: ( ( x , y ) -> ( ( k ( x , y ) ) -> ( k ( x , y ) ) ) ) | mapInsertWith k & order x
mapInsert = mapInsertWith order

mapFind :: (((x,y) -> z) z x ( k (x,y) ) -> z) | mapFindWith k & order x
mapFind = mapFindWith order

mapFindInsert :: (((x,y) -> z) -> (((k (x,y)) -> z) -> ((x,y) -> ((k (x,y)) -> z)))) | mapFindInsertWith k & order x
mapFindInsert = mapFindInsertWith order

mapErase :: ( x ( k ( x , y ) ) -> k ( x , y ) ) | mapEraseWith k & order x
mapErase = mapEraseWith order

mapUpdate :: ( ( z y -> y ) y ( x , z ) ( k ( x , y ) ) -> k ( x , y ) ) | mapUpdateWith k & order x
mapUpdate = mapUpdateWith order

