// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : setClass
// Purpose : Overloaded dynamic set operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------

implementation module setClass

import orderClass

class setInsertWith k ::              ( Order2 x ) x ( k x ) -> k x
class setFindWith   k :: ( x -> z ) z ( Order1 x )   ( k x ) -> z
class setEraseWith  k ::              ( Order1 x )   ( k x ) -> k x

setInsert :: x ( k x ) -> k x | setInsertWith k & order x
setInsert x k = setInsertWith order x k

setFind :: ( x -> y ) y x ( k x ) -> y | setFindWith k & order x
setFind y n x k = setFindWith y n ( order x ) k

setErase :: x ( k x ) -> k x | setEraseWith k & order x
setErase x k = setEraseWith ( order x ) k

setMemberWith :: ( Order2 x ) ( k x ) x -> Bool | setFindWith k
setMemberWith o k x = setFindWith ( \ _ -> True ) False ( o x ) k

setMember :: ( k x ) x -> Bool | setFindWith k & order x
setMember k x = setMemberWith order k x
