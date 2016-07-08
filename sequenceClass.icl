// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : sequenceClass
// Purpose : Overloaded sequence operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------

implementation module sequenceClass

class isEmpty  k :: ( k x ) -> Bool
class forward  k :: ( k x ) -> [ x ]
class backward k :: ( k x ) -> [ x ]
