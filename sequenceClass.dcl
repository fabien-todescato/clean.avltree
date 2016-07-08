// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : sequenceClass
// Purpose : Overloaded sequence operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module exports efficient operations for sequential traversal of a
// container, by means of lazy lists used as unidirectional iterators.
// These operations are type constructor classes for a unary type constructor k,
// the types of which are expected to be suitable polymorphic data structures.
// The data structures are expected to define a natural sequential order on their
// elements, such that the forward operator implements a traversal following the
// natural sequential order, and the backward operator implements a traversal
// following the reverse order.
// --------------------------------------------------------------------------------
// Although in the case of finite data structures a trivial definition of backward
// can be defined as follows :
//   backward = reverse o forward
// It is often possible to give more efficient direct implementation.
// --------------------------------------------------------------------------------

definition module sequenceClass

class isEmpty  k :: ( k x ) -> Bool
class forward  k :: ( k x ) -> [ x ]
class backward k :: ( k x ) -> [ x ]
