// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : orderClass
// Purpose : Overloaded strict total order operator.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module exports the 'order' overloaded operator defining an order on its
// types. Instances of the 'order' type class are provided for the basic type and
// type constructors of Clean.
// Facilities are given to define instances for user-defined data types.
// The instances of the order operator defined for the Clean's primitive data types
// are based on the overloaded (<) operator from the Clean's StdEnv. In these
// cases, the order so defined is a strict total order.
// The operations of the sorted asociative containers map and set expects their
// element's type to provide such an operator defining a strict total order.
// --------------------------------------------------------------------------------

definition module orderClass

from StdEnv import <

import sequenceClass
import StdArray

:: Order        = LT | EQ | GT
:: Order1 x :== ! x     -> Order
:: Order2 x :== ! x ! x -> Order

// --------------------------------------------------------------------------------
// The Order algebraic datatype has the 3 zero-ary constructors LT, EQ and GT that
// stands for the 3 possible cases of the ordering of two elements.
// LT stands for 'Lower Than', or '<'.
// EQ stands for 'EQual', or '=='.
// GT stands for 'Greater Than', or '>'.
// --------------------------------------------------------------------------------
// The ( Order1 x ) type alias stands for the type of a unary comparator function.
// The ( Order2 x ) type alias stands for the type of a binary comparator function.
// These type aliases come in handy for the definition of sorting or searching
// algorithms parameterized with the order of the elements.
// Usually, a ( Order1 x ) unary comparator is obtained from a ( Order2 x ) binary
// comparator by partial application.
// --------------------------------------------------------------------------------

class order x :: Order2 x

// --------------------------------------------------------------------------------
// The order overloaded operator defines a generic binary comparator.
// --------------------------------------------------------------------------------

?             :: ! Order e e e -> e
(:>) infixr 1 :: ! Order Order -> Order

// --------------------------------------------------------------------------------
// In the same manner that 'if' implements a two-way branching expression  based
// on the value of a Bool expression, the '?' implements a three-way branching
// based on the value of a Order expression. '?' comes in handy when expressing
// sorting or searching algorithm where a decision must be taken based on the
// relative order of two elements.
// --------------------------------------------------------------------------------
// The (:>) infix operator comes in handy when defining for an aggregate data type,
// the lexicographical order based on the order of the components of the aggregate.
// See the in orderClass.icl how the lexicographical ordering is defined for 2 and
// 3 tuples using the (:>) operator.
// Notice that (:>) is strict in its first argument only.
// --------------------------------------------------------------------------------

lower   :: ! x ! x -> Bool | order x
equal   :: ! x ! x -> Bool | order x
greater :: ! x ! x -> Bool | order x

// --------------------------------------------------------------------------------
// The lower, equal and greater boolean function provide the usual '<', '==' and
// '>' operations, based on the overloaded order operator.
// --------------------------------------------------------------------------------

fstOrder :: Order2 ( x , y ) | order x
sndOrder :: Order2 ( x , y ) | order y

// --------------------------------------------------------------------------------
// The fstOrder operator defines an order on 2 tuples, based on the order on the
// 1st component of the tuple.
// The sndOrder operator defines an order on 2 tuples, based on the order on the
// 2nd component of the tuple.
// --------------------------------------------------------------------------------
// The fstOrder operator is used in the implementation of maps as search trees.
// See avlTree.icl.
// --------------------------------------------------------------------------------

// --------------------------------------------------------------------------------
// The following are instances declarations of the order operator for Clean's basic
// datatypes.
// These are based on the (<) overloaded operator of the StdEnv.
// --------------------------------------------------------------------------------

instance order Int
instance order Char
instance order Real
instance order Bool

// --------------------------------------------------------------------------------
// The following are instances declarations of the order operator for Clean's basic
// type constructors, lists, arrays, 2 and 3-tuples.
// The order defined on these aggregate type constructors is the lexicographical
// order based on the order for the components of the aggregate.
// --------------------------------------------------------------------------------

instance order [  x ]        | order x
instance order {  x }        | order , uselect_u , usize_u x
instance order {! x }        | order , uselect_u , usize_u x
instance order {# x }        | order , uselect_u , usize_u x
instance order ( x , y )     | order x & order y
instance order ( x , y , z ) | order x & order y & order z

//instance order ( k x ) | forward k & order x

forwardOrder  :: ( k x ) ( k x ) -> Order | forward  k & order x
backwardOrder :: ( k x ) ( k x ) -> Order | backward k & order x
