// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : setClass
// Purpose : Overloaded dynamic set operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module exports efficient dynamic set operations, insertion, search,
// and deletion.
// These operations are type constructor classes for a unary type constructor k,
// the instances of which are expected to be suitable polymorphic data structures.
// The elements stored in these data structures are expected to be instances of the
// 'order' type class, defining a strict total order.
// --------------------------------------------------------------------------------
// For an increased flexibility, the overloaded operators take the order on the
// elements as an explicit argument. Specialized versions of these operators taking
// the standard overloaded 'order' operator are however provided as a convenience.
// --------------------------------------------------------------------------------
// The order operator that these set operators rely on should define a strict
// total order, unless undefined behaviour may occur.
// --------------------------------------------------------------------------------

definition module setClass

import orderClass

// --------------------------------------------------------------------------------
// The following are the overloaded operators for set insertion, search and
// deletion. These operators explicitly take as an argument the order on the
// elements. Any reasonably efficient instanciation of these operators should run
// in O(log(N)) time, as is the case with AVL trees.
// --------------------------------------------------------------------------------

class setInsertWith k ::              ( Order2 x ) x ( k x ) -> k x
class setFindWith   k :: ( x -> z ) z ( Order1 x )   ( k x ) -> z
class setEraseWith  k ::              ( Order1 x )   ( k x ) -> k x

// --------------------------------------------------------------------------------
// ( setInsertWith o x s ) is the set s' obtained by inserting in the set s the
// element x. In case an element x' such that o x x' == EQ exists in the set s,
// it is replaced by the element x in the set s'. In case no such an element does
// exist, the element x is inserted in the set s.
// --------------------------------------------------------------------------------
// ( setFindWith yes no k s ) is either ( yes x' ), or no, whether the element x'
// such that k x' == EQ is present or not in the set s. The argument k is a unary
// comparator.
// --------------------------------------------------------------------------------
// ( setEraseWith o x z ) is the set s' obtained by removing from the set s the
// element x' such that o x x' == EQ. In case no such an element does exist, the
// set s' is the set s.
// --------------------------------------------------------------------------------
// In all the above,  in case the o operator does not define a strict total order,
// the element x' is ambiguously defined and an unspecified behaviour may occur.
// --------------------------------------------------------------------------------

// --------------------------------------------------------------------------------
// The following are specialization of the above operators, taking as order the
// overloaded order operator for the elements of the set.
// --------------------------------------------------------------------------------

setInsert ::              x ( k x ) -> k x | setInsertWith k & order x
setFind   :: ( x -> y ) y x ( k x ) -> y   | setFindWith   k & order x
setErase  ::              x ( k x ) -> k x | setEraseWith  k & order x

// --------------------------------------------------------------------------------
// The following membership check operators are based on the setFindWith operation.
// --------------------------------------------------------------------------------

setMemberWith :: ( Order2 x ) ( k x ) x -> Bool | setFindWith k
setMember     ::              ( k x ) x -> Bool | setFindWith k & order x

// --------------------------------------------------------------------------------
// ( setMemberWith o s x ) checks for the membership of the element x in the set s,
// based on the order o on the elements.
// See in the setClass.icl how setMemberWith is implemented with setFindWith.
// --------------------------------------------------------------------------------
// ( setMember s x ) checks for the membership of the element x in the set s, based
// on the overloaded order operator for the elements x.
// --------------------------------------------------------------------------------
