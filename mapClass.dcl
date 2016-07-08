// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : mapClass
// Purpose : Overloaded dynamic map operators.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module exports efficient dynamic map operations, insertion of entries,
// search an entry with the given key, and deletion of entries.
// These operations are type constructor classes for a unary type constructor k,
// the instances of which are expected to be suitable polymorphic data structures.
// The elements stored in these data structures are expected to be ( x , y ) pairs
// where x is an instance of the order overloaded operator defining a strict total
// order.
// --------------------------------------------------------------------------------
// For an increased flexibility, the overloaded operators take the order on the
// elements x as an explicit argument. Specialized versions of these operators
// taking the standard overloaded 'order' operator are however provided as a
// convenience.
// --------------------------------------------------------------------------------
// The order operator that these map operators rely on should define a strict
// total order, unless undefined behaviour may occur.
// --------------------------------------------------------------------------------

definition module mapClass

import orderClass

// --------------------------------------------------------------------------------
// The following are the overloaded operators for map insertion, search, deletion,
// value updates and transformations. These operators explicitly take as an
// argument the order on the elements.
// Any reasonably efficient instanciation of these operators should run in
// O(log(N)) time, as is the case with AVL trees.
// --------------------------------------------------------------------------------
// The maMap operator should run in O(N) time.
// --------------------------------------------------------------------------------

class mapInsertWith     k :: ( Order2 x )                ( x , y ) ( k ( x , y ) ) -> k ( x , y )
class mapFindWith       k :: ( Order2 x ) ( ( x , y ) -> z ) z x ( k ( x , y ) ) -> z
class mapFindInsertWith k :: ( Order2 x ) ( ( x , y ) -> z ) ( ( k ( x , y ) ) -> z ) ( x , y ) ( k ( x , y ) ) -> z
class mapEraseWith      k :: ( Order2 x )                  x       ( k ( x , y ) ) -> k ( x , y )
class mapUpdateWith     k :: ( Order2 x ) ( z y -> y ) y ( x , z ) ( k ( x , y ) ) -> k ( x , y )
class mapMap            k ::              (   y -> z )             ( k ( x , y ) ) -> k ( x , z )

// --------------------------------------------------------------------------------
// ( mapInsertWith o ( x , y ) m ) is the map m' obtained from the map m by
// inserting in m the entry ( x , y ) associating the value y to the key x. The
// order o is used to search in the map for the key x.
// In case no entry does exist in the map for the key x, the new ( x , y ) entry is
// inserted in the map.
// In case an entry ( x' , y' ) does already exist in the map such that
// o x x' == EQ, it is replaced by the entry ( x , y ).
// --------------------------------------------------------------------------------
// ( mapfindWith o yes no x m ) is either ( yes ( x' , y ) ) where ( x' , y ) is
// the entry in the map m such that o x x' == EQ, or no if no such an entry exists
// in the map m.
// --------------------------------------------------------------------------------
// ( mapFindInsertWith o found inserted ( x , y ) m ) is either
// ( found ( x' , y' ) ) if an entry ( x' , y' ) exists in the map m such that
// o x x' == EQ, or ( inserted m' ) if no such an entry exists, where m' is the
// map m with the entry ( x , y ) inserted.
// --------------------------------------------------------------------------------
// Although mapFindInsertWith can be implemented as follows using mapFindWith
// and mapInsertWith :
//   mapFindInsert o found inserted xy m
//     = mapFindWith o yes no x m
//   where
//     ( x , y ) = xy
//     no        = mapInsertWith o xy m
//     yes       = found
// It is usually possible to define a more efficient implementation working with
// a single pass traversal of the map's underlying data structure.
// --------------------------------------------------------------------------------
// ( mapEraseWith o x m ) is the map m' where m' is either the map m from which the
// entry ( x' , y ) such that o x x' == EQ has been removed, if such an entry does
// exist, or the map m if such an entry does not exist.
// --------------------------------------------------------------------------------
// ( mapUpdateWith o f y ( x , z ) m ) is the map m' obtained by updating in the
// following manner the map m.
// In case an entry ( x' , y' ) does exist in the map m such that o x x' == EQ, the
// map m' is the map m with the entry ( x' , y' ) replaced with the entry 
// ( x' , f z y' ).
// In case no such entry ( x' , y' ) does exist the map m' is the map m with the
// entry ( x , f z y ) inserted.
// --------------------------------------------------------------------------------
// Although mapUpdateWith can be implemented as follows using mapFindWith and
// mapInsertWith :
//   mapUpdateWith o f y xz m
//     = mapFindWith o updated inserted xz m
//   where
//     ( x , z )         = xz
//     updated ( x , y ) = mapInsertWith o ( x , f y z ) m
//     inserted          = mapInsertWith o ( x , f y z ) m
// It is usually possible to define a more efficient implementation working with
// a single pass traversal of the map's underlying data structure.
// --------------------------------------------------------------------------------
// In all the above,  in case the o operator does not define a strict total order,
// the element x' is ambiguously defined and an unspecified behaviour may occur.
// --------------------------------------------------------------------------------
// ( mapMap f m ) is the map m' obtained by mapping each ( x , y ) entry in m to
// the entry ( x , f y ) in m'.
// --------------------------------------------------------------------------------

// --------------------------------------------------------------------------------
// The following are specialization of the above operators, taking as order the
// overloaded order operator for the keys of the map.
// --------------------------------------------------------------------------------

mapInsert     :: ( ( x , y ) -> ( ( k ( x , y ) ) -> ( k ( x , y ) ) ) ) | mapInsertWith k & order x
mapFind       :: (((x,y) -> z) z x ( k (x,y) ) -> z) | mapFindWith k & order x
mapFindInsert :: (((x,y) -> z) -> (((k (x,y)) -> z) -> ((x,y) -> ((k (x,y)) -> z)))) | mapFindInsertWith k & order x
mapErase      :: ( x ( k ( x , y ) ) -> k ( x , y ) ) | mapEraseWith k & order x
mapUpdate     :: ( ( z y -> y ) y ( x , z ) ( k ( x , y ) ) -> k ( x , y ) ) | mapUpdateWith k & order x

