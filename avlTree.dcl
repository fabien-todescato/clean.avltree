// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : avlTree
// Purpose : AVL Tree data structure.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module exports the Tree abstract type constructor implementing the AVL tree
// abstract data type.
// --------------------------------------------------------------------------------
// An instance of the order overloaded operator is provided for ( Tree x ), defined
// as the lexicographical order of the elements the tree listed by an infix
// traversal of the tree, and provided that the order operator be overloaded for
// the elements x in the ( Tree x ).
// --------------------------------------------------------------------------------
// Instances of the map and set overloaded operations are provided so that a
// ( Tree x ) can be manipulated as a set of x elements, and a ( Tree ( x , y ) )
// can be manipulated as a map from x to y elements.
// --------------------------------------------------------------------------------
// Instances of the sequence operators are provided implementing forward and
// backward sequential traversal of a Tree.
// --------------------------------------------------------------------------------

definition module avlTree

import    orderClass
import sequenceClass
import      setClass
import      mapClass

:: Tree a

// --------------------------------------------------------------------------------
// Operator instances for the orderClass module.
// --------------------------------------------------------------------------------

instance order ( Tree x ) | order x

// --------------------------------------------------------------------------------
// Operator instances for the sequenceClass module.
// --------------------------------------------------------------------------------

instance isEmpty  Tree
instance forward  Tree
instance backward Tree

// --------------------------------------------------------------------------------
// Operator instances for the setClass module.
// --------------------------------------------------------------------------------

instance setInsertWith Tree
instance setEraseWith  Tree
instance setFindWith   Tree

// --------------------------------------------------------------------------------
// Operator instances for the mapClass module.
// --------------------------------------------------------------------------------

instance mapInsertWith Tree
instance mapFindWith   Tree
instance mapEraseWith  Tree
instance mapUpdateWith Tree
instance mapMap        Tree

// --------------------------------------------------------------------------------
// Basic ( Tree x ) operations.
// --------------------------------------------------------------------------------

treeEmpty     ::     Tree a
treeIsEmpty   ::              ! ( Tree a ) -> .Bool
treeCheck     ::              ! ( Tree x ) -> Bool | order x
treeCheckWith :: ( Order2 x ) ! ( Tree x ) -> Bool

// --------------------------------------------------------------------------------
// AVL tree functions.
// --------------------------------------------------------------------------------
// These functions are usually not used directly. The instances of the map and set
// overloaded operations should be used instead.
// See the implementation module for documentation about these functions.
// --------------------------------------------------------------------------------

treeInsert       ::              ( Order2 a ) a ! ( Tree a ) -> Tree a
treeFind         :: ( a -> b ) b ( Order1 a )   ! ( Tree a ) -> b
treeFindInsert   :: ( Order2 a ) ( a -> b ) ( ( Tree a ) ->  b ) a ! ( Tree a ) -> b
treeErase        :: ( Order1 a ) !( Tree a ) -> Tree a
treeUnsafeUpdate :: ( Order1 a ) ( a -> a ) a ! ( Tree a ) -> Tree a
treeUnsafeMap    :: ( a -> b ) ! ( Tree a ) -> Tree b

// --------------------------------------------------------------------------------
// Implementations of the forward and backward iterators.
// --------------------------------------------------------------------------------

treeForward  :: ! ( Tree . a ) -> [ .a ]
treeBackward :: ! ( Tree . a ) -> [ .a ]
