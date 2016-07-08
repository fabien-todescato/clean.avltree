// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : avlTree
// Purpose : AVL Tree data structure.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------

implementation module avlTree

from StdEnv import max , < , + , - , == , && , fst , o , id

import    orderClass
import sequenceClass
import      setClass
import      mapClass

// --------------------------------------------------------------------------------
// Operator instances for the orderClass module.
// --------------------------------------------------------------------------------

instance
  order ( Tree x ) | order x
where
  order = \ t1 t2 -> order ( treeForward t1 ) ( treeForward t2 )

// --------------------------------------------------------------------------------
// Operator instances for the sequenceClass module.
// --------------------------------------------------------------------------------

instance isEmpty  Tree where isEmpty  t = treeIsEmpty  t
instance forward  Tree where forward  t = treeForward  t
instance backward Tree where backward t = treeBackward t

// --------------------------------------------------------------------------------
// Operator instances for the setClass module.
// --------------------------------------------------------------------------------
// A ( set x ) is represented as a ( Tree x ), that is, an AVL tree whose nodes
// hold x elements.
// The AVL tree representing the set is such that an infix traversal of the tree
// yields a strictly sorted sequence of x elements.
// The set operations are then trivially deduced from the underlying AVL tree
// operations.
// --------------------------------------------------------------------------------

instance setInsertWith Tree where setInsertWith     o x k = treeInsert     o x k
instance setEraseWith  Tree where setEraseWith      o   k = treeErase      o   k
instance setFindWith   Tree where setFindWith   y n o   k = treeFind   y n o   k

// --------------------------------------------------------------------------------
// Operator instances for the mapClass module.
// --------------------------------------------------------------------------------
// A ( map ( x , y ) ) mapping x elements to y elements is represented as a
// ( Tree ( x , y ) ), that is, an AVL tree whose nodes hold ( x , y ) pairs.
// The AVL tree representing the map is such that an infix traversal of the tree
// yields a sequence of ( x , y ) pairs whose first components are strictly sorted.
// The map operations are trivially deduced from the underlying AVL tree
// by using the order on the first components of the pairs.
// --------------------------------------------------------------------------------

instance
  mapInsertWith Tree
where
  mapInsertWith o xy m
  = treeInsert ( \ ( x1 , _ ) ( x2 , _ ) -> o x1 x2 ) xy m

instance
  mapFindWith Tree
where
  mapFindWith o yes no x m
  = treeFind yes no ( \ ( x1 , _ ) -> o x x1 ) m

instance
  mapFindInsertWith Tree
where
  mapFindInsertWith o found inserted xy m
  = treeFindInsert ( \ ( x1 , _ ) ( x2 , _ ) -> o x1 x2 ) found inserted xy m

instance
  mapEraseWith Tree
where
  mapEraseWith o x m
  = treeErase ( \ ( x1 , _ ) -> o x x1 ) m

instance
  mapUpdateWith Tree
where
  mapUpdateWith o f y ( x , z ) k
  = treeUnsafeUpdate ( \ ( k , _ ) -> o x k ) ( \ ( x , y ) -> ( x , f z y ) ) ( x , f z y ) k

instance
  mapMap Tree
where
  mapMap f t
  = treeUnsafeMap ( \ ( x , y ) -> ( x , f y ) ) t

// --------------------------------------------------------------------------------
// ( Tree a ) algebraic data type and basic access functions.
// --------------------------------------------------------------------------------
// AVL trees with elements x stored at the nodes are represented by the algebraic
// datatype ( Tree x ).
// The empty tree is represented with a nullary constructor, while internal tree
// nodes are represented by several constructors, each of which encode the local
// imbalance of the tree.
// --------------------------------------------------------------------------------
// In the following, let ( height t ) be the height of the tree, defined as
// follows :
//   height ( T_       ) = 0
//   height ( L_ l _ r ) = 1 + max ( height l ) ( height r )
//   height ( B_ l _ r ) = 1 + max ( height l ) ( height r )
//   height ( R_ l _ r ) = 1 + max ( height l ) ( height r )
// And let ( size t ) be the size of the tree, defined as follows :
//   size ( T_       ) = 0
//   size ( L_ l _ r ) = size l + size r
//   size ( B_ l _ r ) = size l + size r
//   size ( R_ l _ r ) = size l + size r
// --------------------------------------------------------------------------------
// Any internal node in an AVL tree satisfies the following balance invariant :
// A left-tipping node ( L_ l k r ) is such that height l     == height r + 1.
// A balanced     node ( B_ l k r ) is such that height l     == height r.
// A left-tipping node ( L_ l k r ) is such that height l + 1 == height r.
// --------------------------------------------------------------------------------
// From the above it can be shown that ( height t ) ~ log ( size t ).
// --------------------------------------------------------------------------------

:: Tree a
= T_                           // Empty tree, leaf.
| L_ ( Tree a ) ! a ( Tree a ) // Left-tipping  internal node.
| B_ ( Tree a ) ! a ( Tree a ) // Balanced      internal node.
| R_ ( Tree a ) ! a ( Tree a ) // Right-tipping internal node.

left :: !u:(Tree .a) -> v:Tree .a, [u <= v]
left ( L_ l _ _ ) = l
left ( B_ l _ _ ) = l
left ( R_ l _ _ ) = l

key :: !(Tree .a) -> .a
key ( L_ _ k _ ) = k
key ( B_ _ k _ ) = k
key ( R_ _ k _ ) = k

right :: !u:(Tree .a) -> v:Tree .a, [u <= v]
right ( L_ _ _ r ) = r
right ( B_ _ _ r ) = r
right ( R_ _ _ r ) = r

treeEmpty :: Tree a
treeEmpty = T_

treeIsEmpty :: !(Tree a) -> .Bool
treeIsEmpty ( T_ ) = True
treeIsEmpty _      = False

// --------------------------------------------------------------------------------
// The following functions check for the balance invariant and infix order
// invariant of the nodes of the tree.
// --------------------------------------------------------------------------------

treeCheck :: ! (Tree x) -> Bool | order x
treeCheck t = treeCheckWith order t

treeCheckWith :: ( Order2 x ) ! ( Tree x ) -> Bool
treeCheckWith o t = checkInfixOrderWith o t && checkBalance t

checkInfixOrderWith :: ( Order2 x ) ! ( Tree x ) -> Bool
checkInfixOrderWith o t = check t ( \ _ -> True )
where
  check ( T_       ) _        = True
  check ( L_ l x r ) checkKey = checkNode l x r checkKey
  check ( B_ l x r ) checkKey = checkNode l x r checkKey
  check ( R_ l x r ) checkKey = checkNode l x r checkKey
  
  checkNode l x r checkKey
  = ( checkKey x                                                ) &&
    ( check l ( \ k -> ? ( o k x ) ( checkKey k ) False False ) ) &&
    ( check r ( \ k -> ? ( o x k ) ( checkKey k ) False False ) )

checkBalance :: !(Tree .a) -> .Bool;
checkBalance t = check t ( \ _ -> True )
where
  check ( T_        ) checked = checked 0
  check ( L_ l  _ r ) checked = check l \ hl -> check r \ hr -> ( hl - hr == (+1) ) && checked ( hl + 1 )
  check ( B_ l  _ r ) checked = check l \ hl -> check r \ hr -> ( hl - hr ==   0  ) && checked ( hl + 1 )
  check ( R_ l  _ r ) checked = check l \ hl -> check r \ hr -> ( hl - hr == (-1) ) && checked ( hr + 1 )

// --------------------------------------------------------------------------------
// The following are the implementations of the forward and backward iterators
// on AVL trees.
// --------------------------------------------------------------------------------

treeForward :: ! ( Tree . a ) -> [ .a ]
treeForward t = forward t [ ]
where
  forward ( T_       ) m = m
  forward ( L_ l k r ) m = forward l [ k : forward r m ]
  forward ( B_ l k r ) m = forward l [ k : forward r m ]
  forward ( R_ l k r ) m = forward l [ k : forward r m ]

treeBackward :: ! ( Tree . a ) -> [ .a ]
treeBackward t = backward t [ ]
where
  backward ( T_       ) m = m
  backward ( L_ l k r ) m = backward r [ k : backward l m ]
  backward ( B_ l k r ) m = backward r [ k : backward l m ]
  backward ( R_ l k r ) m = backward r [ k : backward l m ]

// --------------------------------------------------------------------------------
// AVL Tree rebalancing functions.
// --------------------------------------------------------------------------------
// The following are elementary AVL tree rebalancing functions. These functions do
// a case analysis on an AVL sub-tree s', and take as additional arguments a key k
// and a sibling sub-tree t.
// --------------------------------------------------------------------------------
// The AVL sub-tree s' is supposed to originate from an AVL sub-tree s by an
// operation of insertion or deletion. It is supposed by induction hypothesis that
// the height of s and s' differ by at most 1. Furthermore, s, k and t are supposed
// to be part of a larger AVL tree T where an insertion or deletion operation is to
// take place in the sub-tree s, which is then replaced by the modified sub-tree s'.
// --------------------------------------------------------------------------------
// An elementary rebalancing function rebuilds from the modified sub-tree s', the
// key and the sibling sub-tree t a modified larger AVL tree T' where the insertion
// or deletion operation has taken place.
// --------------------------------------------------------------------------------
// For each of the combination of cases below :
// - of s being either the left or right son,
// - of either a left-tipping, balanced, or right-tipping node,
// - with the height of s and s' differing by either +1, 0 or -1,
// There is defined an elementary rebalancing function.
// --------------------------------------------------------------------------------
// Each rebalancing function performs a single rebalancing step where the height
// of the tree to be rebalanced is decreased. This generates a new modified sub-tree
// s'' one level up toward the root of T'. It can be shown that the height of that
// new sub-tree differs by at most 1 with the height of the analogue old sub-tree.
// The same  rebalancing process can be applied iteratively up to the root of T'.
// Each rebalancing function takes three additional arguments ih uh and dh that are
// the continuation of the rebalancing process in the three new possible cases
// where the height of the new and old subtrees differ by +1, 0, and -1
// respectively.
// --------------------------------------------------------------------------------

// Increased Height of Left son of Left-tipping node.
ihll ih uh dh l z ( L_ x k y            ) = uh ( B_ x k ( B_ y l z )            ) // +LLL
ihll ih uh dh l z ( B_ x k y            ) = ih ( R_ x k ( L_ y l z )            ) // +LLB
ihll ih uh dh m z ( R_ w k ( L_ x l y ) ) = uh ( B_ ( B_ w k x ) l ( R_ y m z ) ) // +LLRL
ihll ih uh dh m z ( R_ w k ( B_ x l y ) ) = uh ( B_ ( B_ w k x ) l ( B_ y m z ) ) // +LLRB
ihll ih uh dh m z ( R_ w k ( R_ x l y ) ) = uh ( B_ ( L_ w k x ) l ( B_ y m z ) ) // +LLRR

// Increased Height of Left son of Balanced node.
ihlb ih uh dh k y x = ih ( L_ x k y )

// Increased Height of Left son of Right-tipping node.
ihlr ih uh dh k y x = uh ( B_ x k y )

// Increased Height of Right son of Right-tipping node.
ihrr ih uh dh l z ( R_ y k x            ) = uh ( B_ ( B_ z l y ) k x            ) // +RRR
ihrr ih uh dh l z ( B_ y k x            ) = ih ( L_ ( R_ z l y ) k x            ) // +RRB
ihrr ih uh dh m z ( L_ ( R_ y l x ) k w ) = uh ( B_ ( L_ z m y ) l ( B_ x k w ) ) // +RRLR
ihrr ih uh dh m z ( L_ ( B_ y l x ) k w ) = uh ( B_ ( B_ z m y ) l ( B_ x k w ) ) // +RRLB
ihrr ih uh dh m z ( L_ ( L_ y l x ) k w ) = uh ( B_ ( B_ z m y ) l ( R_ x k w ) ) // +RRLL

// Increased Height of Right son of Balanced node.
ihrb ih uh dh k y x = ih ( R_ y k x ) // +RB

// Increased Height of Right son of Left-tipping node.
ihrl ih uh dh k y x = uh ( B_ y k x ) // +LB

// Unchanged Height cases are trivial.
uhll ih uh dh k y x = uh ( L_ x k y )
uhrl ih uh dh k y x = uh ( L_ y k x )
uhlb ih uh dh k y x = uh ( B_ x k y )
uhrb ih uh dh k y x = uh ( B_ y k x )
uhlr ih uh dh k y x = uh ( R_ x k y )
uhrr ih uh dh k y x = uh ( R_ y k x )

// Decreased Height of Right son of Left-tipping node.
dhrl ih uh dh m ( R_ w k ( R_ x l y ) ) z = dh ( B_ ( L_ w k x ) l ( B_ y m z ) ) // -RLRR
dhrl ih uh dh m ( R_ w k ( B_ x l y ) ) z = dh ( B_ ( B_ w k x ) l ( B_ y m z ) ) // -RLRB
dhrl ih uh dh m ( R_ w k ( L_ x l y ) ) z = dh ( B_ ( B_ w k x ) l ( R_ y m z ) ) // -RLRL
dhrl ih uh dh l ( L_ x k y            ) z = dh ( B_ x            k ( B_ y l z ) ) // -RLL
dhrl ih uh dh l ( B_ x k y            ) z = uh ( R_ x            k ( L_ y l z ) ) // -RLB

// Decreased Height of Left son of Left-tipping node.
dhll ih uh dh k y x = dh ( B_ x k y )

// Decreased Height of Left son of Right-tipping node.
dhlr ih uh dh m ( L_ ( L_ y l x ) k w ) z = dh ( B_ ( B_ z m y ) l ( R_ x k w ) ) // -LRLL
dhlr ih uh dh m ( L_ ( B_ y l x ) k w ) z = dh ( B_ ( B_ z m y ) l ( B_ x k w ) ) // -LRLB
dhlr ih uh dh m ( L_ ( R_ y l x ) k w ) z = dh ( B_ ( L_ z m y ) l ( B_ x k w ) ) // -LRLR
dhlr ih uh dh l ( R_ y            k x ) z = dh ( B_ ( B_ z l y ) k x            ) // -LRR
dhlr ih uh dh l ( B_ y            k x ) z = uh ( L_ ( R_ z l y ) k x            ) // -LRB

// Decreased Height of Right son of Right-tipping node.
dhrr ih uh dh k y x = dh ( B_ y k x )

// Decreased Height of sons of Balanced node.
dhlb ih uh dh k y x = uh ( R_ x k y ) // Left  son.
dhrb ih uh dh k y x = uh ( L_ y k x ) // Right son.

// --------------------------------------------------------------------------------
// Representation of AVL tree upward paths as higher-order rebalancing functions.
// --------------------------------------------------------------------------------
// A path in an AVL tree T leading from a sub-tree s up to the root of T is
// represented as an higher-order function p that transforms a 3-tuple of
// rebalancing functions ih uh dh applied to the sub-tree s into a corresponding
// 3-tuple of rebalancing function on the tree T.
// Instead of explicitly returning a 3-tuple, a continuation applied to the
// returned 3-tuple is taken instead as the argument f.
// --------------------------------------------------------------------------------
// An upward path in an AVL tree T is constructed by following downward from the
// root of T a path to a sub-tree. At each traversal of an internal node, whether
// the node is either left-tipping, balanced, or right-tipping, and the branch taken
// is either the left or right branch, a new specific node is accumulated in the
// downward path being constructed, in a manner reminiscent of a stack.
// The stacking of a node on the current upward path being constructed is
// implemented as function composition.
// --------------------------------------------------------------------------------

p__     f ih uh dh = f ih uh dh              // Empty path.
pll k y f ih uh dh = f ( ihll ih uh dh k y ) // Left son of Left-tipping node.
                       ( uhll ih uh dh k y )
                       ( dhll ih uh dh k y )
prl k x f ih uh dh = f ( ihrl ih uh dh k x ) // Right son of Right-tipping node.
                       ( uhrl ih uh dh k x )
                       ( dhrl ih uh dh k x )
plb k y f ih uh dh = f ( ihlb ih uh dh k y ) // Left son of Balanced node.
                       ( uhlb ih uh dh k y )
                       ( dhlb ih uh dh k y )
prb k x f ih uh dh = f ( ihrb ih uh dh k x ) // Right son of Balanced node.
                       ( uhrb ih uh dh k x )
                       ( dhrb ih uh dh k x )
plr k y f ih uh dh = f ( ihlr ih uh dh k y ) // Left son of Right-tipping node.
                       ( uhlr ih uh dh k y )
                       ( dhlr ih uh dh k y )
prr k x f ih uh dh = f ( ihrr ih uh dh k x ) // Right son of Right-tipping node.
                       ( uhrr ih uh dh k x )
                       ( dhrr ih uh dh k x )

// --------------------------------------------------------------------------------
// The various functions 'findX' below are the basic searching function on which
// most of the AVL tree function are based. These function 'findX' searches for an
// element in the the tree and returns the path upward from the sub-tree whose root
// node holds the looked for element if such a node does exist, or the path from
// the leaf where such an element should be inserted.
// --------------------------------------------------------------------------------
// The expressions ( findX t ) have complexity ~ height t ~log ( size t ).
// --------------------------------------------------------------------------------

// --------------------------------------------------------------------------------
// ( find f c t ) applies the continuation f to the path p upward from the node
// whose element x is such that c x == EQ, if such an element exists, or to the
// leaf where the element x would be placed in the tree.
// --------------------------------------------------------------------------------
// The higher-order functions pll, prl, ..., are used as constructors for the nodes
// of the path being constructed, and the 'o' function composition operator is
// used to stack up these nodes. The current path being built is passed in the
// accumulator argument p, with the initial value p__ for the empty path.
// --------------------------------------------------------------------------------

find f c t = findfc t p__
where
  findfc t =: ( T_       ) p = f t p
  findfc t =: ( L_ x k y ) p = ? ( c k ) ( findfc x ( p o pll k y ) ) ( f t p ) ( findfc y ( p o prl k x ) )
  findfc t =: ( B_ x k y ) p = ? ( c k ) ( findfc x ( p o plb k y ) ) ( f t p ) ( findfc y ( p o prb k x ) )
  findfc t =: ( R_ x k y ) p = ? ( c k ) ( findfc x ( p o plr k y ) ) ( f t p ) ( findfc y ( p o prr k x ) )

// --------------------------------------------------------------------------------
// ( findMin f t ) applies the continuation f to the path p upward from the node
// whose element x is the minimum in the tree t. Because of the infix order
// invariant, it is not necessary to perform comparisons.
// --------------------------------------------------------------------------------
// The same technique as above with the accumulator argument is used.
// --------------------------------------------------------------------------------

findMin f t = findf t p__
where
  findf t =: ( L_ l k r ) p = findL l
  where
    findL ( T_ ) = f t p
    findL ( l  ) = findf l ( p o pll k r )
  findf t =: ( B_ l k r ) p = findB l
  where
    findB ( T_ ) = f t p
    findB ( l  ) = findf l ( p o plb k r )
  findf t =: ( R_ l k r ) p = findR l
  where
    findR ( T_ ) = f t p
    findR ( l  ) = findf l ( p o plr k r )

// --------------------------------------------------------------------------------
// AVL tree functions.
// --------------------------------------------------------------------------------
// The various functions below implement search, insertion and deletion on an AVL
// tree. These function follow a similar implementation. The path upward from the
// sub-tree to be modified by the operation is computed by the function 'find' seen
// above, then the sub-tree is locally modified, and the path upward is used to
// rebalance a new complete tree from the locally modified sub-tree.
// --------------------------------------------------------------------------------

// --------------------------------------------------------------------------------
// ( treInsert o x t ) is the tree obtained by inserting the element x in the tree
// t, using the order o to search for the node where x is to be put.
// --------------------------------------------------------------------------------
// The functional p for the path upward from the sub-tree whose root is the looked
// for node is computed using the function 'find'.
// Depending upon the found node being a leaf or an internal node, the new sub-tree
// is given back to the rebalancing function built from the path and the selection
// of the ih, uh or dh rebalancing function to use for the respective cases where
// the height of the new sub-tree is increased by 1, unchanged or decreased by 1.
// --------------------------------------------------------------------------------

treeInsert :: ( Order2 a ) a ! ( Tree a ) -> Tree a
treeInsert order x t = find insert ( order x ) t id id id
where
  insert ( T_       ) p = p \ ih uh dh -> ih ( B_ T_ x T_ )
  insert ( L_ l _ r ) p = p \ ih uh dh -> uh ( L_ l  x r  )
  insert ( B_ l _ r ) p = p \ ih uh dh -> uh ( B_ l  x r  )
  insert ( R_ l _ r ) p = p \ ih uh dh -> uh ( R_ l  x r  )

// --------------------------------------------------------------------------------
// ( treeFindInsert o f i x t ) is either ( f x' ) if there exists in t an element
// x' such that o x x' == EQ, or ( i t' ) where t' is the tree obtained by
// inserting x in the tree t.
// --------------------------------------------------------------------------------
// The implementation is similar to the one of treeInsert. Note that only in the
// case where the element x is not found is built a new tree.
// --------------------------------------------------------------------------------

treeFindInsert :: ( Order2 a ) ( a -> b ) ( ( Tree a ) ->  b ) a ! ( Tree a ) -> b
treeFindInsert order f i x t = find findInsert ( order x ) t
where
  findInsert ( T_       ) p = i ( p ( \ ih uh dh -> ih ( B_ T_ x T_ ) ) id id id )
  findInsert ( L_ _ k _ ) _ = f k
  findInsert ( B_ _ k _ ) _ = f k
  findInsert ( R_ _ k _ ) _ = f k

// --------------------------------------------------------------------------------
// ( treeFind yes no o ) is either ( yes x ) if there exists in t an element
// x such that o x == EQ, or no if there does not exist such an element.
// --------------------------------------------------------------------------------
// As there is no need to rebuild a new tree, the implementation below doesn't
// reuse the 'find' function, and directly does the downward traversal of the tree
// to reach the looked for node.
// --------------------------------------------------------------------------------

treeFind :: ( a -> b ) b ( Order1 a ) ! ( Tree a ) -> b
treeFind y n c t = findync t
where
  findync ( T_       ) = n
  findync ( L_ l k r ) = ? ( c k ) ( findync l ) ( y k ) ( findync r )
  findync ( B_ l k r ) = ? ( c k ) ( findync l ) ( y k ) ( findync r )
  findync ( R_ l k r ) = ? ( c k ) ( findync l ) ( y k ) ( findync r )

// --------------------------------------------------------------------------------
// ( treeErase o t ) is the tree t from which the element x such that o x == EQ has
// been removed. If no such an element does exist, it is the same tree t.
// --------------------------------------------------------------------------------
// A case analysis is performed on the node that holds the element to be erased.
// In case the node is the leaf, the element to be erased is not in the tree at
// at all, so it is enough to directly return the same tree.
// In case the node has a single non-empty child, it is enough to splice out the
// node and rebalance the tree, with the height of the new sub-tree being decreased
// by 1.
// In case the node has two non-empty children, we splice out the node holding the
// min value in the right sub-tree, and replace the element to be deleted with the
// min element so found.
// --------------------------------------------------------------------------------

from StdEnv import abort

treeErase :: ( Order1 a ) ! ( Tree a ) -> Tree a
treeErase c t = find erase c t id id id
where
  erase ( T_         ) p = p \ ih uh dh ->    t // Element not in tree.
//erase ( L_ T_ _ r  ) p = p \ ih uh dh -> dh r // Node with a single child.
  erase ( L_ l  _ T_ ) p = p \ ih uh dh -> dh l // Same as above.
  erase ( B_ T_ _ r  ) p = p \ ih uh dh -> dh r // Same as above.
//erase ( B_ l  _ T_ ) p = p \ ih uh dh -> dh l // Same as above.
  erase ( R_ T_ _ r  ) p = p \ ih uh dh -> dh r // Same as above.
//erase ( R_ l  _ T_ ) p = p \ ih uh dh -> dh l // Same as above.

  erase ( L_ l _ r ) p // Node with two non-empty children.
  = findMin ( \ m q -> ( p o prl ( key m ) l o q ) \ ih uh dh -> dh ( right m ) ) r
  erase ( B_ l _ r ) p // Same as above.
  = findMin ( \ m q -> ( p o prb ( key m ) l o q ) \ ih uh dh -> dh ( right m ) ) r
  erase ( R_ l _ r ) p // Same as above.
  = findMin ( \ m q -> ( p o prr ( key m ) l o q ) \ ih uh dh -> dh ( right m ) ) r

// --------------------------------------------------------------------------------
// ( treeUnsafeUpdate o f x t ) is the tree t' obtained from the tree t by applying
// to the element x' such that o x == EQ the function f. That is, the tree t' is the
// same as the tree t, except that the element x' has been updated to ( f x' ).
// In case no such an element x' does exist in the tree, the element x is inserted
// in the tree.
// --------------------------------------------------------------------------------
// The treeUnsafeUpdate function serves as a basis for the implementation of the
// mapUpdate function that is useful to accumulate data associated to the keys in
// the map.
// This function, however, should be used with care for it may violate the infix
// order invariant at the nodes of the tree.
// --------------------------------------------------------------------------------

treeUnsafeUpdate :: ( Order1 a ) ( a -> a ) a ! ( Tree a ) -> Tree a
treeUnsafeUpdate c f x t = find update c t id id id
where
  update ( T_       ) p = p \ ih uh dh -> ih ( B_ T_ ( x   ) T_ )
  update ( L_ l k r ) p = p \ ih uh dh -> uh ( L_ l  ( f k ) r  )
  update ( B_ l k r ) p = p \ ih uh dh -> uh ( B_ l  ( f k ) r  )
  update ( R_ l k r ) p = p \ ih uh dh -> uh ( R_ l  ( f k ) r  )

// --------------------------------------------------------------------------------
// ( treeUnsafeMap f t ) is the tree t' obtained from the tree t by replacing at
// each node the key k by the key ( f k ). It is the analogue of map for lists.
// --------------------------------------------------------------------------------
// The treeUnsafeMap function serves as a basis for the implementation of the
// mapMap function that is useful to transform data associated to the keys in
// the map.
// This function, however, should be used with care for it may violate the infix
// order invariant at the nodes of the tree.
// --------------------------------------------------------------------------------

treeUnsafeMap :: (a -> b) !(Tree a) -> Tree b
treeUnsafeMap f t = mapf t
where
  mapf ( T_       ) = ( T_                               )
  mapf ( L_ l k r ) = ( L_ ( mapf l ) ( f k ) ( mapf r ) )
  mapf ( B_ l k r ) = ( L_ ( mapf l ) ( f k ) ( mapf r ) )
  mapf ( R_ l k r ) = ( L_ ( mapf l ) ( f k ) ( mapf r ) )
	  