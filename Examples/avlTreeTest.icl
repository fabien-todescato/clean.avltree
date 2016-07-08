// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : avlTreeTest
// Purpose : Testing avl tree operations.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This module performs a simple insertion and deletion test. As the AVL tree
// rebalancing logic is shared between all AVL tree functions this simple test is
// enough.
// The Merseene Twister random sequence generator - available on the Clean library
// download pages - is required to build the project.
// --------------------------------------------------------------------------------

module avlTreeTest

import mapClass
import setClass
import sequenceClass
import avlTree
import StdEnum

from StdEnv          import take ,&& , foldl , map , * , o , scan , flip , all
from MersenneTwister import genRandReal

Start
  =  True
  && ( all treeCheck o scan ( flip setInsert ) treeEmpty ) l // Insertion test.
  && ( all treeCheck o scan ( flip setErase  ) treeFull  ) l // Deletion test.
where
  s        = 1
  n        = 2 * toInt m
  m        = 1024.0
  l        = ( take n o map ( toInt o ( (*) m ) ) o genRandReal ) s
  treeFull = foldl ( flip setInsert ) treeEmpty l
