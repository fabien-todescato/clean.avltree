// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : asymptotics
// Purpose : Illustration of asymptotic complexity with lists and search trees .
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// Running this example with either the 'tree' or 'list' part commented out
// the time complexities of searching in a balanced tree, vs a list.
// Depending on your machine and project memory setup, you may play with
// different values of 'n' to find the threshold between lists and trees.
// --------------------------------------------------------------------------------

module asymptotics

import avlTree
import StdEnum

from StdEnv import all , flip , foldl

/*// List part.

empty = [ ]

insert x      [       ]  = [ x ]
insert x l =: [ h : t ] = ? ( order x h ) [ x : l ] [ x : l ] [ h : insert x t ]

member _ [       ] = False
member x [ h : t ] = ? ( order x h ) False True ( member x t )
*/

// Tree part.
empty      = treeEmpty
insert x   = setInsert x
member x t = setMember t x


// Main.

Start
  = all ( flip member s ) l
where
  n = 2048
  l = [1..n]
  s = foldl ( flip insert ) empty l
