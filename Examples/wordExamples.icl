// --------------------------------------------------------------------------------
// Project : Clean avlTree package.
// Module  : wordExamples
// Purpose : Examples of processing a list of words with the avl tree package.
// Authors : Fabien TODESCATO - FabienT@acm.org
// System  : Clean Compiler 1.3.3 - Clean IDE 2.0
// --------------------------------------------------------------------------------
// This simple example program illustrates the use of maps and sets to compute
// some statistics on a list of words.
// --------------------------------------------------------------------------------

module wordExamples

import avlTree
import wordList
import StdEnum

from StdEnv import map , foldr , foldl , o , id , + , - , ++ , snd , flip

Start = ( length bigWordList // Number of words in raw word list.
        , wordSet // Sorted list of words without duplicates.
        , wordOccurrences // List of pairs of word and occurrence count.
        , wordAnagrams // Groups of anagrams.
        )
where
  bigWordList = ( times 8 twice ) wordList
  where
    times 0 _ = id
    times n f = f o times ( n - 1 ) f
    twice l   = l ++ l

  wordSet
    = ( forward // Flattening the set into a forward sorted list.
      o foldl ( flip setInsert ) treeEmpty // Building a set from the word list.
      ) bigWordList

  wordOccurrences
    = multiset bigWordList

  wordAnagrams
    = ( map snd // Dropping the fst component of the pairs.
      o forward // Flattening the map to a list of pairs.
      o foldr ( mapUpdate cons [ ] ) treeEmpty // Accumulating in lists the words with same letter multiset.
      o map ( pair ( multiset o list ) id ) // Pairing each word with the multiset of its letters.
      ) wordSet
  where
    pair f g x = ( f x , g x )
    cons h t   = [ h : t ]
    list a     = [ x \\ x <-: a ]
  
  multiset :: ( [ x ] -> [ ( x , Int ) ] ) | order x
  multiset = forward o foldl ( flip ( \ x -> mapUpdate (+) 0 ( x , 1 ) ) ) treeEmpty

