namespace BTree
    module BTree =
        //Nodes are either the first node, which only has a "less than" branch, or subsequent nodes which have a "less than"
        //and greater than branch
        type internal StructureNode<'a,'b> = Single of  'a*TreeNode<'a,'b> | Double of 'a*TreeNode<'a,'b>*TreeNode<'a,'b>
        and internal TreeNode<'a,'b> = Structure of StructureNode<'a,'b> list| Leaf of ('a*'b) list
        


        //Public interface for BTrees
        type public BTree<'a,'b when 'a : comparison> = int*TreeNode<'a,'b>
        val public Insert : BTree<'a,'b>->'a->'b-> BTree<'a,'b>
        val public Remove: BTree<'a,'b> -> 'a -> BTree<'a,'b>
        val public Find: BTree<'a,'b> -> 'a -> 'b option
        val public Create<'a,'b when 'a :comparison> : int-> BTree<'a,'b>
